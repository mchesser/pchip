use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};

use ast;

use dlx::asm;
use dlx::asm::{RegId, LabelId};
use dlx::asm::Instruction;
use dlx::types;
use dlx::types::{Type, TypeTable};

use error::{InputSpan, Logger};

use self::IdentId::*;
use self::Location::*;
use self::Ident::*;

const UNIT_TYPE: Type = types::Normal(0);
const INT_TYPE: Type = types::Normal(1);
const CHAR_TYPE: Type = types::Normal(2);
const BOOL_TYPE: Type = types::Normal(3);

// Special register that is always 0
const ZERO_REG: RegId = 0;
// Frame pointer register
const FRAME_POINTER: RegId = 30;
// Stack pointer register
const STACK_POINTER: RegId = 14;
// Heap pointer register
const HEAP_POINTER: RegId = 15;
// Return address register (set by jal)
const RETURN_REG: RegId = 31;
// Register used for storing the results of computations
const RESULT_REG: RegId = 1;
// Register used for temporary values
const TEMP_REG: RegId = 2;
// Register used for storing addresses
const ADDR_REG: RegId = 3;
// Register use for copying values
const COPY_REG: RegId = 4;

const DATA_SEGMENT: &'static str = "        .seg    data";
const CONST_DATA_SEGMENT: &'static str = "        .seg    constdata";
const CODE_SEGMENT: &'static str = "        .seg    code";

const PROGRAM_START: &'static str =
"
; Allocate some dynamic memory for the program to use
        .seg    data
stack   .space  800
heap    .space  800

; Manually start the program
        .seg    code
        .start  prgsrt

prgsrt  addui   r14,r0,stack            ; Give the program a stack
        addui   r15,r0,heap             ; Give the program a heap
        jal     main                    ; Jump to the program entry point
        halt                            ; Stop the machine
";


pub struct Function {
    ast: ast::FunctionDeclaration,
    arg_types: Vec<Type>,
    rtype: Type,
    location: LabelId,
}

impl Function {
    fn new(ast: ast::FunctionDeclaration, type_table: &TypeTable, scope: &Scope,
        location: LabelId) -> Function
    {
        let arg_types = ast.params.iter().map(|p| type_table.resolve_type(scope, &p.1))
            .collect();
        let rtype = type_table.resolve_type(scope, &ast.rtype);
        Function {
            ast: ast,
            arg_types: arg_types,
            rtype: rtype,
            location: location,
        }
    }
}

#[derive(Clone, Debug)]
enum Location {
    Label(LabelId),
    Offset(i16),
    Register(RegId),
}

pub struct Variable {
    ast: ast::LetStatement,
    rtype: Type,
    is_const: bool,
    location: Location,
}

impl Variable {
    fn new(ast: ast::LetStatement, rtype: Type, location: Location, is_const: bool) -> Variable {
        Variable {
            ast: ast,
            rtype: rtype,
            is_const: is_const,
            location: location,
        }
    }
}

#[derive(PartialEq, Hash)]
pub enum IdentId {
    FnIdentId(usize),
    VarIdentId(usize),
}

pub enum Ident<'a> {
    FnIdent(&'a Function),
    VarIdent(&'a Variable),
}

impl<'a> Ident<'a> {
    pub fn rtype(&self) -> Type {
        match *self {
            FnIdent(func) => func.rtype.clone(),
            VarIdent(var) => var.rtype.clone(),
        }
    }

    fn unwrap_var(self) -> &'a Variable {
        match self {
            FnIdent(..) => panic!("ICE attempted to unwrap function when attempting to get var"),
            VarIdent(var) => var,
        }
    }
}

pub struct Scope<'a> {
    functions: Vec<Function>,
    vars: Vec<Variable>,
    next_offset: i16,
    ident_table: HashMap<String, IdentId>,
    loop_ends: Vec<LabelId>,
    end_label: LabelId,
    parent: Option<&'a Scope<'a>>
}

impl<'a> Scope<'a> {
    pub fn new(end_label: LabelId) -> Scope<'a> {
        Scope {
            functions: vec![],
            vars: vec![],
            // Note: first avalible offset is 8, (the first 8 bytes store the frame pointer of prev
            // stack frame, and return location).
            next_offset: 8,
            ident_table: HashMap::new(),
            loop_ends: vec![],
            end_label: end_label,
            parent: None,
        }
    }

    fn new_with_parent(parent: &'a Scope<'a>, end_label: LabelId) -> Scope<'a> {
        let mut scope = Scope::new(end_label);
        scope.parent = Some(parent);
        scope
    }

    /// Add an identifier to the scope
    fn add_ident(&mut self, ident_name: String, ident: IdentId, _span: InputSpan) {
        match self.ident_table.entry(ident_name) {
            Vacant(entry) => { entry.insert(ident); },
            // This identifier shadows an existing one. Variable shadowing is not supported.
            Occupied(..) => panic!("IDENT_SHADOW_ERROR, TODO: improve error message"),
        }
    }

    /// Get the identifier corresponding to an identifier name.
    pub fn get_ident(&self, ident_name: &String, span: InputSpan) -> Ident {
        match self.ident_table.get(ident_name) {
            Some(&FnIdentId(id)) => FnIdent(&self.functions[id]),
            Some(&VarIdentId(id)) => VarIdent(&self.vars[id]),
            None => {
                // If the identifier was not found in this scope, check the parent scope.
                match self.parent {
                    Some(parent) => parent.get_ident(ident_name, span),
                    None => {
                        // Reached the top level scope, but still could not find the identifier
                        // therefore it doesn't not exist at this location.
                        panic!("IDENT_NOT_FOUND_ERROR, ({}), TODO: improve error message",
                            ident_name);
                    },
                }
            },
        }
    }
}

pub fn codegen<'a>(program: ast::Program, logger: &'a Logger<'a>, add_prog_start: bool)
    -> Vec<Instruction>
{
    let mut global = Scope::new("exit".to_string());
    let mut data = CodeData {
        instructions: vec![],
        type_table: types::typegen(&program),
        label_count: 0,
        logger: logger,
        add_to_address: false,
        const_mem: false,
    };

    // Parse globals
    for item in program.items {
        match item {
            ast::FunctionItem(fn_item) => {
                let id = FnIdentId(global.functions.len());
                let name = fn_item.name.clone();
                let label = name.clone();
                global.add_ident(name, id, fn_item.span.clone());
                let function = Function::new(fn_item, &data.type_table, &global, label);
                global.functions.push(function);
            },
            ast::LetItem(let_item) => {
                let id = VarIdentId(global.vars.len());
                let name = let_item.name.clone();
                let label = format!("{}", name);
                global.add_ident(name, id, let_item.span.clone());
                let rtype = data.resolve_type(&global, &let_item.var_type);
                let is_const = let_item.is_const;
                global.vars.push(Variable::new(let_item, rtype, Label(label), is_const));
            },

            // Handled by type gen
            ast::StructItem(..) => {},
        }
    }

    data.instructions.push(asm::RawAsm(DATA_SEGMENT.to_string()));
    // Compile global variables
    for i in 0..global.vars.len() {
        data.compile_global_var(&global, i);
    }

    if add_prog_start {
        data.instructions.push(asm::RawAsm(PROGRAM_START.to_string()));
    }
    else {
        data.instructions.push(asm::RawAsm(CODE_SEGMENT.to_string()));
    }
    // Compile global functions
    for i in 0..global.functions.len() {
        data.compile_global_fn(&global, i);
    }

    data.instructions
}

struct CodeData<'a> {
    instructions: Vec<Instruction>,
    type_table: TypeTable,
    label_count: usize,
    logger: &'a Logger<'a>,
    add_to_address: bool,
    const_mem: bool,
}

impl<'a> CodeData<'a> {
    /// Codegen experienced a fatal error which must kill the program
    fn fatal_error(&self) -> ! {
        panic!();
    }

    /// Generating a unique label id
    fn next_unique_id(&mut self) -> usize {
        self.label_count += 1;
        self.label_count - 1
    }

    fn anon_label(&mut self) -> LabelId {
        format!("a{}", self.next_unique_id())
    }

    /// Compile a global variable
    fn compile_global_var(&mut self, scope: &Scope, var_id: usize) {
        let is_const = scope.vars[var_id].is_const;
        if is_const != self.const_mem {
            if is_const { self.instructions.push(asm::RawAsm(CONST_DATA_SEGMENT.to_string())); }
            else { self.instructions.push(asm::RawAsm(DATA_SEGMENT.to_string())); }
            self.const_mem = is_const;
        }

        // Add the variable's label
        let label = match scope.vars[var_id].location {
            Label(ref s) => s.clone(),
            ref other => panic!("ICE: Location of global var is not a label, was {:?}", other),
        };
        self.instructions.push(asm::Label(label));

        // Allocate and initialize the variable
        let rtype = self.resolve_type(scope, &scope.vars[var_id].ast.var_type);
        match scope.vars[var_id].ast.assignment {
            // Initialized variables
            Some(ref expr) => {
                let rhs_expr: &ast::Expr = match *expr.rhs.expr {
                    ast::CastExpr(ref inner) => &inner.expr,
                    ref other => other,
                };

                match *rhs_expr {
                    ast::LitNumExpr(value) => {
                        self.instructions.push(asm::AllocateWords(vec![value as i32]));
                    },

                    ast::StaticArrayExpr(ref inner) => {
                        match *inner.elements[0].expr {
                            // Array of integers
                            ast::LitNumExpr(..) => {
                                let mut unwrapped = vec![];
                                for element in &inner.elements {
                                    match *element.expr {
                                        ast::LitNumExpr(n) => unwrapped.push(n as i32),
                                        ref _invalid => panic!("Array has not literal value"),
                                    }
                                }
                                self.instructions.push(asm::AllocateWords(unwrapped));
                            },
                            ref invalid => {
                                panic!("Unable to statically resolve expression: `{:?}`", invalid)
                            },
                        }
                    },

                    ast::LitStringExpr(ref value) => {
                        self.instructions.push(asm::AllocateAscii(value.clone()));
                        if value.len() % 4 != 0 {
                            self.instructions.push(asm::Align(2));
                        }
                    },

                    // TODO: Handle other types of static data
                    ref invalid => {
                        println!("{:?}", invalid);
                        unimplemented!();
                    },
                }
            },
            // Uninitialized variables
            None => {
                let size = self.size_of(&rtype) as u32;
                self.instructions.push(asm::AllocateSpace(size));
            }
        }
    }

    /// Compile a global function.
    fn compile_global_fn(&mut self, scope: &Scope, fn_id: usize) {
        // Add the functions label
        let label = scope.functions[fn_id].location.clone();
        let span = scope.functions[fn_id].ast.span.clone();
        self.instructions.push(asm::Label(label));

        // Store caller's frame pointer and set current frame pointer
        self.instructions.push(asm::Store32(asm::Const(0), STACK_POINTER, FRAME_POINTER));
        self.instructions.push(asm::AddUnsigned(FRAME_POINTER, STACK_POINTER, ZERO_REG));

        // Store return location (this should be done by caller)
        self.instructions.push(asm::Store32(asm::Const(4), FRAME_POINTER, RETURN_REG));

        // Create a local scope for this function
        let mut local = Scope::new_with_parent(scope, self.anon_label());

        // Register function parameters as local variables
        // The input params are stored in negative offset before the frame pointer with the last
        // param stored at FRAME_POINTER[-1]
        let mut next_param_addr = 0_i32;
        for &(ref name, ref var_type) in scope.functions[fn_id].ast.params.iter().rev() {
            let var_ast = ast::LetStatement {
                name: name.clone(),
                var_type: var_type.clone(),
                assignment: None,
                is_const: false,
                span: span.clone(),
            };
            let rtype = self.resolve_type(scope, &var_ast.var_type);
            next_param_addr -= self.size_of(&rtype) as i32;
            let var = Variable::new(var_ast, rtype, Offset(next_param_addr as i16), false);

            let id = VarIdentId(local.vars.len());
            local.add_ident(name.clone(), id, span.clone());
            local.vars.push(var);
        }

        // Reserve stack space for the function:
        // Note: since the stack space required is unknown at this point the total memory required
        // for the function is unknown, so the instruction is set to Nop, and changed after the
        // function has been fully compiled. The variable reserve_stack_index keeps track of the
        // index to the value we need to change.
        let reserve_stack_index = self.instructions.len();
        self.instructions.push(asm::Nop);

        // Compile the body of the function
        self.compile_block(&mut local, &scope.functions[fn_id].ast.body);

        // Now set the amount of stack space to allocate
        let frame_size = local.next_offset;
        self.instructions[reserve_stack_index] =
            asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER, frame_size as u16);

        self.instructions.push(asm::Label(local.end_label.clone()));

        // Remove this stack frame, and return to the previous one
        self.instructions.push(asm::Load32(RETURN_REG, asm::Const(4), FRAME_POINTER));
        self.instructions.push(asm::AddUnsigned(STACK_POINTER, FRAME_POINTER, ZERO_REG));
        self.instructions.push(asm::Load32(FRAME_POINTER, asm::Const(0), STACK_POINTER));
        self.instructions.push(asm::JumpR(RETURN_REG));

        // Insert a new line to make the output nicer to read
        self.instructions.push(asm::RawAsm("".to_string()));
    }

    fn compile_block(&mut self, scope: &mut Scope, block: &ast::Block) {
        for statement in &block.statements {
            self.compile_expression(scope, statement);
        }
    }

    fn compile_expression(&mut self, scope: &mut Scope, expression: &ast::Expression) {
        let span = expression.span;
        match *expression.expr {
            ast::RefExpr(ref inner) => {
                let valid_address = self.compile_address(scope, inner);
                if !valid_address {
                    self.logger.report_error(format!("Cannot take reference to target expression"),
                        span);
                    self.fatal_error();
                }
            },
            ast::DerefExpr(ref inner) => {
                // Check that we can dereference the expression
                let inner_type = self.resolve_type(scope, &inner.rtype);
                match inner_type {
                    types::Pointer(..) => {
                        // Evaluate the inner expression
                        self.compile_expression(scope, inner);

                        // Then dereference it
                        self.load_var(inner_type.deref(), &Register(RESULT_REG));
                    },
                    types::StaticArray(..) => {
                        // Evaluate the inner expression
                        self.compile_expression(scope, inner);
                        // Since this is a static array we don't need to dereference it
                    },
                    _invalid => {
                        self.logger.report_error(format!("type `{:?}` cannot be dereferenced",
                            "FIXME"), span);
                        self.fatal_error();
                    }
                }
            },
            ast::FieldRefExpr(ref inner) => {
                self.compile_field_ref(scope, inner);
                let inner_type = self.resolve_type(scope, &expression.rtype);
                self.load_var(&inner_type, &Register(RESULT_REG));
            },
            ast::ArrayIndexExpr(ref inner) => {
                self.compile_array_index(scope, inner);
                let inner_type = self.resolve_type(scope, &expression.rtype);
                self.load_var(&inner_type, &Register(RESULT_REG));
            },
            ast::IfExpr(ref inner) => self.compile_if(scope, inner),
            ast::ForLoopExpr(ref inner) => self.compile_for(scope, inner),
            ast::LoopExpr(ref inner) => self.compile_loop(scope, inner),
            ast::CallExpr(ref inner) => self.compile_call(scope, inner),
            ast::Break => {
                match scope.loop_ends.last() {
                    Some(label) => self.instructions.push(asm::Jump(label.clone())),
                    None => {
                        self.logger.report_error(format!("`break` outside of loop"), span);
                        self.fatal_error();
                    },
                }
            },
            ast::Return(ref inner) => {
                self.compile_expression(scope, inner);
                let return_label = scope.end_label.clone();
                self.instructions.push(asm::Jump(return_label));
            },
            ast::LetExpr(ref inner) => self.compile_let(scope, inner),
            ast::AssignExpr(ref inner) => self.compile_assign(scope, inner),
            ast::VariableExpr(ref name) => {
                let var = scope.get_ident(name, span).unwrap_var();
                self.load_var(&var.rtype, &var.location);
            },
            ast::StaticArrayExpr(ref inner) => self.compile_static_array(scope, inner),
            ast::LitStringExpr(ref inner) => {
                // Convert the string into a byte array
                // FIXME: this should happen in the lexer/parser
                let mut bytes = vec![];

                // The remaining characters left to parse in this expression
                let mut rem: &str = &inner;

                while let Some(mut next) = rem.chars().next() {
                    rem = &rem[next.len_utf8()..];
                    if next == '\\' {
                        let escaped = rem.chars().next().expect("ICE, end of string was escaped");
                        rem = &rem[escaped.len_utf8()..];

                        match escaped {
                            'n' => next = '\n',
                            '0' => next = '\0',
                            invalid => panic!("Invalid escape char: {}", invalid),
                        }
                    }

                    let expression = ast::Expression {
                        expr: box ast::LitCharExpr(next),
                        rtype: ast::Primitive(ast::CharType),
                        span: span.clone(),
                    };
                    bytes.push(expression);
                }

                let static_array = ast::StaticArray {
                    elements: bytes,
                    span: span.clone(),
                };
                self.compile_static_array(scope, &static_array);
            },
            ast::StructInitExpr(ref inner) => self.compile_struct_init(scope, inner),
            ast::LitNumExpr(value) => {
                self.instructions.push(asm::AddSignedValue(RESULT_REG, ZERO_REG, value as i16));
            },
            ast::LitCharExpr(value) => {
                self.instructions.push(asm::AddSignedValue(RESULT_REG, ZERO_REG, value as i16));
            },
            ast::AsmOpExpr(ref inner) => {
                self.instructions.push(asm::RawAsm(inner.clone()));
            },
            ast::CastExpr(ref inner) => self.compile_expression(scope, inner),
            ast::EmptyExpr => {},
        }
    }

    fn compile_field_ref(&mut self, scope: &mut Scope, field_ref: &ast::FieldRef) {
        self.compile_expression(scope, &field_ref.target);

        let target_type = self.resolve_type(scope, &field_ref.target.rtype);
        let target_base_type = self.type_table.base_type(&target_type);

        let (field_offset, _) = self.find_field(target_base_type, &field_ref.field,
            field_ref.span.clone());

        // Add the offset to the target address
        self.instructions.push(asm::AddUnsignedValue(RESULT_REG, RESULT_REG, field_offset));
    }

    fn find_field(&self, target_type: &types::BaseType, field: &String, span: InputSpan)
        -> (u16, types::Type)
    {
        match *target_type {
            types::Composite(ref inner) => {
                match inner.fields.get(field) {
                    Some(&(offset, ref type_)) => (offset, type_.clone()),
                    None => {
                        self.logger.report_error(format!("type `{:?}` has no field {:?}",
                            target_type, field), span);
                        self.fatal_error();
                    }
                }
            },
            ref invalid => {
                self.logger.report_error(format!("type `{:?}` has no field {:?}", invalid, field),
                    span);
                self.fatal_error();
            }
        }
    }

    fn compile_array_index(&mut self, scope: &mut Scope, index_expr: &ast::ArrayIndex) {
        // Check that the type that we are indexing can be indexed
        let target_type = self.resolve_type(scope, &index_expr.target.rtype);
        match target_type {
            types::Pointer(..) | types::StaticArray(..) => {},
            _invalid => {
                self.logger.report_error(format!("type `{:?}` cannot be dereferenced", "FIXME"),
                    index_expr.span);
                self.fatal_error();
            }
        }

        // Evaluate the index
        self.compile_expression(scope, &index_expr.index);

        // Check that we are indexing with the correct type
        let index_type = self.resolve_type(scope, &index_expr.index.rtype);
        self.check_type(&index_type, &INT_TYPE, index_expr.index.span.clone());

        // Multiply by the size of the target type
        let type_size = self.unaligned_size_of(target_type.deref());
        self.multiply_by(type_size as usize);

        // Either replace or add to the current address
        if self.add_to_address {
            self.instructions.push(asm::AddUnsigned(ADDR_REG, RESULT_REG, ADDR_REG));
        }
        else {
            self.instructions.push(asm::AddUnsigned(ADDR_REG, RESULT_REG, ZERO_REG));
        }

        // Evaluate the target address
        self.add_to_address = true;
        self.compile_expression(scope, &index_expr.target);
        self.add_to_address = false;

        // Add the index to the target address
        self.instructions.push(asm::AddUnsigned(RESULT_REG, RESULT_REG, ADDR_REG));
    }

    fn compile_if(&mut self, scope: &mut Scope, if_statement: &ast::IfStatement) {
        self.compile_expression(scope, &if_statement.condition);

        // Check that the expression returns a boolean type
        let cond_type = self.resolve_type(scope, &if_statement.condition.rtype);
        self.check_type(&cond_type, &BOOL_TYPE, if_statement.span.clone());

        let else_label = self.anon_label();
        let end_label = match if_statement.else_block {
            Some(..) => self.anon_label(),
            // If there is no else block, then the end label is equal to the else label
            None => else_label.clone(),
        };

        self.instructions.push(asm::JumpIfZero(RESULT_REG, else_label.clone()));

        // Compile the then block
        self.compile_block(scope, &if_statement.body);
        let then_rtype = self.resolve_type(scope, &if_statement.body.rtype());

        match if_statement.else_block {
            Some(ref block) => {
                // If there is an else block we need to add a jump from the then block to the
                // end label, and add a label for the else part
                self.instructions.push(asm::Jump(end_label.clone()));
                self.instructions.push(asm::Label(else_label));
                // Then compile the else block
                self.compile_block(scope, block);

                // Check that both sides return the same type
                let else_rtype = self.resolve_type(scope, &block.rtype());
                self.check_type(&else_rtype, &then_rtype, block.span.clone());
            }
            None => {
                // If the else block was left unspecified, then the if statement must return the
                // unit type
                self.check_type(&then_rtype, &UNIT_TYPE, if_statement.span.clone());
            }
        }

        // Add the end label
        self.instructions.push(asm::Label(end_label));
    }

    /// Compile a for loop:
    /// Note: We directly compile for loops instead of de-sugaring them into a normal loop with an
    /// if break, for efficiency.
    fn compile_for(&mut self, scope: &mut Scope, for_statement: &ast::ForLoopStatement) {
        let var_size = self.size_of(&INT_TYPE) as u16;

        // Create a fake ast so the we can refer to the variable inside the loop
        let loop_var_name = for_statement.loop_var.clone();
        let loop_var_ast = ast::LetStatement {
            name: loop_var_name.clone(),
            var_type: ast::Primitive(ast::IntType),
            assignment: None,
            is_const: false,
            span: for_statement.span.clone(),
        };
        let loop_var_type = self.resolve_type(scope, &ast::Primitive(ast::IntType));
        let loop_var = Variable::new(loop_var_ast, loop_var_type, Offset(scope.next_offset), false);
        scope.next_offset += var_size as i16 * 2;

        let loop_var_offset = scope.next_offset - 8;
        let end_var_offset = scope.next_offset - 4;

        let id = VarIdentId(scope.vars.len());
        scope.add_ident(loop_var_name, id, for_statement.span.clone());
        scope.vars.push(loop_var);

        // Compile the expression for the range end. The end range is written first, so that we have
        // the loop var ready in the RESULT_REG
        self.compile_expression(scope, &for_statement.end);
        // Write the end expression to the stack
        self.instructions.push(asm::Store32(asm::Const(end_var_offset), FRAME_POINTER,
            RESULT_REG));

        // Check that the end expression has the correct type
        let end_type = self.resolve_type(scope, &for_statement.end.rtype);
        self.check_type(&end_type, &INT_TYPE, for_statement.end.span.clone());

        // Compile the expression for the range start
        self.compile_expression(scope, &for_statement.start);
        // Write the start expression to the stack
        self.instructions.push(asm::Store32(asm::Const(loop_var_offset), FRAME_POINTER,
            RESULT_REG));

        // Check that the start expression has the correct type
        let start_type = self.resolve_type(scope, &for_statement.start.rtype);
        self.check_type(&start_type, &INT_TYPE, for_statement.start.span.clone());

        let start_label = self.anon_label();
        let cond_label = self.anon_label();
        let end_label = self.anon_label();
        scope.loop_ends.push(end_label.clone());

        // Check the condition before we start by jumping to the condition label
        self.instructions.push(asm::Jump(cond_label.clone()));

        // Compile the main body of the loop
        self.instructions.push(asm::Label(start_label.clone()));
        self.compile_block(scope, &for_statement.body);

        // Increment the loop variable
        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(loop_var_offset), FRAME_POINTER));
        self.instructions.push(asm::AddUnsignedValue(RESULT_REG, RESULT_REG, 1));
        self.instructions.push(asm::Store32(asm::Const(loop_var_offset), FRAME_POINTER,
            RESULT_REG));

        // Jump back to the start if we haven't finished the loop.
        // Note: The loop variable will be already loaded into RESULT_REG
        self.instructions.push(asm::Label(cond_label));
        self.instructions.push(asm::Load32(TEMP_REG, asm::Const(end_var_offset), FRAME_POINTER));
        self.instructions.push(asm::SetLt(RESULT_REG, RESULT_REG, TEMP_REG));
        self.instructions.push(asm::JumpIfNotZero(RESULT_REG, start_label));

        // Add end label
        let end_label = scope.loop_ends.pop().expect("ICE: Missing label after loop");
        self.instructions.push(asm::Label(end_label));

        // Check that the body of the loop returns the correct type
        let body_rtype = self.resolve_type(scope, &for_statement.body.rtype());
        self.check_type(&body_rtype, &UNIT_TYPE, for_statement.span.clone());
    }

    fn compile_loop(&mut self, scope: &mut Scope, loop_statement: &ast::LoopStatement) {
        let start_label = self.anon_label();
        self.instructions.push(asm::Label(start_label.clone()));

        // Add the end label to the loop ends vector, so that it can be used by breaks
        let end_label = self.anon_label();
        scope.loop_ends.push(end_label.clone());

        self.compile_block(scope, &loop_statement.body);

        // Add jump to start
        self.instructions.push(asm::Jump(start_label));

        // Add end label
        let end_label = scope.loop_ends.pop().expect("ICE: Missing label after loop");
        self.instructions.push(asm::Label(end_label));

        // Check that the body of the loop returns the correct type
        let body_rtype = self.resolve_type(scope, &loop_statement.body.rtype());
        self.check_type(&body_rtype, &UNIT_TYPE, loop_statement.span.clone());
    }

    fn compile_call(&mut self, scope: &mut Scope, call: &ast::FunctionCall) {
        let mut call_args = vec![];
        // Keep track of the offset of the stack, so that we can restore it later.
        let mut stack_offset = 0;
        for arg in &call.args {
            let arg_type = self.resolve_type(scope, &arg.rtype);
            let arg_size = self.size_of(&arg_type);

            // Compile the expression
            self.compile_expression(scope, arg);
            // Write the result of the expression to the stack
            self.copy_var(&arg_type, RESULT_REG, STACK_POINTER);
            // Increment the stack
            self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER,
                arg_size as u16));
            stack_offset += arg_size as i16;

            call_args.push(arg_type);
        }

        // Get the function corresponding to the call
        let function = match scope.get_ident(&call.name, call.span.clone()) {
            FnIdent(ident) => ident,
            VarIdent(..) => panic!("ERROR_EXPECTED_FUNCTION_FOUND_VAR, TODO: Improve this error"),
        };

        // Check that the call args match the function args
        if call_args.len() != function.arg_types.len() {
            panic!("INCORRECT NUMBER OF ARGUMENTS");
        }
        for (call_arg, fn_arg) in call_args.iter().zip(function.arg_types.iter()) {
            self.check_type(call_arg, fn_arg, call.span.clone());
        }

        // Make the call
        self.instructions.push(asm::JumpStore(function.location.clone()));

        // Restore the stack
        if stack_offset != 0 {
            self.instructions.push(asm::SubUnsignedValue(STACK_POINTER, STACK_POINTER,
                stack_offset as u16));
        }
    }

    fn compile_let(&mut self, scope: &mut Scope, let_statement: &ast::LetStatement) {
        // Register this variable
        let id = VarIdentId(scope.vars.len());
        scope.add_ident(let_statement.name.clone(), id, let_statement.span);

        let rtype = self.resolve_type(scope, &let_statement.var_type);

        let var = Variable::new(let_statement.clone(), rtype.clone(), Offset(scope.next_offset),
            let_statement.is_const);
        scope.next_offset += self.size_of(&rtype) as i16;
        scope.vars.push(var);

        // Compile optional assignment
        match let_statement.assignment {
            Some(ref assignment) => self.compile_assign(scope, assignment),
            None => {},
        }
    }

    fn compile_assign(&mut self, scope: &mut Scope, assignment: &ast::Assignment) {
        // Compile the rhs expression and store the result in the location found
        self.compile_expression(scope, &assignment.rhs);
        self.instructions.push(asm::AddSigned(TEMP_REG, RESULT_REG, ZERO_REG));

        // Check that the rhs result matches the target
        let target_type = self.resolve_type(scope, &assignment.target.rtype);
        self.check_type(&self.resolve_type(scope, &assignment.rhs.rtype), &target_type,
            assignment.span.clone());

        // Get the address of where we want to place the variable
        let valid_address = self.compile_address(scope, &assignment.target);
        if !valid_address {
            self.logger.report_error(format!("illegal left-hand side expression"),
                assignment.target.span.clone());
            self.fatal_error();
        };

        // We now have the result of the rhs in TEMP_REG and the address we want to assign to in
        // RESULT_REG. The type of the variable is required so that we know how to copy the data.
        self.copy_var(&target_type, TEMP_REG, RESULT_REG);
    }

    fn compile_struct_init(&mut self, scope: &mut Scope, struct_init: &ast::StructInit) {
        // Determine the type of the struct
        let struct_type = self.resolve_type(scope, &ast::UserType(struct_init.type_name.clone()));
        let struct_base_type = self.type_table.base_type(&struct_type).clone();
        let struct_size = self.size_of(&struct_type);

        // Reserve memory for the struct
        self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER, struct_size));

        for &(ref field_name, ref expression) in &struct_init.field_init {
            let (field_offset, field_type) = self.find_field(&struct_base_type, field_name,
                struct_init.span.clone());

            self.compile_expression(scope, expression);

            // Check that the types match
            self.check_type(&self.resolve_type(scope, &expression.rtype), &field_type,
                struct_init.span.clone());

            self.instructions.push(asm::AddSignedValue(TEMP_REG, STACK_POINTER,
                (field_offset as i16) - (struct_size as i16)));
            self.copy_var(&field_type, RESULT_REG, TEMP_REG);
        }

        // Unreserve the memory from the struct
        self.instructions.push(asm::AddSignedValue(STACK_POINTER, STACK_POINTER,
            -(struct_size as i16)));
        // Return a pointer to the struct
        self.instructions.push(asm::AddUnsigned(RESULT_REG, STACK_POINTER, ZERO_REG));
    }

    fn compile_static_array(&mut self, scope: &mut Scope, array: &ast::StaticArray) {
        // Special case for a zero sized array
        if array.elements.len() == 0 {
            self.instructions.push(asm::AddUnsigned(RESULT_REG, STACK_POINTER, ZERO_REG));
            return;
        }

        // Compile the first element and add it to the start of the array.
        // NOTE: this needs to be done first so that we can properly resolve the type of the array
        // elements
        self.compile_expression(scope, &array.elements[0]);
        let element_type = self.resolve_type(scope, &array.elements[0].rtype);
        // Using the unaligned size for arrays allows us to efficiently store strings as byte arrays
        let element_size = self.unaligned_size_of(&element_type);
        self.copy_var(&element_type, RESULT_REG, STACK_POINTER);

        let mut offset = element_size as i16;
        self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER, element_size));

        for element in array.elements.iter().skip(1) {
            self.compile_expression(scope, element);
            self.copy_var(&element_type, RESULT_REG, STACK_POINTER);
            self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER,
                element_size));
            offset += element_size as i16;
        }

        // Ensure that the stack pointer is correctly aligned
        if offset % 4 != 0 {
            let pad = 4 - (offset % 4);
            offset = offset + pad;
            self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER,
                pad as u16));
        }

        // Unreserve the memory from the array
        self.instructions.push(asm::AddSignedValue(STACK_POINTER, STACK_POINTER, -offset));
        // Return a pointer to the first element
        self.instructions.push(asm::AddUnsigned(RESULT_REG, STACK_POINTER, ZERO_REG));
    }

    fn copy_var(&mut self, var_type: &types::Type, from: asm::RegId, to: asm::RegId) {
        match *var_type {
            CHAR_TYPE => {
                self.instructions.push(asm::Store8(asm::Const(0), to, from));
            },

            // These types are a single word in size
            INT_TYPE | BOOL_TYPE | types::Pointer(..) => {
                self.instructions.push(asm::Store32(asm::Const(0), to, from));
            },

            // Other types cannot be stored in a single word, and since there is no easy way to do
            // a memcopy in DLX we must manually copy all bytes. In this case, the from register
            // will store the location of the first byte
            ref other => {
                // The copy register should never be either of the input registers
                assert!(from != COPY_REG && to != COPY_REG);

                // NOTE: types *must* be word aligned
                let num_words = (self.size_of(other) / 4) as i16;
                // Perform a load and store for each of the words
                for i in 0..num_words {
                    self.instructions.push(asm::Load32(COPY_REG, asm::Const(i * 4), from));
                    self.instructions.push(asm::Store32(asm::Const(i * 4), to, COPY_REG));
                }
            }
        }
    }

    fn compile_address(&mut self, scope: &mut Scope, expression: &ast::Expression) -> bool {
        let span = expression.span;
        match *expression.expr {
            // Address of an ordinary variable
            ast::VariableExpr(ref name) => {
                let var = scope.get_ident(name, span).unwrap_var();
                self.address_of(&var.location);
            },

            // Address of a dereference (aka don't dereference)
            ast::DerefExpr(ref inner) => self.compile_expression(scope, inner),

            // Address of an array index
            ast::ArrayIndexExpr(ref inner) => self.compile_array_index(scope, inner),

            // Address of a struct field
            ast::FieldRefExpr(ref inner) => self.compile_field_ref(scope, inner),

            // Nothing else has a proper address
            _ => return false,
        }
        true
    }

    fn load_var(&mut self, var_type: &types::Type, location: &Location) {
        match *var_type {
            // These types are byte sized
            CHAR_TYPE => {
                match *location {
                    Label(ref _label) => {
                        // HACK to allow us to access some extra memory
                        self.address_of(location);
                        self.instructions.push(asm::Load8(RESULT_REG, asm::Const(0), RESULT_REG));
                        //self.instructions.push(asm::Load8(RESULT_REG, asm::Unknown(label.clone()),
                        //    ZERO_REG));
                    },
                    Offset(amount) => {
                        self.instructions.push(asm::Load8(RESULT_REG, asm::Const(amount),
                            FRAME_POINTER));
                    },
                    Register(id) => {
                        self.instructions.push(asm::Load8(RESULT_REG, asm::Const(0), id));
                    },
                }
            },

            // These types are word sized
            INT_TYPE | BOOL_TYPE | types::Pointer(..) => {
                match *location {
                    Label(ref _label) => {
                        // HACK to allow us to access some extra memory
                        self.address_of(location);
                        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(0), RESULT_REG));
                        //self.instructions.push(asm::Load32(RESULT_REG, asm::Unknown(label.clone()),
                        //    ZERO_REG));
                    },
                    Offset(amount) => {
                        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(amount),
                            FRAME_POINTER));
                    },
                    Register(id) => {
                        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(0), id));
                    },
                }
            },
            // These types cannot be stored in registers so we load their address into the result
            // reg
            _ => self.address_of(location),
        }
    }

    fn address_of(&mut self, var_location: &Location) {
        match *var_location {
            Label(ref label) => {
                let asm = format!("        addui   r{},r{},{}", RESULT_REG, ZERO_REG, label);
                self.instructions.push(asm::RawAsm(asm));
            },
            Offset(offset) => {
                self.instructions.push(asm::AddSignedValue(RESULT_REG, FRAME_POINTER, offset));
            },
            Register(id) => {
                self.instructions.push(asm::AddUnsigned(RESULT_REG, id, ZERO_REG));
            },
        }
    }

    fn multiply_by(&mut self, num: usize) {
        // Check if it is a power of 2
        // TODO: Generate extra code for multiplications that are not a power of 2
        if !(num & (num - 1) == 0) {
            panic!("ICE, error multiplying by a number that is not a power of 2");
        }

        let lshift_amount = (num as f32).log2() as u16;
        if lshift_amount != 0 {
            self.instructions.push(asm::LShiftValue(RESULT_REG, RESULT_REG, lshift_amount));
        }
    }

    /// Check that a type is the same as the expected type or one path never returns
    fn check_type(&self, input: &Type, expected: &Type, span: InputSpan) {
        if input != &types::Bottom && expected != &types::Bottom && input != expected {
            self.logger.report_error(format!("incorrect type, expected: {:?}, found: {:?}",
                expected, input), span);
            self.fatal_error();
        }
    }

    fn size_of(&self, type_: &Type) -> u16 {
        self.type_table.size_of(type_)
    }

    fn unaligned_size_of(&self, type_: &Type) -> u16 {
        self.type_table.unaligned_size_of(type_)
    }

    fn resolve_type(&self, scope: &Scope, ast_type: &ast::Type) -> Type {
        self.type_table.resolve_type(scope, ast_type)
    }
}
