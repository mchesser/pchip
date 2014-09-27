use std::collections::HashMap;
use std::fmt;

use ast;

use dlx::asm;
use dlx::asm::{RegId, LabelId};
use dlx::asm::Instruction;
use dlx::types;
use dlx::types::{Type, TypeTable};

use error::{InputPos, InputSpan, Logger};

static INT_TYPE: &'static Type = &types::Normal(0);
static BOOL_TYPE: &'static Type = &types::Normal(1);
static UNIT_TYPE: &'static Type = &types::Normal(2);

// Special register that is always 0
static ZERO_REG: RegId = 0;
// Frame pointer register
static FRAME_POINTER: RegId = 30;
// Stack pointer register
static STACK_POINTER: RegId = 14;
// Heap pointer register
static HEAP_POINTER: RegId = 15;
// Return address register (set by jal)
static RETURN_REG: RegId = 31;
// Register used for storing the results of computations
static RESULT_REG: RegId = 1;
// Register used for temporary values
static TEMP_REG: RegId = 2;

static DATA_SEGMENT: &'static str =
"
; Start of compile time data segment
        .seg    data
";

static CODE_SEGMENT: &'static str =
"
; Start of code segment
        .seg    code
";

static PROGRAM_START: &'static str =
"
; Allocate some dynamic memory for the program to use
        .seg    dynamic_mem
stack   .space  1000
heap    .space  1000

; Manually start the program
        .seg    code
        .start  prgsrt

prgsrt  addui   r14,r0,stack            ; Give the program a stack
        addui   r15,r0,heap             ; Give the program a heap
        jal     main                    ; Jump to the program entry point
        halt                            ; Stop the machine
";


struct Function {
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

#[deriving(Clone, Show)]
enum Location {
    Label(LabelId),
    Offset(i16),
    Register(RegId),
}

struct Variable {
    ast: ast::LetStatement,
    rtype: Type,
    location: Location,
}

impl Variable {
    fn new(ast: ast::LetStatement, rtype: Type, location: Location) -> Variable {
        Variable {
            ast: ast,
            rtype: rtype,
            location: location,
        }
    }
}

#[deriving(PartialEq, Hash)]
enum IdentId {
    FnIdentId(uint),
    VarIdentId(uint),
}

enum Ident<'a> {
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
    fn add_ident(&mut self, ident_name: String, ident: IdentId, span: InputSpan) {
        let stored_ident = self.ident_table.find_or_insert(ident_name, ident);
        // If the stored identifier is different to the one we are attempting to add, then this
        // identifier shadows an existing one. Variable shadowing is currently not supported.
        if *stored_ident != ident {
            fail!("IDENT_SHADOW_ERROR, TODO: improve error message")
        }
    }

    /// Get the identifier corresponding to an identifier name.
    pub fn get_ident(&self, ident_name: &String, span: InputSpan) -> Ident {
        match self.ident_table.find(ident_name) {
            Some(&FnIdentId(id)) => FnIdent(&self.functions[id]),
            Some(&VarIdentId(id)) => VarIdent(&self.vars[id]),
            None => {
                // If the identifier was not found in this scope, check the parent scope.
                match self.parent {
                    Some(parent) => parent.get_ident(ident_name, span),
                    None => {
                        // Reached the top level scope, but still could not find the identifier
                        // therefore it doesn't not exist at this location.
                        fail!("IDENT_NOT_FOUND_ERROR, ({}), TODO: improve error message",
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
    let mut global = Scope::new("exit".into_string());
    let mut data = CodeData {
        instructions: vec![],
        type_table: types::typegen(&program),
        label_count: 0,
        logger: logger,
    };

    // Parse globals
    for item in program.items.into_iter() {
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
                let label = format!("{}{}", name, data.next_unique_id());
                global.add_ident(name, id, let_item.span.clone());
                let rtype = data.resolve_type(&global, &let_item.var_type);
                global.vars.push(Variable::new(let_item, rtype, Label(label)));
            },

            // Handled by type gen
            ast::StructItem(..) => {},
        }
    }

    data.instructions.push(asm::RawAsm(DATA_SEGMENT.to_string()));
    // Compile global variables
    for i in range(0u, global.vars.len()) {
        data.compile_global_var(&global, i);
    }

    data.instructions.push(asm::RawAsm(CODE_SEGMENT.to_string()));
    if add_prog_start {
        data.instructions.push(asm::RawAsm(PROGRAM_START.to_string()));
    }
    // Compile global functions
    for i in range(0u, global.functions.len()) {
        data.compile_global_fn(&global, i);
    }

    data.instructions
}

struct CodeData<'a> {
    instructions: Vec<Instruction>,
    type_table: TypeTable,
    label_count: uint,
    logger: &'a Logger<'a>,
}

impl<'a> CodeData<'a> {
    /// Codegen experienced a fatal error which must kill the program
    fn fatal_error(&self) -> ! {
        fail!();
    }

    /// Generating a unique label id
    fn next_unique_id(&mut self) -> uint {
        self.label_count += 1;
        self.label_count - 1
    }

    fn anon_label(&mut self) -> LabelId {
        format!("a{}", self.next_unique_id())
    }

    /// Compile a global variable
    fn compile_global_var(&mut self, scope: &Scope, var_id: uint) {
        // Add the variable's label
        let label = match scope.vars[var_id].location {
            Label(ref s) => s.clone(),
            ref other => fail!("ICE: Location of global var is not a label, was {}", other),
        };
        self.instructions.push(asm::Label(label));

        // Allocate and initialize the variable
        let rtype = self.resolve_type(scope, &scope.vars[var_id].ast.var_type);
        match scope.vars[var_id].ast.assignment {
            // Initialized variables
            Some(ref expr) => {
                match *expr.rhs.expr {
                    // For now global vars can only be words
                    ast::LitNumExpr(n) => {
                        self.instructions.push(asm::AllocateWords(vec![n as i32]));
                    },

                    // TODO: Handle other types of static data
                    _ => unimplemented!(),
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
    fn compile_global_fn(&mut self, scope: &Scope, fn_id: uint) {
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
        let mut next_param_addr = 0;
        for &(ref name, ref var_type) in scope.functions[fn_id].ast.params.iter().rev() {
            let var_ast = ast::LetStatement {
                name: name.clone(),
                var_type: var_type.clone(),
                assignment: None,
                span: span.clone(),
            };
            let rtype = self.resolve_type(scope, &var_ast.var_type);
            next_param_addr -= self.size_of(&rtype);
            let var = Variable::new(var_ast, rtype, Offset(next_param_addr as i16));

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
        *self.instructions.get_mut(reserve_stack_index) =
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
        for statement in block.statements.iter() {
            self.compile_expression(scope, statement);
        }
    }

    fn compile_expression(&mut self, scope: &mut Scope, expression: &ast::Expression) {
        let span = expression.span;
        match *expression.expr {
            ast::RefExpr(ref target) => {
                // Get the identifier corresponding to the target
                let target_ident = match scope.get_ident(target, span) {
                    FnIdent(..) => {
                        self.logger.report_error(format!("attempted to take reference to function"),
                            span);
                        self.fatal_error();
                    },
                    VarIdent(ident) => ident,
                };
                // Take a reference to this variable
                match target_ident.location {
                    Label(ref name) => {
                        let asm = format!("        addui,r{},r{},{}", RESULT_REG, ZERO_REG, name);
                        self.instructions.push(asm::RawAsm(asm));
                    },
                    Offset(offset) => {
                        self.instructions.push(asm::AddSignedValue(RESULT_REG, FRAME_POINTER,
                            offset))
                    },
                    Register(..) => {
                        fail!("ICE: Attempted to take the address of a variable in a register")
                    },
                }

            },
            ast::DerefExpr(ref inner) => {
                // Evaluate the inner expression
                self.compile_expression(scope, inner);
                // Then dereference it
                self.instructions.push(asm::Load32(RESULT_REG, asm::Const(0), RESULT_REG));
            },
            ast::FieldExpr(ref inner) => unimplemented!(),
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
                let var_location = match scope.get_ident(name, InputSpan::invalid()) {
                    FnIdent(..) => fail!("ICE: Functions can't be treated as variables yet"),
                    VarIdent(ident) => ident.location.clone(),
                };
                match var_location {
                    Label(label) => {
                        self.instructions.push(asm::Load32(RESULT_REG, asm::Unknown(label),
                            ZERO_REG));
                    },
                    Offset(amount) => {
                        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(amount),
                            FRAME_POINTER));
                    },
                    Register(id) => {
                        self.instructions.push(asm::AddSigned(RESULT_REG, id, ZERO_REG));
                    },
                }
            },
            ast::StructInitExpr(ref inner) => {
                unimplemented!();
            },
            ast::LitNumExpr(value) => {
                self.instructions.push(asm::AddSignedValue(RESULT_REG, ZERO_REG, value as i16));
            },
            ast::AsmOpExpr(ref inner) => {
                self.instructions.push(asm::RawAsm(inner.clone()));
            }
            ast::EmptyExpr => {},
        }
    }

    fn compile_if(&mut self, scope: &mut Scope, if_statement: &ast::IfStatement) {
        // Check that the expression returns a boolean type
        let cond_type = self.resolve_type(scope, &if_statement.condition.rtype);
        self.check_type(&cond_type, BOOL_TYPE, if_statement.span.clone());

        self.compile_expression(scope, &if_statement.condition);

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
                // Check that both sides return the same type
                let else_rtype = self.resolve_type(scope, &block.rtype());
                self.check_type(&else_rtype, &then_rtype, block.span.clone());

                // If there is an else block we need to add a jump from the then block to the
                // end label, and add a label for the else part
                self.instructions.push(asm::Jump(end_label.clone()));
                self.instructions.push(asm::Label(else_label));
                // Then compile the else block
                self.compile_block(scope, block);
            }
            None => {
                // If the else block was left unspecified, then the if statement must return the
                // unit type
                self.check_type(&then_rtype, UNIT_TYPE, if_statement.span.clone());
            }
        }

        // Add the end label
        self.instructions.push(asm::Label(end_label));
    }

    /// Compile a for loop:
    /// Note: We directly compile for loops instead of de-sugaring them into a normal loop with an
    /// if break, for efficiency.
    fn compile_for(&mut self, scope: &mut Scope, for_statement: &ast::ForLoopStatement) {
        // Check that the body of the loop returns the correct type
        let body_rtype = self.resolve_type(scope, &for_statement.body.rtype());
        self.check_type(&body_rtype, UNIT_TYPE, for_statement.span.clone());

        // Check that the start expression has the correct type
        let start_type = self.resolve_type(scope, &for_statement.start.rtype);
        self.check_type(&start_type, INT_TYPE, for_statement.start.span.clone());
        // Compile the expression for the range start
        self.compile_expression(scope, &for_statement.start);
        // Write the start expression to the stack
        self.instructions.push(asm::Store32(asm::Const(0), STACK_POINTER, RESULT_REG));

        // Check that the end expression has the correct type
        let end_type = self.resolve_type(scope, &for_statement.end.rtype);
        let var_size = self.size_of(&end_type) as u16;
        self.check_type(&end_type, INT_TYPE, for_statement.end.span.clone());
        // Compile the expression for the range end
        self.compile_expression(scope, &for_statement.end);
        // Write the end expression to the stack
        self.instructions.push(asm::Store32(asm::Const(var_size as i16), STACK_POINTER,
            RESULT_REG));

        // Increment the stack
        self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER, var_size * 2));

        // Create a fake ast so the we can refer to the variable inside the loop
        let loop_var_name = for_statement.loop_var.clone();
        let loop_var_ast = ast::LetStatement {
            name: loop_var_name.clone(),
            var_type: ast::Primitive(ast::IntType),
            assignment: None,
            span: for_statement.span.clone(),
        };
        let loop_var_type = self.resolve_type(scope, &ast::Primitive(ast::IntType));
        let loop_var = Variable::new(loop_var_ast, loop_var_type, Offset(scope.next_offset));
        scope.next_offset += var_size as i16 * 2;

        let id = VarIdentId(scope.vars.len());
        scope.add_ident(loop_var_name, id, for_statement.span.clone());
        scope.vars.push(loop_var);

        let start_label = self.anon_label();
        let cond_label = self.anon_label();
        let end_label = self.anon_label();
        scope.loop_ends.push(end_label.clone());

        // Check the condition before we start by jumping to the condition label
        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(-8), STACK_POINTER));
        self.instructions.push(asm::Jump(cond_label.clone()));

        // Compile the main body of the loop
        self.instructions.push(asm::Label(start_label.clone()));
        self.compile_block(scope, &for_statement.body);

        // Increment the loop variable
        self.instructions.push(asm::Load32(RESULT_REG, asm::Const(-8), STACK_POINTER));
        self.instructions.push(asm::AddUnsignedValue(RESULT_REG, RESULT_REG, 1));
        self.instructions.push(asm::Store32(asm::Const(-8), STACK_POINTER, RESULT_REG));

        // Jump back to the start if we haven't finished the loop.
        // Note: The loop variable will be already loaded into RESULT_REG
        self.instructions.push(asm::Label(cond_label));
        self.instructions.push(asm::Load32(TEMP_REG, asm::Const(-4), STACK_POINTER));
        self.instructions.push(asm::SetLt(RESULT_REG, RESULT_REG, TEMP_REG));
        self.instructions.push(asm::JumpIfNotZero(RESULT_REG, start_label));

        // Add end label
        let end_label = scope.loop_ends.pop().expect("ICE: Missing label after loop");
        self.instructions.push(asm::Label(end_label));

        // Restore the stack
        self.instructions.push(asm::SubUnsignedValue(STACK_POINTER, STACK_POINTER, var_size * 2));
        scope.next_offset -= var_size as i16 * 2;
    }

    fn compile_loop(&mut self, scope: &mut Scope, loop_statement: &ast::LoopStatement) {
        // Check that the body of the loop returns the correct type
        let body_rtype = self.resolve_type(scope, &loop_statement.body.rtype());
        self.check_type(&body_rtype, UNIT_TYPE, loop_statement.span.clone());

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
    }

    fn compile_call(&mut self, scope: &mut Scope, call: &ast::FunctionCall) {
        let mut call_args = vec![];
        // Keep track of the offset of the stack, so that we can restore it later.
        let mut stack_offset = 0;
        for arg in call.args.iter() {
            let arg_type = self.resolve_type(scope, &arg.rtype);
            let arg_size = self.size_of(&arg_type);
            call_args.push(arg_type);

            // Compile the expression
            self.compile_expression(scope, arg);
            // Write the result of the expression to the stack
            self.instructions.push(asm::Store32(asm::Const(0), STACK_POINTER, RESULT_REG));
            // Increment the stack
            self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER,
                arg_size as u16));
            stack_offset += arg_size as i16;
        }

        // Get the function corresponding to the call
        let function = match scope.get_ident(&call.name, call.span.clone()) {
            FnIdent(ident) => ident,
            VarIdent(..) => fail!("ERROR_EXPECTED_FUNCTION_FOUND_VAR, TODO: Improve this error"),
        };

        // Check that the call args match the function args
        if call_args.len() != function.arg_types.len() {
            fail!("INCORRECT NUMBER OF ARGUMENTS");
        }
        for (call_arg, fn_arg) in call_args.iter().zip(function.arg_types.iter()) {
            self.check_type(call_arg, fn_arg, call.span.clone());
        }

        // Make the call
        self.instructions.push(asm::JumpStore(function.location.clone()));

        // Restore the stack
        self.instructions.push(asm::SubUnsignedValue(STACK_POINTER, STACK_POINTER,
            stack_offset as u16));
    }

    fn compile_let(&mut self, scope: &mut Scope, let_statement: &ast::LetStatement) {
        // Register this variable
        let id = VarIdentId(scope.vars.len());
        scope.add_ident(let_statement.name.clone(), id, let_statement.span);

        let rtype = self.resolve_type(scope, &let_statement.var_type);
        let var = Variable::new(let_statement.clone(), rtype.clone(), Offset(scope.next_offset));
        scope.next_offset += self.size_of(&rtype) as i16;
        scope.vars.push(var);

        // Compile optional assignment
        match let_statement.assignment {
            Some(ref assignment) => self.compile_assign(scope, assignment),
            None => {},
        }
    }

    fn compile_assign(&mut self, scope: &mut Scope, assignment: &ast::Assignment) {
        // Check that the rhs result matches the target
        self.check_type(&self.resolve_type(scope, &assignment.rhs.rtype),
            &self.resolve_type(scope, &assignment.target.rtype),
            assignment.span.clone());

        // Compile the rhs expression and store the result in the location found
        self.compile_expression(scope, &assignment.rhs);

        let target_span = assignment.target.span.clone();
        match *assignment.target.expr {
            // Assignment to an ordinary variable
            ast::VariableExpr(ref name) => {
                match self.get_var_location(scope, name, target_span) {
                    Label(label) => {
                        self.instructions.push(asm::Store32(asm::Unknown(label), ZERO_REG,
                            RESULT_REG));
                    },
                    Offset(amount) => {
                        self.instructions.push(asm::Store32(asm::Const(amount), FRAME_POINTER,
                            RESULT_REG));
                    },
                    Register(id) => {
                        self.instructions.push(asm::AddSigned(id, RESULT_REG, ZERO_REG));
                    },
                }
            }
            // Assignment to a dereference of a pointer
            ast::DerefExpr(ref inner) => {
                // Store the result from evaluating the RHS in a temp register
                self.instructions.push(asm::AddSigned(TEMP_REG, RESULT_REG, ZERO_REG));
                // Evaluate the target address
                self.compile_expression(scope, inner);
                // Then copy the result to the location that was dereferenced
                self.instructions.push(asm::Store32(asm::Const(0), RESULT_REG, TEMP_REG));
            },

            _ => {
                self.logger.report_error(format!("illegal left-hand side expression"),
                    assignment.target.span.clone());
                self.fatal_error();
            },
        }
    }

    /// Check that a type is the same as the expected type or one path never returns
    fn check_type(&self, input: &Type, expected: &Type, span: InputSpan) {
        if input != &types::Bottom && expected != &types::Bottom && input != expected {
            self.logger.report_error(format!("incorrect type, expected: {}, found: {}",
                expected, input), span);
            self.fatal_error();
        }
    }

    fn size_of(&self, type_: &Type) -> u16 {
        self.type_table.size_of(type_)
    }

    fn resolve_type(&self, scope: &Scope, ast_type: &ast::Type) -> Type {
        self.type_table.resolve_type(scope, ast_type)
    }

    fn get_var_location(&self, scope: &Scope, name: &String, span: InputSpan) -> Location {
        match scope.get_ident(name, span) {
            VarIdent(ident) => ident.location.clone(),
            FnIdent(..) => fail!("EXPECTED_VAR_FOUND_FUNCTION_ERROR, TODO: Improve this error"),
        }
    }
}
