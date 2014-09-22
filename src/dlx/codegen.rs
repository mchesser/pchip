use std::collections::HashMap;

use ast;
use ast::Type;

use dlx::asm;
use dlx::asm::{RegId, SpecialRegId, LabelId, TrapId};
use dlx::asm::Instruction;

use error::{InputPos, InputSpan, Logger};

// Special register that is always 0
static ZERO_REG: RegId = 0;
// Frame pointer register
static FRAME_POINTER: RegId = 30;
// Stack pointer register
static STACK_POINTER: RegId = 14;
// Return address register (set by jal)
static RETURN_REG: RegId = 31;
// Register used for storing the results of computations
static RESULT_REG: RegId = 1;

struct TypeProperty {
    size: u32,
}

struct Function {
    ast: ast::FunctionDeclaration,
    location: LabelId,
}

impl Function {
    fn new(ast: ast::FunctionDeclaration, location: LabelId) -> Function {
        Function {
            ast: ast,
            location: location,
        }
    }
}

#[deriving(Clone)]
enum LabelOrOffset {
    Label(LabelId),
    Offset(i16),
}

struct Variable {
    ast: ast::LetStatement,
    location: LabelOrOffset,
}

impl Variable {
    fn new(ast: ast::LetStatement, location: LabelOrOffset) -> Variable {
        Variable {
            ast: ast,
            location: location,
        }
    }
}

#[deriving(PartialEq, Eq, Hash)]
enum IdentId {
    FnIdentId(uint),
    VarIdentId(uint),
}

enum Ident<'a> {
    FnIdent(&'a Function),
    VarIdent(&'a Variable),
}

impl<'a> Ident<'a> {
    fn rtype(&self) -> ast::Type {
        match *self {
            FnIdent(func) => func.ast.rtype.clone(),
            VarIdent(var) => var.ast.var_type.clone(),
        }
    }
}

struct Scope<'a> {
    functions: Vec<Function>,
    vars: Vec<Variable>,
    next_offset: uint,
    ident_table: HashMap<String, IdentId>,
    loop_ends: Vec<LabelId>,
    end_label: LabelId,
    parent: Option<&'a Scope<'a>>
}

impl<'a> Scope<'a> {
    fn new(end_label: LabelId) -> Scope<'a> {
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
    fn get_ident(&self, ident_name: &String, span: InputSpan) -> Ident {
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


struct CodeData {
    instructions: Vec<Instruction>,
    type_table: HashMap<ast::Type, TypeProperty>,
    label_count: uint,
}

impl CodeData {
    /// Generating a unique label id, by keeping track of the number of labels generated and
    /// appending the label count to the label.
    fn next_unique_label(&mut self) -> LabelId {
        self.label_count += 1;
        format!("L{}", self.label_count - 1)
    }

    /// Compile a global variable
    fn compile_global_var(&mut self, scope: &Scope, var_id: uint) {
        // Add the variable's label
        let label = match scope.vars[var_id].location {
            Label(ref s) => s.clone(),
            Offset(..) => fail!("ICE: Found offset instead of label when compiling global var"),
        };
        self.instructions.push(asm::Label(label));

        // Allocate and initialize the variable
        let rtype = scope.vars[var_id].ast.var_type.clone();
        match scope.vars[var_id].ast.assignment {
            // Initialized variables
            Some(ref expr) => {
                match *expr.expression.expr {
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
                let size = match rtype {
                    ast::Primitive(t) => t.size(),
                    _ => unimplemented!(),
                };
                self.instructions.push(asm::AllocateSpace(size as u32));
            }
        }
    }

    /// Compile a global function. (Are local functions useful?)
    fn compile_global_fn(&mut self, scope: &Scope, fn_id: uint) {
        // Add the functions label
        let label = scope.functions[fn_id].location.clone();
        let span = scope.functions[fn_id].ast.span.clone();
        self.instructions.push(asm::Label(label));

        // Store caller's frame pointer and set current frame pointer
        self.instructions.push(asm::Store32(asm::Const(0), STACK_POINTER, FRAME_POINTER));
        self.instructions.push(asm::AddSigned(FRAME_POINTER, STACK_POINTER, ZERO_REG));

        // Store return location (this should be done by caller)
        self.instructions.push(asm::Store32(asm::Const(4), FRAME_POINTER, RETURN_REG));

        // Create a local scope for this function
        let mut local = Scope::new_with_parent(scope, self.next_unique_label());

        // Register function parameters as local variables
        // The input params are stored in negative offset before the frame pointer with the last
        // param stored at FRAME_POINTER[-1]
        let mut next_param_addr = 0;
        for &(ref name, ref var_type) in scope.functions[fn_id].ast.params.iter().rev() {
            // Get the size of the type, ensuring that it is word aligned
            let size = self.get_type_size(scope, var_type.clone());
            next_param_addr -= size;

            let var = ast::LetStatement {
                name: name.clone(),
                var_type: var_type.clone(),
                assignment: None,
                span: span.clone(),
            };

            let id = VarIdentId(local.vars.len());
            local.add_ident(name.clone(), id, span.clone());
            local.vars.push(Variable::new(var, Offset(next_param_addr as i16)));
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
        let frame_size = local.next_offset as i16;
        *self.instructions.get_mut(reserve_stack_index) =
            asm::AddSignedValue(STACK_POINTER, STACK_POINTER, frame_size);

        self.instructions.push(asm::Label(local.end_label.clone()));

        // Add function return
        self.instructions.push(asm::Load32(RETURN_REG, asm::Const(4), FRAME_POINTER));
        self.instructions.push(asm::AddSigned(STACK_POINTER, FRAME_POINTER, ZERO_REG));
        self.instructions.push(asm::Load32(FRAME_POINTER, asm::Const(0), STACK_POINTER));
        self.instructions.push(asm::JumpR(RETURN_REG));
    }

    fn compile_block(&mut self, scope: &mut Scope, block: &ast::Block) {
        for statement in block.statements.iter() {
            self.compile_expression(scope, statement);
        }
    }

    fn compile_expression(&mut self, scope: &mut Scope, expression: &ast::Expression) {
        match *expression.expr {
            ast::IfExpr(ref inner) => self.compile_if(scope, inner),
            ast::LoopExpr(ref inner) => self.compile_loop(scope, inner),
            ast::CallExpr(ref inner) => self.compile_call(scope, inner),
            ast::Break => {
                match scope.loop_ends.last() {
                    Some(label) => self.instructions.push(asm::Jump(label.clone())),
                    None => fail!("INVALID_BREAK_ERROR, TODO: Improve error message"),
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
                        self.instructions.push(asm::Load32(RESULT_REG,
                            asm::Unknown(label), ZERO_REG));
                    },
                    Offset(amount) => {
                        self.instructions.push(asm::Load32(RESULT_REG,
                            asm::Const(amount), FRAME_POINTER));
                    },
                }
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
        self.compile_expression(scope, &if_statement.condition);
        let else_label = self.next_unique_label();
        let end_label = match if_statement.else_block {
            Some(..) => self.next_unique_label(),
            // If there is no else block, then the end label is equal to the else label
            None => else_label.clone(),
        };

        self.instructions.push(asm::JumpIfZero(RESULT_REG, else_label.clone()));

        // Compile the then block
        self.compile_block(scope, &if_statement.body);

        match if_statement.else_block {
            Some(ref block) => {
                // If there is an else block we need to add a jump from the then block to the
                // end label, and add a label for the else part
                self.instructions.push(asm::Jump(end_label.clone()));
                self.instructions.push(asm::Label(else_label));
                // Then compile the else block
                self.compile_block(scope, block);
            }
            None => {}
        }

        // Add the end label
        self.instructions.push(asm::Label(end_label));
    }

    fn compile_loop(&mut self, scope: &mut Scope, loop_statement: &ast::LoopStatement) {
        let start_label = self.next_unique_label();
        self.instructions.push(asm::Label(start_label.clone()));

        // Add the end label to the loop ends vector, so that it can be used by breaks
        let end_label = self.next_unique_label();
        scope.loop_ends.push(end_label.clone());

        self.compile_block(scope, &loop_statement.body);

        // Add jump to start
        self.instructions.push(asm::Jump(start_label));

        // Add end label
        let end_label = scope.loop_ends.pop().expect("ICE: Missing label after loop");
        self.instructions.push(asm::Label(end_label));
    }

    fn compile_call(&mut self, scope: &mut Scope, call: &ast::FunctionCall) {
        let mut stack_offset = 0;
        for arg in call.args.iter() {
            // Compile the expression
            self.compile_expression(scope, arg);
            // Write the result of the expression to the stack
            self.instructions.push(asm::Store32(asm::Const(0), STACK_POINTER, RESULT_REG));
            self.instructions.push(asm::AddUnsignedValue(STACK_POINTER, STACK_POINTER, 4));
            stack_offset += self.get_type_size(scope, arg.rtype.clone());
        }

        // Check if the name resolves
        let function = match scope.get_ident(&call.name, call.span.clone()) {
            FnIdent(ident) => ident,
            VarIdent(..) => fail!("ERROR_EXPECTED_FUNCTION_FOUND_VAR, TODO: Improve this error"),
        };

        // Make the call
        self.instructions.push(asm::JumpStore(function.location.clone()));

        // Restore the stack
        self.instructions.push(asm::SubUnsignedValue(STACK_POINTER, STACK_POINTER,
            stack_offset as u16));
    }

    fn compile_let(&mut self, scope: &mut Scope, let_statement: &ast::LetStatement) {
        // Reserve an address for this variable
        let offset = scope.next_offset;
        scope.next_offset += self.get_type_size(scope, let_statement.var_type.clone()) as uint;

        // Register this variable
        let id = VarIdentId(scope.vars.len());
        scope.add_ident(let_statement.name.clone(), id, let_statement.span);
        scope.vars.push(Variable::new(let_statement.clone(), Offset(offset as i16)));

        // Compile optional assignment
        match let_statement.assignment {
            Some(ref assignment) => self.compile_assign(scope, assignment),
            None => {},
        }
    }

    fn compile_assign(&mut self, scope: &mut Scope, assignment: &ast::Assignment) {
        // Check if the name resolves
        let target_location =  match scope.get_ident(&assignment.target, assignment.span.clone()) {
            VarIdent(ident) => ident.location.clone(),
            FnIdent(..) => fail!("EXPECTED_VAR_FOUND_FUNCTION_ERROR, TODO: Improve this error"),
        };

        // Compile the expression and store the result in the location found
        self.compile_expression(scope, &assignment.expression);
        match target_location {
            Label(label) => {
                self.instructions.push(asm::Store32(asm::Unknown(label), ZERO_REG, RESULT_REG));
            },
            Offset(amount) => {
                self.instructions.push(asm::Store32(asm::Const(amount), FRAME_POINTER, RESULT_REG));
            }
        }
    }

    fn get_type_size(&self, scope: &Scope, type_value: ast::Type) -> u32 {
        match type_value {
            t @ ast::Primitive(..) => {
                self.type_table[t].size
            },
            ast::VariableType(name) => {
                let var_type = scope.get_ident(&name, InputSpan::invalid()).rtype();
                self.get_type_size(scope, var_type)
            },
            _ => fail!("ICE: User types unsupported"),
        }
    }
}

pub fn codegen(program: ast::Program) -> Vec<Instruction> {
    let mut global = Scope::new("exit".into_string());
    let mut data = CodeData {
        instructions: vec![],
        type_table: HashMap::new(),
        label_count: 0,
    };

    data.type_table.insert(ast::Primitive(ast::IntType), TypeProperty { size: 4 });
    data.type_table.insert(ast::Primitive(ast::UnitType), TypeProperty { size: 0 });

    // Parse globals
    for item in program.items.into_iter() {
        let location = data.next_unique_label();
        match item {
            ast::FunctionItem(fn_item) => {
                let id = FnIdentId(global.functions.len());
                global.add_ident(fn_item.name.clone(), id, fn_item.span.clone());
                global.functions.push(Function::new(fn_item, location));
            },
            ast::StructItem(struct_item) => {
                unimplemented!();
            },
            ast::LetItem(let_item) => {
                let id = VarIdentId(global.vars.len());
                global.add_ident(let_item.name.clone(), id, let_item.span.clone());
                global.vars.push(Variable::new(let_item, Label(location)));
            },
        }
    }

    // Compile global variables
    for i in range(0u, global.vars.len()) {
        data.compile_global_var(&global, i);
    }

    // Compile global functions
    for i in range(0u, global.functions.len()) {
        data.compile_global_fn(&global, i);
    }

    data.instructions
}
