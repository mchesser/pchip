use std::{collections::HashMap, fmt};

use crate::{ast, dlx::codegen, error::InputSpan};

pub use self::BaseType::*;
pub use self::Type::*;

type TypeId = usize;

#[derive(Clone)]
pub struct CompositeType {
    pub name: String,
    pub fields: HashMap<String, (u16, Type)>,
    pub size: u16,
}

impl CompositeType {
    /// Create blank type with only a name.
    /// This is useful for referencing an incomplete type with pointers.
    fn blank_type(name: String) -> CompositeType {
        CompositeType { name, fields: HashMap::new(), size: 0 }
    }
}

impl PartialEq for CompositeType {
    fn eq(&self, other: &CompositeType) -> bool {
        // No two unique types can have the same name, so it is sufficient just to compare the
        // names of the types to see if they are the same
        self.name == other.name
    }
}

impl fmt::Debug for CompositeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum BaseType {
    Bool,
    Int,
    Char,
    Unit,
    Composite(Box<CompositeType>),
}

impl BaseType {
    /// Returns the size of the type.
    /// Currently all types must be word aligned.
    pub fn size(&self) -> u16 {
        match *self {
            Bool => 4,
            Int => 4,
            Char => 1,
            Unit => 0,
            Composite(ref tp) => tp.size,
        }
    }
}

/// A resolved type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Normal(TypeId),
    StaticArray(Box<Type>, u16),
    Pointer(Box<Type>),
    Any,
    Bottom,
}

impl Type {
    pub fn deref(&self) -> &Type {
        match self {
            StaticArray(inner, _) => inner,
            Pointer(inner) => inner,
            invalid => panic!("ICE: Attempted to dereference `{:?}`", invalid),
        }
    }
}

pub struct TypeTable {
    type_map: HashMap<ast::Type, usize>,
    types: Vec<BaseType>,
}

impl TypeTable {
    pub fn resolve_type(&self, scope: &codegen::Scope, ast_type: &ast::Type) -> Type {
        match ast_type {
            ast::VariableType(name) => scope.get_ident(name, InputSpan::invalid()).rtype(),
            ast::Pointer(inner) => Pointer(Box::new(self.resolve_type(scope, inner))),
            ast::StaticArrayType(inner, size) => {
                StaticArray(Box::new(self.resolve_type(scope, inner)), *size as u16)
            }
            ast::DerefType(inner) => match self.resolve_type(scope, inner) {
                Pointer(inner) => *inner,
                StaticArray(inner, _) => *inner,
                invalid => {
                    panic!("type `{:?}` cannot be dereferenced", invalid);
                }
            },
            ast::FieldRefType(inner, field_name) => {
                let inner_type = self.resolve_type(scope, inner);
                match self.base_type(&inner_type) {
                    Composite(target_type) => (target_type.fields[field_name].1).clone(),
                    invalid => panic!("type `{:?}` has no field `{:?}`", invalid, field_name),
                }
            }
            ast::Primitive(ast::BottomType) => Bottom,
            ast::Primitive(ast::AnyType) => Any,

            // Otherwise this is a normal variable
            _ => Normal(self.type_map[ast_type]),
        }
    }

    pub fn base_type(&self, type_: &Type) -> &BaseType {
        match type_ {
            Normal(id) => &self.types[*id],
            Pointer(inner) => self.base_type(inner),
            invalid => panic!("There is no base type associated with {:?}", invalid),
        }
    }

    pub fn size_of(&self, type_: &Type) -> u16 {
        align(self.unaligned_size_of(type_))
    }

    pub fn unaligned_size_of(&self, type_: &Type) -> u16 {
        match type_ {
            Normal(id) => self.types[*id].size(),
            StaticArray(inner, size) => self.unaligned_size_of(inner) * size,
            Pointer(..) => 4,
            Bottom => panic!("ICE: Attempted to determine size of bottom type"),
            Any => panic!("ICE: Attempted to determine size of any type"),
        }
    }

    fn add_mapping(&mut self, ast_type: ast::Type, resolved_type: BaseType) {
        let index = self.types.len();
        self.type_map.insert(ast_type, index);
        self.types.push(resolved_type);
    }
}

// Aligns types to words
fn align(size: u16) -> u16 {
    let padding = size % 4;
    if padding != 0 { size + (4 - padding) } else { size }
}

struct TypeGenData<'a> {
    unresolved_map: HashMap<String, (usize, ast::StructDeclaration)>,
    type_table: TypeTable,
    fake_scope: codegen::Scope<'a>,
}

impl<'a> TypeGenData<'a> {
    fn generate_type_table(mut self, struct_list: Vec<String>) -> TypeTable {
        // Iterate through the list of remaining types an resolve them. Some types depend on other
        // types, so we recursively resolve dependencies.
        for current_type in &struct_list {
            let (id, struct_decl) = match self.unresolved_map.get(current_type) {
                // This type is in still in unresolved list so we need to resolve it
                Some(&(id, ref struct_decl)) => (id, struct_decl.clone()),

                // Already resolved this type due to a dependency.
                None => continue,
            };
            let resolved = Composite(Box::new(self.gen_struct_type(&struct_decl)));
            self.type_table.types[id] = resolved;
        }

        self.type_table
    }

    /// FIXME: Can have multiple fields with the same name
    /// FIXME: Infinite sized data structures will result in an infinite loop
    fn gen_struct_type(&mut self, struct_decl: &ast::StructDeclaration) -> CompositeType {
        let mut new_type = CompositeType::blank_type(struct_decl.name.clone());
        let mut next_offset = 0;
        // Loop though all the fields of the struct and resolve their types and offsets
        for &(ref field_name, ref field_type) in &struct_decl.fields {
            let resolved_type = self.gen_type(field_type);

            let field_offset = next_offset;
            next_offset += self.type_table.size_of(&resolved_type);

            new_type.fields.insert(field_name.clone(), (field_offset, resolved_type));
        }

        new_type.size = next_offset;

        // Now that we have resolved this we can remove it from the list of unresolved
        // types.
        self.unresolved_map.remove(&struct_decl.name);
        new_type
    }

    fn full_resolve_type(&mut self, name: &String) -> Type {
        let (id, struct_decl) = match self.unresolved_map.get(name) {
            // This type is in still in unresolved list so we need to resolve it
            Some(&(id, ref struct_decl)) => (id, struct_decl.clone()),

            // Already resolved this type due to a dependency.
            None => {
                return self
                    .type_table
                    .resolve_type(&self.fake_scope, &ast::UserType(name.clone()));
            }
        };
        let resolved = Composite(Box::new(self.gen_struct_type(&struct_decl)));
        self.type_table.types[id] = resolved;
        Normal(id)
    }

    fn gen_type(&mut self, ast_type: &ast::Type) -> Type {
        match ast_type {
            // If this type is a pointer, we don't care if it hasn't been defined yet
            ast::Pointer(inner) => {
                Pointer(Box::new(self.type_table.resolve_type(&self.fake_scope, inner)))
            }

            // If the type is a user defined type, then it must be fully resolved before we can
            // continue
            ast::UserType(name) => self.full_resolve_type(name),

            ast::StaticArrayType(inner, size) => {
                StaticArray(Box::new(self.gen_type(inner)), *size as u16)
            }

            // Primitive types should already be resolved
            primitive @ ast::Primitive(..) => {
                self.type_table.resolve_type(&self.fake_scope, primitive)
            }

            // No other types are valid here
            invalid => panic!("unexpected type: {:?}", invalid),
        }
    }
}

pub fn typegen(program: &ast::Program) -> TypeTable {
    let mut data = TypeGenData {
        unresolved_map: HashMap::new(),
        type_table: TypeTable { type_map: HashMap::new(), types: vec![] },
        fake_scope: codegen::Scope::new("TYPE_ERROR".to_string()),
    };

    // Insert primitive types into the type map
    data.type_table.add_mapping(ast::Primitive(ast::UnitType), Unit);
    data.type_table.add_mapping(ast::Primitive(ast::IntType), Int);
    data.type_table.add_mapping(ast::Primitive(ast::CharType), Char);
    data.type_table.add_mapping(ast::Primitive(ast::BoolType), Bool);

    let mut struct_list = vec![];

    for item in &program.items {
        if let ast::StructItem(ref struct_item) = *item {
            let name = struct_item.name.clone();
            let id = data.type_table.types.len();
            data.unresolved_map.insert(name.clone(), (id, struct_item.clone()));
            data.type_table.type_map.insert(ast::UserType(name.clone()), id);
            data.type_table
                .types
                .push(Composite(Box::new(CompositeType::blank_type(name.clone()))));

            struct_list.push(name);
        }
    }

    data.generate_type_table(struct_list)
}
