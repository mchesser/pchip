use std::collections::HashMap;
use std::fmt;

use ast;
use dlx::codegen;
use error::InputSpan;

type TypeId = uint;

#[deriving(Clone)]
pub struct CompositeType {
    pub name: String,
    pub fields: HashMap<String, (u16, Type)>,
    pub size: u16,
}

impl CompositeType {
    /// Create blank type with only a name.
    /// This is useful for referencing an incomplete type with pointers.
    fn blank_type(name: String) -> CompositeType {
        CompositeType {
            name: name,
            fields: HashMap::new(),
            size: 0,
        }
    }
}

impl PartialEq for CompositeType {
    fn eq(&self, other: &CompositeType) -> bool {
        // No two unique types can have the same name, so it is sufficient just to compare the
        // names of the types to see if they are the same
        self.name == other.name
    }
}

impl fmt::Show for CompositeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[deriving(Show, Clone)]
pub enum BaseType {
    Bool,
    Int,
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
            Unit => 0,
            Composite(ref tp) => tp.size,
        }
    }
}

/// A resolved type
#[deriving(Show, Clone, PartialEq)]
pub enum Type {
    Normal(TypeId),
    StaticArray(Box<Type>, u16),
    Pointer(Box<Type>),
    Any,
    Bottom,
}

impl Type {
    pub fn deref(&self) -> &Type {
        match *self {
            StaticArray(ref inner, _) => &**inner,
            Pointer(ref inner) => &**inner,
            ref invalid => fail!("ICE: Attempted to dereference `{}`", invalid),
        }
    }
}

pub struct TypeTable {
    type_map: HashMap<ast::Type, uint>,
    types: Vec<BaseType>,
}

impl TypeTable {
    pub fn resolve_type(&self, scope: &codegen::Scope, ast_type: &ast::Type) -> Type {
        match *ast_type {
            ast::VariableType(ref name) => scope.get_ident(name, InputSpan::invalid()).rtype(),
            ast::Pointer(ref inner) => Pointer(box self.resolve_type(scope, &**inner)),
            ast::StaticArrayType(ref inner, size) => {
                StaticArray(box self.resolve_type(scope, &**inner), size as u16)
            },
            ast::DerefType(ref inner) => {
                match self.resolve_type(scope, &**inner) {
                    Pointer(inner) => {
                        *inner.clone()
                    },
                    StaticArray(inner, _) => {
                        *inner.clone()
                    },
                    invalid => {
                        fail!("type `{}` cannot be dereferenced", invalid);
                    }
                }
            },
            ast::FieldRefType(ref inner, ref field_name) => {
                let inner_type = self.resolve_type(scope, &**inner);
                match *self.base_type(&inner_type) {
                    Composite(ref target_type) => {
                        (target_type.fields[field_name.clone()].1).clone()
                    },
                    ref invalid => fail!("type `{}` has no field `{}`", invalid, field_name),
                }
            },
            ast::Primitive(ast::BottomType) => Bottom,
            ast::Primitive(ast::AnyType) => Any,

            // Otherwise this is a normal variable
            _ => Normal(self.type_map[ast_type.clone()]),
        }
    }

    pub fn base_type(&self, type_: &Type) -> &BaseType {
        match *type_ {
            Normal(id) => &self.types[id],
            Pointer(ref inner) => self.base_type(&**inner),
            ref invalid => fail!("There is no base type associated with {}", invalid),
        }
    }

    pub fn size_of(&self, type_: &Type) -> u16 {
        match *type_ {
            Normal(id) => self.types[id].size(),
            StaticArray(ref inner, size) => self.size_of(&**inner) * size,
            Pointer(..) => 4,
            Bottom => fail!("ICE: Attempted to determine size of bottom type"),
            Any => fail!("ICE: Attempted to determine size of any type"),
        }
    }

    fn add_mapping(&mut self, ast_type: ast::Type, resolved_type: BaseType) {
        let index = self.types.len();
        self.type_map.insert(ast_type, index);
        self.types.push(resolved_type);
    }
}

struct TypeGenData<'a> {
    unresolved_map: HashMap<String, (uint, ast::StructDeclaration)>,
    type_table: TypeTable,
    fake_scope: codegen::Scope<'a>,
}

impl<'a> TypeGenData<'a> {
    fn generate_type_table(mut self, struct_list: Vec<String>) -> TypeTable {
        // Iterate through the list of remaining types an resolve them. Some types depend on other
        // types, so we recursively resolve dependencies.
        for current_type in struct_list.iter() {
            let (id, struct_decl) = match self.unresolved_map.find(current_type) {
                // This type is in still in unresolved list so we need to resolve it
                Some(&(id, ref struct_decl)) => {
                    (id, struct_decl.clone())
                }

                // Already resolved this type due to a dependency.
                None => continue,
            };
            let resolved = Composite(box self.gen_struct_type(&struct_decl));
            *self.type_table.types.get_mut(id) = resolved;
        }

        self.type_table
    }

    /// FIXME: Can have multiple fields with the same name
    /// FIXME: Infinite sized data structures will result in an infinite loop
    fn gen_struct_type(&mut self, struct_decl: &ast::StructDeclaration) -> CompositeType {
        let mut new_type = CompositeType::blank_type(struct_decl.name.clone());
        let mut next_offset = 0;
        // Loop though all the fields of the struct and resolve their types and offsets
        for &(ref field_name, ref field_type) in struct_decl.fields.iter() {
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

    fn gen_type(&mut self, ast_type: &ast::Type) -> Type {
        match *ast_type {
            // If this type is a pointer, we don't care if it hasn't been defined yet
            ast::Pointer(ref inner) => {
                Pointer(box self.type_table.resolve_type(&self.fake_scope, &**inner))
            },

            // If the type is a user defined type, then it must be defined before we can continue
            ast::UserType(ref name) => {
                let (id, struct_decl) = match self.unresolved_map.find(name) {
                    // This type is in still in unresolved list so we need to resolve it
                    Some(&(id, ref struct_decl)) => (id, struct_decl.clone()),

                    // Already resolved this type due to a dependency.
                    None => {
                        return self.type_table.resolve_type(&self.fake_scope,
                            &ast::UserType(name.clone()));
                    },
                };
                let resolved = Composite(box self.gen_struct_type(&struct_decl));
                *self.type_table.types.get_mut(id) = resolved;
                Normal(id)
            },

            // Primitive types should already be resolved
            ref primitive @ ast::Primitive(..) => {
                self.type_table.resolve_type(&self.fake_scope, primitive)
            },

            // No other types are valid here
            ref invalid => fail!("ERROR unexpected type: {}", invalid),
        }
    }
}


pub fn typegen(program: &ast::Program) -> TypeTable {
    let mut data = TypeGenData {
        unresolved_map: HashMap::new(),
        type_table: TypeTable {
            type_map: HashMap::new(),
            types: vec![],
        },
        fake_scope: codegen::Scope::new("TYPE_ERROR".to_string()),
    };

    // Insert primitive types into the type map
    data.type_table.add_mapping(ast::Primitive(ast::IntType), Int);
    data.type_table.add_mapping(ast::Primitive(ast::BoolType), Bool);
    data.type_table.add_mapping(ast::Primitive(ast::UnitType), Unit);

    let mut struct_list = vec![];

    for item in program.items.iter() {
        match *item {
            ast::StructItem(ref struct_item) => {
                let name = struct_item.name.clone();
                let id = data.type_table.types.len();
                data.unresolved_map.insert(name.clone(), (id, struct_item.clone()));
                data.type_table.type_map.insert(ast::UserType(name.clone()), id);
                data.type_table.types.push(Composite(box CompositeType::blank_type(name.clone())));

                struct_list.push(name);
            },
            _ => {},
        }
    }

    data.generate_type_table(struct_list)
}
