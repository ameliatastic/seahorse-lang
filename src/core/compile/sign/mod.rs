// TODO just throwing everything into mod.rs for now, don't want to deal with keeping things clean
// yet
use crate::core::{
    clean::ast::{self as ca, Expression, ParamObj, Params},
    compile::{
        ast::ExpressionObj,
        build::{Transformation, Transformed},
        builtin as bi,
        namespace::*,
    },
    util::*,
};
use crate::match1;
// LOL I JUST LEARNED THAT I COULD DO THIS INSTEAD OF IMPORTING FROM CRATE
use super::{
    builtin::{pyth::ExprContext, Builtin, BuiltinSource, Python},
    check::{DefinedType, ParamType, Ty, TyName},
};
use quote::quote;
use std::collections::HashMap;

enum Error {
    InvalidBase(Ty),
    EnumAccount,
    InvalidEnumVariant,
    InvalidClassField,
    InvalidClassConstructor,
    AccountConstructor,
    InvalidClassDecorator(ca::ExpressionObj),
    UnsupportedClassDecorator(String),
    DuplicateClassField(String),
}

impl Error {
    fn core(self, loc: &Location) -> CoreError {
        match self {
            Self::InvalidBase(ty) => CoreError::make_raw(
                format!("cannot inherit from \"{:?}\"", ty),
                "Help: inheritance in this version of Seahorse is limited to a few builtin types (Account and Enum). This will change in a future release."
            ),
            Self::EnumAccount => CoreError::make_raw("accounts may not be enums", ""),
            Self::InvalidEnumVariant => CoreError::make_raw(
                "invalid enum variant",
                "Help: `Enum` is a special type in Seahorse - you may only define variants like this:\n\n    variant_name = <unique int>"
            ),
            Self::InvalidClassField => CoreError::make_raw(
                "invalid class field",
                "Help: make sure your field has nothing but a type annotation:\n\n    field_name: Type"
            ),
            Self::InvalidClassConstructor => CoreError::make_raw(
                "invalid class constructor",
                "Help: class constructors (__init__ methods) must be instance methods (have a self parameter), and must return nothing."
            ),
            Self::AccountConstructor => CoreError::make_raw(
                "accounts may not have constructors",
                "Help: new accounts must be created through the Solana system program, try using the Empty.init(...) syntax instead."
            ),
            Self::InvalidClassDecorator(dec) => {
                CoreError::make_raw(
                    format!("\"{:#?}\" is not a valid decorator", dec), 
                    "Decorators must be a string"
                )
            }
            Self::UnsupportedClassDecorator(dec) => {
                CoreError::make_raw(
                    format!("{} is not a supported class decorator", dec),
                    "Hint: Only dataclass is currently supported for classes. Imported paths like \"seahorse.prelude.dataclass\" are not currently supported.",
                )
            }
            Self::DuplicateClassField(field) => {
                CoreError::make_raw(
                    format!("duplicate class field {}", field),
                    "Hint: a field can only be declared in a class once"
                )
            }
        }
        .located(loc.clone())
    }
}

/// The output of the "sign" step. Adds additional information to namespaces to make typechecking
/// easier - each defined object gets a signature that is used to determine its type and the types
/// of associated operations on the object (e.g. indexing into a struct or calling a method).
#[derive(Clone, Debug)]
pub struct SignOutput {
    pub namespace_output: NamespaceOutput,
    pub tree: Tree<Signed>,
}

/// Signatures associated with every object defined in a namespace.
pub type Signed = HashMap<String, Signature>;

/// Signature of an object.
#[derive(Clone, Debug)]
pub enum Signature {
    Constant(Expression),
    Class(ClassSignature),
    Function(FunctionSignature),
    Builtin(Builtin),
}

/// Signature for a Python `class`.
#[derive(Clone, Debug)]
pub enum ClassSignature {
    Struct(StructSignature),
    Enum(EnumSignature),
}

/// Signature for a class that gets treated as a struct.
#[derive(Clone, Debug)]
pub struct StructSignature {
    pub is_account: bool,
    pub is_event: bool,
    pub is_dataclass: bool,
    pub bases: Vec<Ty>,
    pub fields: HashMap<String, Ty>,
    pub methods: HashMap<String, (MethodType, FunctionSignature)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MethodType {
    Instance,
    Static,
}

impl StructSignature {
    pub fn constructor(&self, name: TyName) -> Option<Box<Ty>> {
        let func = self.methods.get("__init__");

        return func.as_ref().map(
            |(_, FunctionSignature { params, returns })| {
                Ty::Function(
                    params.clone(),
                    // Need to transform Python's constructor syntax: `Class(...args)`
                    // to our Rust constructor syntax: `Class::__new__(...args)`
                    Ty::Transformed(
                        Ty::Generic(name, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let (class, args) = match1!(expr.obj, ExpressionObj::Call { function, args } => (function, args));

                            // The __new__ function is defined on the loaded type
                            expr.obj = ExpressionObj::Rendered(quote! {
                                <Loaded!(#class)>::__new__(#(#args),*)
                            });

                            Ok(Transformed::Expression(expr))
                        })
                    ).into()
                ).into()
            }
        );
    }
}

/// Signature for a class that gets treated as an enum.
#[derive(Clone, Debug)]
pub struct EnumSignature {
    pub variants: Vec<(String, ())>,
}

/// Signature for a function.
#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub params: Vec<(String, Ty, ParamType)>,
    pub returns: Ty,
}

impl Tree<Signed> {
    /// Correct typenames.
    fn correct(&self, ty: Ty) -> Ty {
        match ty {
            Ty::Generic(name, params) => {
                let name = match name {
                    TyName::Defined(path, _) => {
                        let signature = self.get_leaf_ext(&path);

                        let defined = match signature.unwrap() {
                            Signature::Class(ClassSignature::Struct(StructSignature {
                                is_account: true,
                                ..
                            })) => DefinedType::Account,
                            Signature::Class(ClassSignature::Struct(StructSignature {
                                is_event: true,
                                ..
                            })) => DefinedType::Event,
                            Signature::Class(ClassSignature::Enum(..)) => DefinedType::Enum,
                            _ => DefinedType::Struct,
                        };

                        TyName::Defined(path, defined)
                    }
                    name => name,
                };

                Ty::Generic(
                    name,
                    params.into_iter().map(|ty| self.correct(ty)).collect(),
                )
            }
            ty => ty,
        }
    }
}

impl TryFrom<NamespaceOutput> for SignOutput {
    type Error = CoreError;

    fn try_from(namespace_output: NamespaceOutput) -> CResult<Self> {
        // Runs in two passes:
        // 1. collects most of the signature info, but naively puts every non-builtin type under
        //    `TyName::Defined`.
        // 2. corrects the type names to include `TyName::DefinedAccount` as well.

        let raw_tree = namespace_output
            .tree
            .clone()
            .map_with_path(|namespace, abs| {
                let mut signatures = HashMap::new();

                for (name, export) in namespace.iter() {
                    match export {
                        NamespacedObject::Item(Item::Defined(def)) => {
                            let signature = build_signature(def, abs, &namespace_output.tree)?;
                            signatures.insert(name.clone(), signature);
                        }
                        NamespacedObject::Automatic(builtin)
                        | NamespacedObject::Item(Item::Builtin(builtin)) => {
                            signatures.insert(builtin.name(), Signature::Builtin(builtin.clone()));
                        }
                        _ => {}
                    }
                }

                Ok(signatures)
            })
            .transpose()?;

        let tree = raw_tree.clone().map(|signatures| {
            HashMap::from_iter(signatures.into_iter().map(|(name, signature)| {
                (
                    name,
                    match signature {
                        Signature::Class(ClassSignature::Struct(StructSignature {
                            is_account,
                            is_event,
                            is_dataclass,
                            bases,
                            fields,
                            methods,
                        })) => Signature::Class(ClassSignature::Struct(StructSignature {
                            is_account,
                            is_event,
                            is_dataclass,
                            bases,
                            fields: fields
                                .into_iter()
                                .map(|(name, ty)| (name, raw_tree.correct(ty)))
                                .collect(),
                            methods,
                        })),
                        Signature::Function(FunctionSignature { params, returns }) => {
                            Signature::Function(FunctionSignature {
                                params: params
                                    .into_iter()
                                    .map(|(name, ty, is_required)| {
                                        (name, raw_tree.correct(ty), is_required)
                                    })
                                    .collect(),
                                returns: raw_tree.correct(returns),
                            })
                        }
                        signature => signature,
                    },
                )
            }))
        });

        Ok(SignOutput {
            namespace_output,
            tree,
        })
    }
}

fn build_signature(
    def: &ca::TopLevelStatement,
    // TODO: fairly common pattern to pass around an absolute path to a leaf within a tree + its
    // root, might be cleaner to turn that into a context struct and make the build_ functions
    // member functions
    abs: &Vec<String>,
    root: &Tree<Namespace>,
) -> CResult<Signature> {
    let Located(loc, obj) = def;

    match obj {
        ca::TopLevelStatementObj::Constant { value, .. } => Ok(Signature::Constant(value.clone())),
        ca::TopLevelStatementObj::ClassDef {
            body,
            bases,
            decorator_list,
            ..
        } => {
            let mut is_account = false;
            let mut is_enum = false;
            let mut is_event = false;
            let mut bases_ = vec![];
            let mut is_dataclass = false;

            for Located(loc, decorator) in decorator_list.into_iter() {
                match decorator {
                    ca::ExpressionObj::Id(d) if d == "dataclass" => {
                        is_dataclass = true;
                    }
                    ca::ExpressionObj::Id(d) => {
                        return Err(Error::UnsupportedClassDecorator(d.to_string()).core(&loc))
                    }
                    ca::ExpressionObj::Attribute { name, .. } => {
                        return Err(Error::UnsupportedClassDecorator(name.to_string()).core(&loc))
                    }
                    _ => return Err(Error::InvalidClassDecorator(decorator.clone()).core(&loc)),
                }
            }

            for base in bases.iter() {
                let base = root.build_ty(base, abs)?;
                match base {
                    Ty::Generic(
                        TyName::Builtin(bi::Builtin::Prelude(bi::prelude::Prelude::Account)),
                        _,
                    ) => {
                        bases_.push(base.clone());
                        is_account = true;
                    }
                    Ty::Generic(
                        TyName::Builtin(bi::Builtin::Prelude(bi::prelude::Prelude::Enum)),
                        _,
                    ) => {
                        is_enum = true;
                    }
                    Ty::Generic(
                        TyName::Builtin(bi::Builtin::Prelude(bi::prelude::Prelude::Event)),
                        _,
                    ) => {
                        is_event = true;
                    }
                    ty => {
                        return Err(Error::InvalidBase(ty).core(&loc));
                    }
                }
            }

            if is_account && is_enum {
                return Err(Error::EnumAccount.core(loc));
            }

            if is_enum {
                let mut variants = vec![];
                for statement in body.iter() {
                    let Located(loc, obj) = statement;

                    match obj {
                        ca::ClassDefStatementObj::FieldDef { name, ty, value } => {
                            if ty.is_some() || value.is_none() {
                                return Err(Error::InvalidEnumVariant.core(loc));
                            }

                            variants.push((name.clone(), ()));
                        }
                        _ => {
                            todo!();
                        }
                    }
                }

                Ok(Signature::Class(ClassSignature::Enum(EnumSignature {
                    variants,
                })))
            } else {
                let mut fields = HashMap::new();
                let mut fields_ordered = vec![];
                let mut methods = HashMap::new();
                let mut has_ctor = false;

                for statement in body.iter() {
                    let Located(loc, obj) = statement;

                    match obj {
                        ca::ClassDefStatementObj::FieldDef { name, ty, value } => {
                            if value.is_some() || ty.is_none() {
                                return Err(Error::InvalidClassField.core(loc));
                            }

                            let field_ty = root.build_ty(&ty.as_ref().unwrap(), abs)?;

                            let existing_field = fields.insert(name.clone(), field_ty);

                            if existing_field.is_some() {
                                return Err(Error::DuplicateClassField(name.clone()).core(loc));
                            }

                            let ty_expr = ty.as_ref().unwrap().clone();

                            fields_ordered.push((name, ty_expr));
                        }
                        ca::ClassDefStatementObj::MethodDef(ca::FunctionDef {
                            name,
                            params,
                            returns,
                            ..
                        }) => {
                            if params.is_instance_method {
                                if name == "__init__" {
                                    if is_account {
                                        return Err(Error::AccountConstructor.core(loc));
                                    }
                                    if returns.is_some() {
                                        return Err(Error::InvalidClassConstructor.core(loc));
                                    }
                                    has_ctor = true;
                                }

                                methods.insert(
                                    name.clone(),
                                    (
                                        MethodType::Instance,
                                        build_function_signature(params, returns, abs, root)?,
                                    ),
                                );
                            } else {
                                if name == "__init__" {
                                    return Err(Error::InvalidClassConstructor.core(loc));
                                }

                                methods.insert(
                                    name.clone(),
                                    (
                                        MethodType::Static,
                                        build_function_signature(params, returns, abs, root)?,
                                    ),
                                );
                            }
                        }
                    }
                }

                if !has_ctor && is_dataclass {
                    // If there is no constructor for a dataclass, then for typechecking purposes
                    // we add a __init__ function, which takes all fields as params, and returns None

                    // Convert the fields to param objects
                    let param_objs = fields_ordered.iter().map(|(name, ty_expr)| ParamObj {
                        arg: name.to_string(),
                        annotation: ty_expr.clone(),
                    });

                    let params: Params = Params {
                        is_instance_method: true,
                        params: param_objs
                            .map(|p| Located(loc.clone(), p.clone()))
                            .collect::<Vec<Located<ParamObj>>>(),
                    };

                    methods.insert(
                        String::from("__init__"),
                        (
                            MethodType::Instance,
                            build_function_signature(&params, &None, abs, root)?,
                        ),
                    );
                }

                Ok(Signature::Class(ClassSignature::Struct(StructSignature {
                    is_account,
                    is_event,
                    is_dataclass,
                    fields: fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.clone()))
                        .collect::<HashMap<String, Ty>>(),
                    bases: bases_,
                    methods,
                })))
            }
        }
        ca::TopLevelStatementObj::FunctionDef(ca::FunctionDef {
            params, returns, ..
        }) => Ok(Signature::Function(build_function_signature(
            params, returns, abs, root,
        )?)),
        _ => panic!(),
    }
    .map_err(|err: Error| err.core(loc))
}

fn build_function_signature(
    params: &ca::Params,
    returns: &Option<ca::TyExpression>,
    abs: &Vec<String>,
    root: &Tree<Namespace>,
) -> CResult<FunctionSignature> {
    let params = params
        .params
        .iter()
        .map(|Located(_, ca::ParamObj { arg, annotation })| {
            let ty = root.build_ty(annotation, abs)?;
            Ok((arg.clone(), ty, ParamType::Required))
        })
        .collect::<CResult<Vec<_>>>()?;

    let returns = returns
        .as_ref()
        .map(|ty| root.build_ty(ty, abs))
        .unwrap_or(Ok(Ty::Generic(
            TyName::Builtin(bi::python::Python::None.into()),
            vec![],
        )))?;

    return Ok(FunctionSignature { params, returns });
}

pub fn sign(registry: NamespaceOutput) -> Result<SignOutput, CoreError> {
    registry.try_into()
}
