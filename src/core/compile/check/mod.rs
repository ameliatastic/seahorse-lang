// TODO just throwing everything into mod.rs for now, don't want to deal with keeping things clean
// yet
use crate::core::compile::builtin::BuiltinSource;
use crate::core::{
    clean::ast,
    compile::{
        ast::*,
        build::Transformation,
        builtin::{prelude::Prelude, python::Python, *},
        namespace::*,
        sign::*,
    },
    preprocess as pre,
    util::*,
};
use crate::match1;
use prelude::ExprContext;
use quote::quote;
use std::collections::HashMap;
use std::mem::replace;

use super::builtin::prelude::Transformed;

enum Error {
    VarNotFound(String),
    ReusedVarInTuple,
    TupleIndexNotInt,
    TupleIndexOutOfRange(usize),
    IndexNotFound(Ty),
    AttrNotFound(Ty, String),
    ArrayConstructorLen,
    IsNotFunction(Ty),
    IsNotTarget,
    Unification(Ty, Ty),
    UnificationConst(u64, u64),
    UnificationBase(TyName, TyName),
    UnificationTyParams(TyName, usize, usize),
    UnificationFunctionParams(usize),
    UnificationCast(Ty, Ty),
    UnificationBiCast(Ty, Ty),
    TooManyFunctionParams(String),
    NamedFunctionParamNotFound(String, String),
    ReusedNamedFunctionParam(String),
    AdditionNotFound(Ty),
    NonU32Exponent,
}

impl Error {
    fn core(self, loc: &Location) -> CoreError {
        match self {
            Self::VarNotFound(var) => {
                CoreError::make_raw(format!("variable \"{}\" not found", var), "")
            }
            Self::ReusedVarInTuple => {
                CoreError::make_raw("variable name used multiple times in tuple assignment", "")
            }
            Self::TupleIndexNotInt => {
                CoreError::make_raw(
                    "tuple indices must be int constants",
                    "Help: in order to ensure type safety (and Rust compatibility), the Seahorse compiler must know exactly what type everything is - this means that tuples can't receive arbitrary indices like in Python."
                )
            }
            Self::TupleIndexOutOfRange(len) => {
                CoreError::make_raw(
                    "tuple index out of range",
                    format!("Help: this tuple has {} elements", len)
                )
            }
            Self::IndexNotFound(t) => {
                CoreError::make_raw(format!("indexing operation not found for type {}", t), "")
            }
            Self::AttrNotFound(t, attr) => {
                CoreError::make_raw(format!("attribute not found: {}.\"{}\"", t, attr), "")
            }
            Self::ArrayConstructorLen => {
                CoreError::make_raw("array constructor len must be a non-negative integer literal", "")
            }
            Self::IsNotFunction(t) => {
                CoreError::make_raw("expression is not a function", format!("found: {}", t))
            }
            Self::IsNotTarget => CoreError::make_raw("expression is not an assignment target", ""),
            Self::Unification(t, u) => {
                CoreError::make_raw(format!("type mismatch - expected {}, found {}", t, u), "")
            }
            Self::UnificationConst(n, m) => {
                CoreError::make_raw(
                    format!("constant mismatch - expected {}, found {}", n, m),
                    "Help: you most likely have two arrays with mismatched sizes."
                )
            }
            Self::UnificationBase(x, y) => {
                CoreError::make_raw(format!("type mismatch - expected an instance of {}, found an instance of {}", x, y), "")
            }
            Self::UnificationTyParams(x, n, m) => {
                CoreError::make_raw(format!("type mismatch - expected an instance of {} with {} type parameters, found an instance with {} type parameters", x, n, m), "")
            }
            Self::UnificationFunctionParams(required) => {
                CoreError::make_raw(format!("type mismatch - expected a function that takes {} args", required), "")
            }
            Self::UnificationCast(t, u) => {
                CoreError::make_raw(format!("could not cast {} as {}", t, u), "")
            }
            Self::UnificationBiCast(t, u) => {
                CoreError::make_raw(format!("could not cast {} as {} (or vice-versa)", t, u), "")
            }
            Self::TooManyFunctionParams(format) => CoreError::make_raw(
                "too many function args",
                format!("Help: this function's parameters should look like {}", format)
            ),
            Self::NamedFunctionParamNotFound(name, format) => CoreError::make_raw(
                format!("keyword arg \"{}\" not found", name),
                format!("Help: this function's parameters should look like {}", format)
            ),
            Self::ReusedNamedFunctionParam(name) => CoreError::make_raw(format!("reused keyword arg \"{}\"", name), ""),
            Self::AdditionNotFound(ty) => CoreError::make_raw(
                format!("can not add two instances of {}", ty),
                "Hint: addition can be performed on numbers, lists, and strings."
            ),
            Self::NonU32Exponent => CoreError::make_raw(
                "non-u32 exponent in integer exponentiation",
                "Hint: in Rust, the pow() operation on integers requires the exponent to be a u32, and Seahorse needs to keep that constraint. If you want a non-integer result, make sure that the left side is an f64:\n\n\tf64(b) ** e"
            )
        }
        .located(loc.clone())
    }
}

/// A parameterized type.
#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    // Reference to another type in the context - like using an anonymous type name. If a `Param`
    // holds a reference to its own type parameter, then it's considered a free variable.
    Param(usize),
    IntParam(usize),
    /// Represents a free variable that hasn't been put into the context yet - context needs to
    /// de-anonymize these types before using it.
    Anonymous(usize),
    Any,
    Never,
    // Type that permits unification with types that can cast to its type
    Cast(Box<Ty>),
    // "Base" type. Most types are just generics with no parameters.
    Generic(TyName, Vec<Ty>),
    Function(Vec<(String, Ty, ParamType)>, Box<Ty>),
    // THE TYPE TYPE
    Type(TyName, Option<Box<Ty>>),
    // Const int - used as a param for `Array`s, not the same as `FreeInt`
    Const(u64),
    // References (by path) to packages and modules
    Path(Vec<String>),
    // Type result of anything that produces a transformation.
    Transformed(Box<Ty>, Transformation),
    // Special types for array constructors - unfortunately, the array constructor signature(s) I
    // wanted to have involved more compiler magic than the type system could handle. Hopefully this
    // will chnage in the future, until now these two types are used to handle the very unique
    // array constructor functions.
    ArrayConstructor1,
    ArrayConstructor2,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParamType {
    Required,
    Optional,
    Variadic,
}

impl Ty {
    pub fn python(builtin: Python, params: Vec<Self>) -> Self {
        Ty::Generic(TyName::Builtin(Builtin::Python(builtin)), params)
    }

    pub fn prelude(builtin: Prelude, params: Vec<Self>) -> Self {
        Ty::Generic(TyName::Builtin(Builtin::Prelude(builtin)), params)
    }

    pub fn pyth(builtin: Pyth, params: Vec<Self>) -> Self {
        Ty::Generic(TyName::Builtin(Builtin::Pyth(builtin)), params)
    }

    pub fn new_function(params: Vec<(&'static str, Ty, ParamType)>, returns: Ty) -> Self {
        Self::Function(
            params
                .into_iter()
                .map(|(name, ty, param_type)| (name.to_string(), ty, param_type))
                .collect(),
            returns.into(),
        )
    }

    /// Create a List[`t`].
    pub fn new_list(t: Ty) -> Self {
        Self::Generic(TyName::Builtin(Builtin::Python(Python::List)), vec![t])
    }

    /// Return whether this type is Python-mutable.
    pub fn is_mut(&self) -> bool {
        match self {
            Ty::Generic(TyName::Builtin(Builtin::Prelude(builtin)), _) => match builtin {
                Prelude::RustFloat
                | Prelude::RustInt(..)
                | Prelude::Pubkey
                | Prelude::Empty
                | Prelude::Clock => false,
                _ => true,
            },
            Ty::Generic(TyName::Builtin(Builtin::Python(builtin)), _) => match builtin {
                Python::Bool | Python::Int | Python::None | Python::Str | Python::Tuple => false,
                _ => true,
            },
            Ty::Generic(
                TyName::Defined(_, DefinedType::Struct | DefinedType::Account | DefinedType::Event),
                _,
            ) => true,
            Ty::Generic(TyName::Defined(_, DefinedType::Enum), _) => false,
            _ => false,
        }
    }

    /// Returns whether this type is Rust-`Copy`.
    pub fn is_copy(&self) -> bool {
        match self {
            Ty::Generic(TyName::Builtin(Builtin::Prelude(builtin)), _) => match builtin {
                Prelude::RustInt(..) | Prelude::RustFloat => true,
                _ => false,
            },
            _ => false,
        }
    }

    /// Returns whether this type is Rust-`Display`.
    pub fn is_display(&self) -> bool {
        match self {
            Ty::Generic(TyName::Builtin(builtin), _) => match builtin {
                Builtin::Python(Python::Str | Python::Bool | Python::None)
                | Builtin::Prelude(Prelude::RustInt(..) | Prelude::RustFloat) => true,
                _ => false,
            },
            Ty::IntParam(_) => true,
            _ => false,
        }
    }

    /// Returns whether this type represents an account.
    /// TODO this function is becoming a mess, could stand to refactor TyName
    /// to include the "is_account" info
    pub fn is_account(&self) -> bool {
        match self {
            Ty::Generic(TyName::Builtin(Builtin::Prelude(builtin)), _) => match builtin {
                Prelude::Account
                | Prelude::Empty
                | Prelude::Program
                | Prelude::Signer
                | Prelude::TokenMint
                | Prelude::TokenAccount
                | Prelude::UncheckedAccount
                | Prelude::Clock => true,
                _ => false,
            },
            Ty::Generic(TyName::Builtin(Builtin::Pyth(Pyth::PriceAccount)), _) => true,
            Ty::Generic(TyName::Defined(_, DefinedType::Account), _) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Param(i) => write!(f, "T#{}", i),
            Ty::IntParam(i) => write!(f, "{{Int}} T#{}", i),
            Ty::Anonymous(i) => write!(f, "?{}", i),
            Ty::Any => write!(f, "any"),
            Ty::Never => write!(f, "never"),
            Ty::Cast(t) => write!(f, "{{Cast}} {}", t),
            Ty::Generic(x, a) => {
                write!(f, "{}", x)?;
                if a.len() > 0 {
                    write!(f, "[")?;

                    let mut comma = false;
                    for param in a.iter() {
                        if comma {
                            write!(f, ", ")?;
                        }

                        write!(f, "{}", param)?;

                        comma = true;
                    }

                    write!(f, "]")?;
                }

                Ok(())
            }
            Ty::Function(params, returns) => {
                write!(f, "(")?;

                let mut comma = false;
                for (name, t, param_type) in params.iter() {
                    if comma {
                        write!(f, ", ")?;
                    }

                    match param_type {
                        ParamType::Required => write!(f, "{}: {}", name, t)?,
                        ParamType::Optional => write!(f, "{}?: {}", name, t)?,
                        ParamType::Variadic => write!(f, "*{}: {}", name, t)?,
                    }

                    comma = true;
                }

                write!(f, ") -> {}", returns)?;

                Ok(())
            }
            Ty::Const(n) => write!(f, "{}", n),
            Ty::Type(x, _) => write!(f, "type:{}", x),
            Ty::Path(path) => {
                write!(f, "module:")?;

                let mut dot = false;
                for part in path.iter().skip(1) {
                    if dot {
                        write!(f, ".{}", part)?;
                    } else {
                        write!(f, "{}", part)?;
                    }

                    dot = true;
                }

                Ok(())
            }
            Ty::Transformed(ty, _) => write!(f, "{}", ty),

            Ty::ArrayConstructor1 => write!(f, "<array constructor>"),
            Ty::ArrayConstructor2 => write!(f, "<array constructor function>"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TyName {
    Defined(Vec<String>, DefinedType),
    Builtin(Builtin),
}

#[derive(Clone, Debug, PartialEq)]
pub enum DefinedType {
    Struct,
    Account,
    Enum,
    Event,
}

impl std::fmt::Display for TyName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Defined(path, _) => {
                let mut dot = false;
                for part in path.iter().skip(1) {
                    if dot {
                        write!(f, ".{}", part)?;
                    } else {
                        write!(f, "{}", part)?;
                    }

                    dot = true;
                }

                Ok(())
            }
            Self::Builtin(builtin) => {
                write!(f, "{}", builtin.name())
            }
        }
    }
}

/// Finalized context for a defined object.
///
/// Includes a type for "directives", top-level special expressions like declare_id. These get
/// typechecked like a normal expression and placed in a `FinalContext::Directives` under the name
/// "" in the `Checked` output.
#[derive(Clone, Debug)]
pub enum FinalContext {
    Constant(TypecheckOutput),
    Class(HashMap<String, TypecheckOutput>),
    Function(TypecheckOutput),
    Directives(Vec<(ast::Expression, TypecheckOutput)>),
}

/// Typechecking context output for a single function.
///
/// The `ty_order` collects `Ty` objects for each expression, in depth-first search order.
/// Basically, as you go down the AST in reading order, the list mirrors the order that you will
/// discover each expression. The corresponding element is the expression's type.
#[derive(Clone, Debug)]
pub struct TypecheckOutput {
    pub types: Vec<Ty>,
    pub expr_order: Vec<Ty>,
    pub assign_order: Vec<Assign>,
}

#[derive(Clone, Debug)]
pub enum Assign {
    Mutate,
    Declare {
        undeclared: Vec<String>,
        target: Target,
    },
}

#[derive(Clone, Debug)]
pub enum Target {
    Var(String),
    Tuple(Vec<Target>),
}

/// Optionally convert an expression into targets for variable assignment.
fn as_assignment_target(expression: &ast::Expression) -> Option<Target> {
    // TODO check names for validity here (can't use any Rust keywords for instance)?
    let Located(_, expression) = expression;

    match expression {
        ast::ExpressionObj::Id(var) => Some(Target::Var(var.clone())),
        ast::ExpressionObj::Tuple(tuple) => Some(Target::Tuple(
            tuple
                .iter()
                .map(|element| as_assignment_target(element))
                .collect::<Option<Vec<_>>>()?,
        )),
        _ => None,
    }
}

/// Small structure to hold the result of an arg ordered via `args_order`.
pub enum OrderedArg<'a> {
    Pos(&'a ast::Expression),
    Var(Vec<&'a ast::Expression>),
    Kw(Option<&'a ast::Expression>),
}

/// Reorder `Args` into the order specified by a list of params.
pub fn order_args<'a>(
    args: &'a ast::Args,
    params: &Vec<(String, Ty, ParamType)>,
    loc: &Location,
) -> CResult<Vec<OrderedArg<'a>>> {
    let params_format = format!("{}", Ty::Function(params.clone(), Ty::Any.into()))
        .split("->")
        .next()
        .unwrap()
        .to_string();

    let mut pos_i = 0;
    let mut kw = HashMap::new();
    for (name, arg) in args.kw.iter() {
        if kw.contains_key(name) {
            return Err(Error::ReusedNamedFunctionParam(name.clone()).core(&arg.0));
        }

        kw.insert(name.clone(), arg);
    }

    let mut order = vec![];
    for (name, _, param_type) in params.iter() {
        // Python function params have the following form:
        // [pos1, pos2, ..., posN, variadic?, kw1, kw2, ..., kwN]
        // (Actually it's a bit more complicated than that but this is what Seahorse allows.)
        match param_type {
            ParamType::Required => {
                if pos_i < args.pos.len() {
                    order.push(OrderedArg::Pos(&args.pos[pos_i]));
                    pos_i += 1;
                } else {
                    if let Some(arg) = kw.remove(name) {
                        order.push(OrderedArg::Kw(Some(arg)));
                    } else {
                        return Err(
                            Error::NamedFunctionParamNotFound(name.clone(), params_format)
                                .core(loc),
                        );
                    }
                }
            }
            ParamType::Optional => {
                if pos_i < args.pos.len() {
                    order.push(OrderedArg::Pos(&args.pos[pos_i]));
                    pos_i += 1;
                } else {
                    if let Some(arg) = kw.remove(name) {
                        order.push(OrderedArg::Kw(Some(arg)));
                    } else {
                        order.push(OrderedArg::Kw(None));
                    }
                }
            }
            ParamType::Variadic => {
                // Variadics just consume the remaining positional args
                let mut variadic = vec![];
                while pos_i < args.pos.len() {
                    variadic.push(&args.pos[pos_i]);
                    pos_i += 1;
                }
                order.push(OrderedArg::Var(variadic));
            }
        }
    }

    if pos_i < args.pos.len() || kw.len() > 0 {
        return Err(Error::TooManyFunctionParams(params_format).core(loc));
    }

    return Ok(order);
}

/// Context for typechecking a function body.
pub struct Context<'a> {
    sign_output: &'a SignOutput,
    abs: &'a Vec<String>,
    namespace: &'a Namespace,

    types: Vec<Ty>,
    scopes: Vec<HashMap<String, usize>>,
    returns: usize,
    expr_order: Vec<Ty>,
    assign_order: Vec<Assign>,
}

impl From<Context<'_>> for TypecheckOutput {
    fn from(context: Context<'_>) -> Self {
        let expr_order = context
            .expr_order
            .iter()
            .map(|ty| context.finalize(ty.clone()))
            .collect();

        return Self {
            types: context.types,
            expr_order,
            assign_order: context.assign_order,
        };
    }
}

impl<'a> Context<'a> {
    fn new(sign_output: &'a SignOutput, abs: &'a Vec<String>) -> Self {
        Self {
            sign_output,
            abs,
            namespace: sign_output.namespace_output.tree.get_leaf(abs).unwrap(),
            types: vec![],
            scopes: vec![],
            returns: 0,
            expr_order: vec![],
            assign_order: vec![],
        }
    }

    /// Typecheck a function from start to finish.
    fn typecheck_func(
        func: &ast::FunctionDef,
        signature: &FunctionSignature,
        sign_output: &'a SignOutput,
        abs: &'a Vec<String>,
        this: Option<Ty>,
    ) -> CResult<TypecheckOutput> {
        let mut context = Self::new(sign_output, abs);

        context.check_func(func, signature, this)?;

        return Ok(context.into());
    }

    fn typecheck_constant(
        constant: &ast::Expression,
        sign_output: &'a SignOutput,
        abs: &'a Vec<String>,
    ) -> CResult<TypecheckOutput> {
        let mut context = Self::new(sign_output, abs);

        let ty = Ty::Param(context.free());
        context.check_expr(ty, constant)?;

        return Ok(context.into());
    }

    /// Add a new free variable, returning the new type parameter.
    fn free(&mut self) -> usize {
        let param = self.types.len();
        self.types.push(Ty::Param(param));
        return param;
    }

    /// Add a new type, returning the associated parameter.
    fn new_ty(&mut self, t: Ty) -> usize {
        let i = self.free();
        self.types[i] = t;
        return i;
    }

    /// Get the base type of a type parameter (recursively).
    fn base(&self, i: usize) -> Ty {
        match &self.types[i] {
            Ty::Param(j) if i == *j => Ty::Param(i),
            Ty::IntParam(j) if i == *j => Ty::IntParam(i),
            Ty::Param(j) => self.base(*j),
            Ty::IntParam(j) => self.base(*j),
            t => t.clone(),
        }
    }

    /// Turn a group of anonymous free variables into contextualized free variables.
    ///
    /// Anonymouse free variables can come back from any call that might use generics - class
    /// methods, for instance. Builtins implement generics by using anonymous free variables - for
    /// example, the builtin for List.append returns two types, one that unifies to the caller and
    /// one that represents the function itself. In this case, the caller is List[Anon(0)] and the
    /// function is fn(Anon(0)) -> None. During deanonymization, the two instances of Anon(0) become
    /// the same free type, effectively making the method signature List[T].append = fn(T) -> None.
    fn deanonymize(&mut self, ty: Ty) -> Ty {
        let base = self.types.len();
        self._deanonymize(ty, base)
    }

    fn _deanonymize(&mut self, t: Ty, base: usize) -> Ty {
        match t {
            Ty::Anonymous(i) => {
                while self.types.len() <= base + i {
                    self.free();
                }

                Ty::Param(base + i)
            }
            Ty::Cast(t) => Ty::Cast(self._deanonymize(*t, base).into()),
            Ty::Generic(x, a) => Ty::Generic(
                x,
                a.into_iter().map(|t| self._deanonymize(t, base)).collect(),
            ),
            Ty::Function(params, returns) => Ty::Function(
                params
                    .into_iter()
                    .map(|(name, t, is_required)| (name, self._deanonymize(t, base), is_required))
                    .collect(),
                self._deanonymize(*returns, base).into(),
            ),
            Ty::Transformed(ty, transformation) => {
                Ty::Transformed(self._deanonymize(*ty, base).into(), transformation)
            }
            Ty::Type(name, Some(constructor)) => {
                Ty::Type(name, Some(self._deanonymize(*constructor, base).into()))
            }
            t => t,
        }
    }

    fn deanonymize_pair(&mut self, tys: (Ty, Ty)) -> (Ty, Ty) {
        let (t, u) = tys;
        let base = self.types.len();
        (self._deanonymize(t, base), self._deanonymize(u, base))
    }

    /// Make a parameterized type into a finalized type, if possible.
    fn finalize(&self, t: Ty) -> Ty {
        match t {
            Ty::Param(i) if !self.is_free(i) => self.finalize(self.base(i)),
            Ty::IntParam(i) if !self.is_free(i) => self.finalize(self.base(i)),
            Ty::Anonymous(i) => Ty::Anonymous(i),
            Ty::Never => Ty::Never,
            Ty::Cast(t) => self.finalize(*t).into(),
            Ty::Generic(x, a) => Ty::Generic(x, a.into_iter().map(|t| self.finalize(t)).collect()),
            Ty::Function(p, t) => Ty::Function(
                p.into_iter()
                    .map(|(name, t, is_required)| (name, self.finalize(t), is_required))
                    .collect(),
                self.finalize(*t).into(),
            ),
            Ty::Transformed(ty, transformation) => {
                Ty::Transformed(self.finalize(*ty).into(), transformation)
            }
            t => t,
        }
    }

    /// Find the location, if any, of a variable in scope. Returns the scope level that the variable
    /// is stored in.
    fn find_var(&self, var: &String) -> Option<usize> {
        for (i, scope) in self.scopes.iter().enumerate() {
            if scope.contains_key(var) {
                return Some(i);
            }
        }

        return None;
    }

    /// Get the yet-to-be declared variables from a target.
    fn get_undeclared(&self, target: &Target, loc: &Location) -> CResult<Vec<String>> {
        match target {
            Target::Var(var) => match self.find_var(var) {
                None => Ok(vec![var.clone()]),
                Some(..) => Ok(vec![]),
            },
            Target::Tuple(tuple) => {
                let mut undeclared = vec![];
                for target in tuple.iter() {
                    let mut part = self.get_undeclared(target, loc)?;
                    if part.iter().any(|var| undeclared.contains(var)) {
                        return Err(Error::ReusedVarInTuple.core(loc));
                    }

                    undeclared.append(&mut part);
                }

                Ok(undeclared)
            }
        }
    }

    /// Declare a target, creating a new type parameter for it.
    ///
    /// Also builds a list of the variable names that get declared by this
    /// target. Declarations can happen due to the creation of a new free type,
    /// or the modification of an existing type (according to the scoping rules).
    fn declare_target(
        &mut self,
        target: &Target,
        force_declare: bool,
        declarations: &mut Vec<String>,
    ) -> CResult<usize> {
        match target {
            Target::Var(var) => {
                if !force_declare {
                    if let Some(i) = self.find_var(var) {
                        if i < self.scopes.len() - 1 {
                            return Ok(*self.scopes[i].get(var).unwrap());
                        }
                    }
                }

                let param = self.free();
                declarations.push(var.clone());
                self.scopes.last_mut().unwrap().insert(var.clone(), param);

                Ok(param)
            }
            Target::Tuple(tuple) => {
                let params = tuple
                    .iter()
                    .map(|target| {
                        Ok(Ty::Param(self.declare_target(
                            target,
                            force_declare,
                            declarations,
                        )?))
                    })
                    .collect::<Result<Vec<_>, CoreError>>()?;

                let param = self.new_ty(Ty::python(Python::Tuple, params));

                Ok(param)
            }
        }
    }

    /// Get the type of an attribute of a type.
    fn attr(&mut self, t: Ty, attr: &String) -> Option<(Ty, Ty)> {
        match t.clone() {
            Ty::Param(i) => self.attr(self.types[i].clone(), attr),
            Ty::IntParam(i) => self.attr(self.types[i].clone(), attr),
            Ty::Generic(t, _) => match t {
                TyName::Builtin(x) => x.attr(attr),
                TyName::Defined(path, DefinedType::Struct | DefinedType::Enum) => self.defined_attr(&path, attr),
                TyName::Defined(path, DefinedType::Event) => self.defined_attr(&path, attr).or_else(|| {
                    match attr.as_str() {
                        "emit" => Some((
                            Ty::Anonymous(0),
                            Ty::new_function(
                                vec![],
                                Ty::Transformed(
                                    Box::new(Ty::Never),
                                    Transformation::new(|mut expr| {
                                        let event = match1!(expr.obj, ExpressionObj::Call { function, .. } => *function);
                                        let event = match1!(event.obj, ExpressionObj::Attribute { value, .. } => *value);

                                        expr.obj = ExpressionObj::Rendered(quote! {
                                            #event.__emit__();
                                        });

                                        Ok(Transformed::Expression(expr))
                                    })
                                )
                            )
                        )),
                        _ => None
                    }
                }),
                TyName::Defined(path, DefinedType::Account) => self.defined_attr(&path, attr).or_else(|| {
                    match attr.as_str() {
                        "key" => Some((
                            Ty::Anonymous(0),
                            Ty::new_function(
                                vec![],
                                Ty::Transformed(
                                    Ty::prelude(Prelude::Pubkey, vec![]).into(),
                                    Transformation::new_with_context(|mut expr, context_stack| {
                                        if !context_stack.has(&ExprContext::Seed) {
                                            // If not in a seed, rewrite account.key() to account.borrow().__account__.key()

                                            let account = match1!(expr.obj, ExpressionObj::Call { function, .. } => *function);
                                            let account = match1!(account.obj, ExpressionObj::Attribute { value, .. } => *value);

                                            // add the .borrow().__account__.key()
                                            let value = ExpressionObj::BorrowImmut(account.into());
                                            let value = ExpressionObj::Attribute {
                                                value: value.into(),
                                                name: String::from("__account__"),
                                            };
                                            let value = ExpressionObj::Call {
                                                function: ExpressionObj::Attribute {
                                                    value: value.into(),
                                                    name: "key".to_string(),
                                                }.into(),
                                                args: vec![]
                                            };
                                            expr.obj = value;
                                        }
                                        Ok(Transformed::Expression(expr))
                                    }, None)
                                )
                            )
                        )),
                        "transfer_lamports" => Some((
                            Ty::Anonymous(0),
                            Ty::new_function(
                                vec![
                                    ("to", Ty::Cast(Ty::prelude(Prelude::Account, vec![]).into()), ParamType::Required),
                                    ("amount", Ty::prelude(Prelude::RustInt(false, 64), vec![]), ParamType::Required)
                                ],
                                Ty::Transformed(
                                    Ty::python(Python::Tuple, vec![]).into(),
                                    Transformation::new(|mut expr| {
                                        let (function, mut args) = match1!(expr.obj, ExpressionObj::Call { function, args, } => (function, args.into_iter()));
                                        let account = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                                        let to = args.next().unwrap();
                                        let amount = args.next().unwrap();

                                        expr.obj = ExpressionObj::Rendered(quote! {
                                            {
                                                let amount = #amount;
                                                **#account.borrow().__account__.to_account_info().try_borrow_mut_lamports().unwrap() -= amount;
                                                **#to.to_account_info().try_borrow_mut_lamports().unwrap() += amount;
                                            }
                                        });

                                        Ok(Transformed::Expression(expr))
                                    })
                                )
                            )
                        )),
                        _ => self.defined_attr(&path, attr)
                    }
                })
            },
            Ty::Type(t, _) => match t {
                TyName::Builtin(x) => x.static_attr(attr).map(|t| (Ty::Anonymous(0), t)),
                TyName::Defined(path, DefinedType::Struct | DefinedType::Enum | DefinedType::Account | DefinedType::Event) => self.defined_static_attr(&path, attr),
            },
            Ty::Path(mut abs) => match self.sign_output.namespace_output.tree.get(&abs) {
                Some(Tree::Leaf(namespace)) => match namespace.get(attr) {
                    Some(NamespacedObject::Automatic(..)) => None,
                    Some(NamespacedObject::Import(import)) => match import {
                        Located(_, ImportObj { path, import_type: ImportType::Symbol, .. }) => {
                            let mut abs = abs.clone();
                            let attr = abs.pop().unwrap();
                            self.attr(Ty::Path(abs), &attr)
                        }
                        Located(_, ImportObj { path, .. }) => {
                            Some((Ty::Anonymous(0), Ty::Path(abs.clone())))
                        }
                    },
                    Some(NamespacedObject::Item(..)) => {
                        match self.sign_output.tree.get_leaf(&abs).unwrap().get(attr) {
                            // wow this got ugly
                            Some(Signature::Constant(expansion)) => {
                                let ty = Ty::Param(self.free());

                                Some((
                                    Ty::Anonymous(0),
                                    self.check_constant(ty, expansion).ok()?
                                ))
                            },
                            Some(Signature::Class(ClassSignature::Struct(StructSignature {
                                is_account: true,
                                ..
                            }))) => Some((
                                Ty::Anonymous(0),
                                Ty::Type(TyName::Defined(abs, DefinedType::Account), None),
                            )),
                            Some(Signature::Class(ClassSignature::Struct(sig @ StructSignature {
                                is_event: true,
                                ..
                            }))) => {
                                Some((
                                    Ty::Anonymous(0),
                                    Ty::Type(
                                        TyName::Defined(abs.clone(), DefinedType::Event),
                                        sig.constructor(TyName::Defined(abs, DefinedType::Event))
                                    )
                                ))
                            }
                            Some(Signature::Class(ClassSignature::Struct(sig @ StructSignature {
                                is_account: false,
                                ..
                            }))) => {
                                Some((
                                    Ty::Anonymous(0),
                                    Ty::Type(
                                        TyName::Defined(abs.clone(), DefinedType::Struct),
                                        sig.constructor(TyName::Defined(abs, DefinedType::Struct))
                                    )
                                ))
                            }
                            Some(Signature::Class(ClassSignature::Enum(..))) => {
                                Some((Ty::Anonymous(0), Ty::Type(TyName::Defined(abs, DefinedType::Enum), None)))
                            }
                            Some(Signature::Function(func)) => Some((
                                Ty::Anonymous(0),
                                Ty::Function(func.params.clone(), func.returns.clone().into()),
                            )),
                            Some(Signature::Builtin(builtin)) => Some((
                                Ty::Anonymous(0),
                                Ty::Type(TyName::Builtin(builtin.clone()), None),
                            )),
                            None => None,
                        }
                    }
                    None => None,
                },
                Some(Tree::Node(package)) => match package.get(attr) {
                    Some(..) => {
                        abs.push(attr.clone());
                        Some((Ty::Anonymous(0), Ty::Path(abs)))
                    }
                    None => None,
                },
                None => None,
            },
            Ty::Transformed(ty, _) => self.attr(*ty, attr),
            Ty::Anonymous(..)
            | Ty::Cast(..)
            | Ty::Function(..)
            | Ty::Const(..)
            | Ty::Any
            | Ty::Never
            | Ty::ArrayConstructor1
            | Ty::ArrayConstructor2 => None,
        }
    }

    fn defined_attr(&mut self, path: &Vec<String>, attr: &String) -> Option<(Ty, Ty)> {
        match self.sign_output.tree.get_leaf_ext(path).unwrap() {
            Signature::Class(ClassSignature::Struct(sig)) => {
                if sig.fields.contains_key(attr) {
                    Some((Ty::Anonymous(0), sig.fields.get(attr).unwrap().clone()))
                } else if let Some((MethodType::Instance, FunctionSignature { params, returns })) =
                    sig.methods.get(attr)
                {
                    Some((
                        Ty::Anonymous(0),
                        Ty::Function(params.clone(), returns.clone().into()),
                    ))
                } else {
                    let mut attr_ty = None;
                    for base in sig.bases.iter() {
                        attr_ty = self.attr(base.clone(), attr);
                        if attr_ty.is_some() {
                            break;
                        }
                    }

                    attr_ty
                }
            }
            Signature::Class(ClassSignature::Enum(..))
            | Signature::Function(..)
            | Signature::Constant(..) => None,
            Signature::Builtin(builtin) => builtin.attr(attr),
        }
    }

    fn defined_static_attr(&self, path: &Vec<String>, attr: &String) -> Option<(Ty, Ty)> {
        self.sign_output
            .tree
            .get_leaf_ext(path)
            .and_then(|signature| match signature {
                Signature::Class(ClassSignature::Struct(sig)) => {
                    if let Some((MethodType::Static, FunctionSignature { params, returns })) =
                        sig.methods.get(attr)
                    {
                        Some((
                            Ty::Anonymous(0),
                            Ty::Function(params.clone(), returns.clone().into()),
                        ))
                    } else {
                        None
                    }
                }
                Signature::Class(ClassSignature::Enum(EnumSignature { variants })) => {
                    if variants.iter().any(|(name, _)| name == attr) {
                        Some((
                            Ty::Anonymous(0),
                            Ty::Generic(TyName::Defined(path.clone(), DefinedType::Enum), vec![]),
                        ))
                    } else {
                        None
                    }
                }
                Signature::Function(..) | Signature::Constant(..) => None,
                Signature::Builtin(builtin) => {
                    builtin.static_attr(attr).map(|t| (Ty::Anonymous(0), t))
                }
            })
    }

    /// Typecheck a function.
    fn check_func(
        &mut self,
        func: &ast::FunctionDef,
        signature: &FunctionSignature,
        this: Option<Ty>,
    ) -> CResult<()> {
        let ast::FunctionDef {
            body,
            decorator_list,
            ..
        } = func;

        let FunctionSignature {
            params, returns, ..
        } = signature;

        for decorator in decorator_list.iter() {
            let i = self.free();
            self.check_expr(Ty::Param(i), decorator)?;
        }

        let mut scope = HashMap::new();
        if let Some(ty) = this {
            let i = self.new_ty(ty);
            scope.insert("self".to_string(), i);
        }
        for (name, t, ..) in params.iter() {
            let i = self.new_ty(t.clone());
            scope.insert(name.clone(), i);
        }

        let i = self.new_ty(returns.clone());
        self.returns = i;

        self.check_block(body, Some(scope))?;

        return Ok(());
    }

    /// Typecheck a block of statements. Includes an optional parameter for passing in a pre-made
    /// scope.
    fn check_block(
        &mut self,
        block: &Vec<ast::Statement>,
        scope: Option<HashMap<String, usize>>,
    ) -> CResult<()> {
        self.scopes.push(scope.unwrap_or(HashMap::new()));
        for statement in block.iter() {
            self.check_statement(statement)?;
        }
        self.scopes.pop();

        return Ok(());
    }

    /// Typecheck a statement.
    fn check_statement(&mut self, statement: &ast::Statement) -> CResult<()> {
        let Located(loc, obj) = statement;

        match obj {
            ast::StatementObj::Return { value } => {
                let returns = Ty::Param(self.returns);

                match value {
                    Some(value) => {
                        self.check_expr(returns, value)?;
                    }
                    None => {
                        self.unify(
                            Ty::Generic(TyName::Builtin(Python::None.into()), vec![]),
                            returns,
                            loc,
                        )?;
                    }
                }
            }
            ast::StatementObj::Assert { test, msg } => {
                self.check_expr(Ty::python(Python::Bool, vec![]), test)?;
                match msg {
                    Some(msg) => {
                        self.check_expr(Ty::python(Python::Str, vec![]), msg)?;
                    }
                    None => {}
                }
            }
            ast::StatementObj::OpAssign { target, op, value } => {
                let param_op = self.free();

                let loc = &target.0;
                self.check_binop(Ty::Param(param_op), target, op, value, loc, true)?;
            }
            ast::StatementObj::TyAssign { target, ty, value } => {
                match (as_assignment_target(target), value) {
                    (Some(target), Some(value)) => {
                        let assign_i = self.assign_order.len();
                        self.assign_order.push(Assign::Declare {
                            undeclared: vec![],
                            target: target.clone(),
                        });

                        let ty = self
                            .sign_output
                            .namespace_output
                            .tree
                            .build_ty(ty, self.abs)?;

                        self.check_expr(ty.clone(), value)?;

                        let mut declarations = vec![];
                        let param_target =
                            self.declare_target(&target, false, &mut declarations)?;
                        // Infallible
                        if let Assign::Declare {
                            ref mut undeclared, ..
                        } = self.assign_order[assign_i]
                        {
                            *undeclared = declarations;
                        }

                        self.unify(Ty::Param(param_target), ty, loc)?;
                    }
                    (Some(target), None) => {
                        todo!()
                    }
                    (None, _) => return Err(Error::IsNotTarget.core(loc)),
                }
            }
            ast::StatementObj::Assign { target, value } => match as_assignment_target(target) {
                Some(target) => {
                    let assign_i = self.assign_order.len();
                    self.assign_order.push(Assign::Declare {
                        undeclared: vec![],
                        target: target.clone(),
                    });

                    let param_value = self.free();
                    self.check_expr(Ty::Param(param_value), value)?;

                    let mut declarations = vec![];
                    let param_target = self.declare_target(&target, false, &mut declarations)?;
                    // Infallible
                    if let Assign::Declare {
                        ref mut undeclared, ..
                    } = self.assign_order[assign_i]
                    {
                        *undeclared = declarations;
                    }

                    self.unify(Ty::Param(param_target), Ty::Param(param_value), loc)?;
                }
                None => {
                    let i = self.free();
                    self.assign_order.push(Assign::Mutate);
                    self.check_expr(Ty::Param(i), target)?;
                    self.check_expr(Ty::Param(i), value)?;
                }
            },
            ast::StatementObj::ExpressionObj { expression } => {
                let param_expr = self.free();
                self.check_expr(Ty::Param(param_expr), expression)?;
            }
            ast::StatementObj::If { test, body, orelse } => {
                self.check_expr(Ty::python(Python::Bool, vec![]), test)?;
                self.check_block(body, None)?;
                if let Some(orelse) = orelse {
                    self.check_block(orelse, None)?;
                }
            }
            ast::StatementObj::While { test, body } => {
                self.check_expr(Ty::python(Python::Bool, vec![]), test)?;
                self.check_block(body, None)?;
            }
            ast::StatementObj::For { target, iter, body } => {
                let param_iter = self.free();
                self.check_expr(Ty::Param(param_iter), iter)?;

                self.scopes.push(HashMap::new());
                match as_assignment_target(target) {
                    Some(target) => {
                        let param_target = self.declare_target(&target, true, &mut vec![])?;
                        self.assign_order.push(Assign::Declare {
                            undeclared: vec![],
                            target,
                        });
                        self.unify(Ty::Param(param_target), Ty::Param(param_iter), loc)?;
                    }
                    None => {
                        return Err(Error::IsNotTarget.core(loc));
                    }
                }
                let scope = self.scopes.pop().unwrap();

                self.check_block(body, Some(scope))?;
            }
            ast::StatementObj::Break | ast::StatementObj::Continue | ast::StatementObj::Pass => {}
        }

        return Ok(());
    }

    /// Typecheck an expression that needs to unify to type `expr_ty`. Based on the expression, might
    /// impose additional constraints on the underlying type of `expr_ty`.
    fn check_expr(&mut self, expr_ty: Ty, expression: &ast::Expression) -> CResult<usize> {
        let param = self.free();
        let expr_i = self.expr_order.len();
        self.expr_order.push(Ty::Param(param));

        let Located(loc, obj) = expression;
        let expr_ty = match obj {
            ast::ExpressionObj::BinOp { left, op, right } => {
                self.check_binop(expr_ty, &*left, op, &*right, loc, false)?
            }
            ast::ExpressionObj::Index { value, index } => {
                let i = self.free();
                self.check_expr(Ty::Param(i), &*value)?;

                let u = self.base(i);
                match &u {
                    // Tuples can't use the generic indexing operation code because they're weird
                    Ty::Generic(TyName::Builtin(Builtin::Python(Python::Tuple)), params) => {
                        let Located(loc, obj) = &**index;
                        match obj {
                            ast::ExpressionObj::Int(n) => {
                                let n = *n;

                                let index_i = self.free();
                                self.types[index_i] = Ty::IntParam(index_i);
                                self.check_expr(Ty::IntParam(index_i), index)?;

                                if n < 0 || n >= params.len() as i128 {
                                    return Err(Error::TupleIndexOutOfRange(params.len()).core(loc));
                                }

                                let ty = self.expr_order[expr_i].clone();
                                self.expr_order[expr_i] = Ty::Transformed(
                                    ty.into(),
                                    Transformation::new(move |expr| {
                                        let tuple = match1!(expr.obj, ExpressionObj::Index { value, .. } => value);

                                        Ok(Transformed::Expression(TypedExpression {
                                            ty: expr.ty,
                                            obj: ExpressionObj::TupleIndex {
                                                tuple,
                                                index: n as usize,
                                            },
                                        }))
                                    }),
                                );

                                params[n as usize].clone()
                            }
                            _ => {
                                return Err(Error::TupleIndexNotInt.core(loc));
                            }
                        }
                    }
                    Ty::Generic(TyName::Builtin(builtin), _) => {
                        let (this, index_op) = builtin
                            .index()
                            .map(|pair| self.deanonymize_pair(pair))
                            .ok_or(
                                Error::IndexNotFound(self.finalize(expr_ty.clone())).core(loc),
                            )?;

                        self.unify(this, u, loc)?;

                        let ty = self.check_call(
                            expr_ty,
                            index_op,
                            &ast::Args {
                                pos: vec![*index.clone()],
                                kw: vec![],
                            },
                            loc,
                        )?;

                        ty
                    }
                    _ => {
                        return Err(Error::IndexNotFound(self.finalize(expr_ty)).core(loc));
                    }
                }
            }
            ast::ExpressionObj::UnOp { op, value } => {
                match op {
                    ast::UnaryOperator::Pos | ast::UnaryOperator::Neg | ast::UnaryOperator::Inv => {
                        // TODO also check if the operation is supported
                        self.check_expr(expr_ty.clone(), &*value)?;
                        expr_ty
                    }
                    ast::UnaryOperator::Not => {
                        // TODO can be more permissive by turning this into Cast(Bool) instead
                        self.check_expr(Ty::python(Python::Bool, vec![]), &*value)?;

                        self.unify(expr_ty, Ty::python(Python::Bool, vec![]), loc)?
                    }
                }
            }
            ast::ExpressionObj::Attribute { value, name } => {
                let param_value = self.free();
                self.check_expr(Ty::Param(param_value), &*value)?;

                let (this_ty, attr_ty) = self
                    .attr(Ty::Param(param_value), name)
                    .map(|pair| self.deanonymize_pair(pair))
                    .ok_or(
                        Error::AttrNotFound(
                            self.finalize(self.types[param_value].clone()),
                            name.clone(),
                        )
                        .core(loc),
                    )?;

                self.unify(Ty::Param(param_value), this_ty, loc)?;
                self.unify(expr_ty, attr_ty, loc)?
            }
            ast::ExpressionObj::Call { function, args } => {
                let param_function = self.free();
                self.check_expr(Ty::Param(param_function), &*function)?;

                let ty = self.check_call(expr_ty, self.base(param_function), args, loc)?;

                ty
            }
            ast::ExpressionObj::Ternary { test, body, orelse } => {
                self.check_expr(Ty::python(Python::Bool, vec![]), &**test)?;

                self.check_expr(expr_ty.clone(), &**body)?;
                self.check_expr(expr_ty.clone(), &**orelse)?;

                expr_ty
            }
            ast::ExpressionObj::Int(_) => {
                let param_n = self.free();
                self.types[param_n] = Ty::IntParam(param_n);

                self.unify(expr_ty, self.types[param_n].clone(), loc)?;
                Ty::Param(param_n)
            }
            ast::ExpressionObj::Float(_) => {
                self.unify(expr_ty, Ty::prelude(Prelude::RustFloat, vec![]), loc)?
            }
            ast::ExpressionObj::List(list) => {
                let param_element = self.free();
                let list_ty = self.unify(
                    expr_ty,
                    Ty::python(Python::List, vec![Ty::Param(param_element)]),
                    loc,
                )?;

                for element in list.iter() {
                    self.check_expr(Ty::Param(param_element), element)?;
                }

                list_ty
            }
            ast::ExpressionObj::Tuple(tuple) => {
                let tuple_params = (0..tuple.len())
                    .map(|_| Ty::Param(self.free()))
                    .collect::<Vec<_>>();
                let tuple_ty = self.unify(
                    expr_ty,
                    Ty::Generic(
                        TyName::Builtin(Builtin::Python(Python::Tuple)),
                        tuple_params.clone(),
                    ),
                    loc,
                )?;

                for (element, t) in tuple.iter().zip(tuple_params.into_iter()) {
                    self.check_expr(t, element)?;
                }

                tuple_ty
            }
            ast::ExpressionObj::Comprehension { element, parts } => {
                // For checking purposes, each nested for loop creates a new scope, then at the end
                // we rewind the scope stack back
                let scope_base = self.scopes.len();
                for part in parts.iter() {
                    match part {
                        ast::ComprehensionPart::For { target, iter } => {
                            let param_iter = self.free();
                            self.check_expr(Ty::Param(param_iter), iter)?;

                            self.scopes.push(HashMap::new());
                            match as_assignment_target(target) {
                                Some(target) => {
                                    // TODO not dry, copy-pasted from StatementObj::For
                                    let param_target =
                                        self.declare_target(&target, true, &mut vec![])?;
                                    self.assign_order.push(Assign::Declare {
                                        undeclared: vec![],
                                        target,
                                    });

                                    self.unify(
                                        Ty::Param(param_target),
                                        Ty::Param(param_iter),
                                        loc,
                                    )?;
                                }
                                None => {
                                    return Err(Error::IsNotTarget.core(loc));
                                }
                            }
                        }
                        ast::ComprehensionPart::If { cond } => {
                            self.check_expr(Ty::python(Python::Bool, vec![]), cond)?;
                        }
                    }
                }

                let param_element = self.free();
                let ty_list = self.unify(
                    expr_ty,
                    Ty::python(Python::List, vec![Ty::Param(param_element)]),
                    loc,
                )?;

                self.check_expr(Ty::Param(param_element), element)?;

                self.scopes.truncate(scope_base);

                ty_list
            }
            ast::ExpressionObj::Str(..) => {
                self.unify(expr_ty, Ty::python(Python::Str, vec![]), loc)?
            }
            ast::ExpressionObj::FStr { parts } => {
                // TODO need to typecheck the parts, they unify to anything so technically it won't
                // matter but the typecheck output needs those types anyway
                for part in parts.iter() {
                    match part {
                        ast::FStrPart::ExpressionObj(expr) => {
                            self.check_expr(Ty::Any, expr)?;
                        }
                        _ => {}
                    }
                }
                self.unify(expr_ty, Ty::python(Python::Str, vec![]), loc)?
            }
            // Two levels of checks for an `Id`:
            // 1. Check to see if a matching variable was declared locally
            // 2. Check to see if a matching name was imported/declared in the namespace
            ast::ExpressionObj::Id(var) => match self.find_var(var) {
                Some(level) => {
                    let param_var = *self.scopes[level].get(var).unwrap();
                    self.unify(expr_ty, Ty::Param(param_var), loc)?
                }
                None => match self.namespace.get(var) {
                    Some(NamespacedObject::Import(Located(loc, import))) => match import {
                        ImportObj {
                            path,
                            import_type: ImportType::Symbol,
                            ..
                        } => {
                            match self.sign_output.tree.get_leaf_ext(path) {
                                Some(Signature::Constant(expansion)) => {
                                    let ty = self.check_constant(expr_ty.clone(), expansion)?;
                                    self.unify(expr_ty, ty, loc)?
                                }
                                Some(Signature::Builtin(builtin)) => {
                                    let ty = self.deanonymize(builtin.ty());
                                    self.unify(expr_ty, ty, loc)?
                                }
                                Some(Signature::Class(ClassSignature::Struct(
                                    StructSignature {
                                        is_account: true, ..
                                    },
                                ))) => self.unify(
                                    expr_ty,
                                    Ty::Type(
                                        TyName::Defined(path.clone(), DefinedType::Account),
                                        None,
                                    ),
                                    loc,
                                )?,
                                Some(Signature::Class(ClassSignature::Struct(
                                    sig @ StructSignature { is_event: true, .. },
                                ))) => self.unify(
                                    expr_ty,
                                    Ty::Type(
                                        TyName::Defined(path.clone(), DefinedType::Event),
                                        sig.constructor(TyName::Defined(
                                            path.clone(),
                                            DefinedType::Event,
                                        )),
                                    ),
                                    loc,
                                )?,
                                Some(Signature::Class(ClassSignature::Struct(
                                    sig @ StructSignature {
                                        is_account: false, ..
                                    },
                                ))) => self.unify(
                                    expr_ty,
                                    Ty::Type(
                                        TyName::Defined(path.clone(), DefinedType::Struct),
                                        sig.constructor(TyName::Defined(
                                            path.clone(),
                                            DefinedType::Struct,
                                        )),
                                    ),
                                    loc,
                                )?,
                                Some(Signature::Class(ClassSignature::Enum(..))) => self.unify(
                                    expr_ty,
                                    Ty::Type(
                                        TyName::Defined(path.clone(), DefinedType::Enum),
                                        None,
                                    ),
                                    loc,
                                )?,
                                Some(Signature::Function(FunctionSignature {
                                    params,
                                    returns,
                                })) => self.unify(
                                    expr_ty,
                                    Ty::Function(params.clone(), returns.clone().into()),
                                    loc,
                                )?,
                                None => {
                                    return Err(Error::VarNotFound(var.clone()).core(loc));
                                }
                            }
                            // TODO get object signature, unify with type
                        }
                        ImportObj { path, .. } => {
                            self.unify(expr_ty, Ty::Path(path.clone()), loc)?
                        }
                    },
                    Some(NamespacedObject::Automatic(..) | NamespacedObject::Item(..)) => {
                        let mut path = self.abs.clone();
                        path.push(var.clone());

                        match self.sign_output.tree.get_leaf_ext(&path) {
                            Some(Signature::Constant(expansion)) => {
                                let ty = self.check_constant(expr_ty.clone(), expansion)?;
                                self.unify(expr_ty, ty, loc)?
                            }
                            Some(Signature::Builtin(builtin)) => {
                                let ty = self.deanonymize(builtin.ty());
                                self.unify(expr_ty, ty, loc)?
                            }
                            Some(Signature::Class(ClassSignature::Struct(StructSignature {
                                is_account: true,
                                ..
                            }))) => self.unify(
                                expr_ty,
                                Ty::Type(TyName::Defined(path.clone(), DefinedType::Account), None),
                                loc,
                            )?,
                            Some(Signature::Class(ClassSignature::Struct(
                                sig @ StructSignature { is_event: true, .. },
                            ))) => self.unify(
                                expr_ty,
                                Ty::Type(
                                    TyName::Defined(path.clone(), DefinedType::Event),
                                    sig.constructor(TyName::Defined(
                                        path.clone(),
                                        DefinedType::Event,
                                    )),
                                ),
                                loc,
                            )?,
                            Some(Signature::Class(ClassSignature::Struct(
                                sig @ StructSignature {
                                    is_account: false, ..
                                },
                            ))) => self.unify(
                                expr_ty,
                                Ty::Type(
                                    TyName::Defined(path.clone(), DefinedType::Struct),
                                    sig.constructor(TyName::Defined(
                                        path.clone(),
                                        DefinedType::Struct,
                                    )),
                                ),
                                loc,
                            )?,
                            Some(Signature::Class(ClassSignature::Enum(..))) => self.unify(
                                expr_ty,
                                Ty::Type(TyName::Defined(path.clone(), DefinedType::Enum), None),
                                loc,
                            )?,
                            Some(Signature::Function(FunctionSignature { params, returns })) => {
                                self.unify(
                                    expr_ty,
                                    Ty::Function(params.clone(), returns.clone().into()),
                                    loc,
                                )?
                            }
                            None => {
                                return Err(Error::VarNotFound(var.clone()).core(loc));
                            }
                        }
                    }
                    None => {
                        return Err(Error::VarNotFound(var.clone()).core(loc));
                    }
                },
            },
            ast::ExpressionObj::Bool(..) => {
                self.unify(expr_ty, Ty::python(Python::Bool, vec![]), loc)?
            }
            ast::ExpressionObj::None => {
                self.unify(expr_ty, Ty::python(Python::None, vec![]), loc)?
            }
            ast::ExpressionObj::Iter { value } => {
                let param_iter = self.free();
                self.check_expr(
                    Ty::Cast(Ty::python(Python::Iter, vec![Ty::Param(param_iter)]).into()),
                    value,
                )?;

                self.unify(expr_ty, Ty::Param(param_iter), loc)?
            }
        };

        self.types[param] = expr_ty;

        return Ok(expr_i);
    }

    /// Check an expression indepedently. Works with a fresh copies of `types`, `scopes`, and
    /// `expr_order` to keep this typecheck independent from everything else.
    fn check_expr_independent(&mut self, expr_ty: Ty, expression: &ast::Expression) -> CResult<Ty> {
        let scopes = replace(&mut self.scopes, vec![]);
        let expr_order = replace(&mut self.expr_order, vec![]);

        let res = self.check_expr(expr_ty, expression)?;
        let ty = self.expr_order[res].clone();

        self.scopes = scopes;
        self.expr_order = expr_order;

        return Ok(ty);
    }

    /// Check a constant expansion.
    fn check_constant(&mut self, expr_ty: Ty, expansion: &ast::Expression) -> CResult<Ty> {
        let ty = self.check_expr_independent(expr_ty, expansion)?;
        let ty = Ty::Transformed(
            ty.into(),
            Transformation::new(|mut expr| {
                let name = expr.obj;
                expr.obj = ExpressionObj::Rendered(quote! { #name !() });
                Ok(Transformed::Expression(expr))
            }),
        );

        return Ok(ty);
    }

    /// Check a (binary) operation that needs to unify to type `t`.
    fn check_binop(
        &mut self,
        expr_ty: Ty,
        left: &ast::Expression,
        op: &ast::Operator,
        right: &ast::Expression,
        loc: &Location,
        assign: bool,
    ) -> CResult<Ty> {
        match op {
            // Most operations just need two types that can cast to each other
            ast::Operator::Add => {
                if !assign {
                    let ty = self.check_binop_sides(left, right, loc)?;

                    let ty = self.unify(expr_ty, ty, loc)?;
                    let mut ty = match ty {
                        Ty::Transformed(ty, _) => *ty,
                        ty => ty,
                    };

                    match &ty {
                        Ty::IntParam(..) => {}
                        Ty::Generic(TyName::Builtin(builtin), _) => match builtin {
                            Builtin::Prelude(Prelude::RustInt(..) | Prelude::RustFloat) => {}
                            Builtin::Python(Python::List) => {
                                ty = Ty::Transformed(
                                    ty.into(),
                                    Transformation::new(|mut expr| {
                                        let (lhs, rhs) = match1!(expr.obj, ExpressionObj::BinOp { left, right, .. } => (*left, *right));

                                        expr.obj = ExpressionObj::Rendered(quote! {
                                            Mutable::new(
                                                #lhs.borrow().iter()
                                                    .chain(#rhs.borrow().iter())
                                                    .map(|elem| elem.clone())
                                                    .collect::<Vec<_>>()
                                            )
                                        });

                                        Ok(Transformed::Expression(expr))
                                    }),
                                );
                            }
                            Builtin::Python(Python::Str) => {
                                ty = Ty::Transformed(
                                    ty.into(),
                                    Transformation::new(|mut expr| {
                                        let (lhs, rhs) = match1!(expr.obj, ExpressionObj::BinOp { left, right, .. } => (*left, *right));

                                        expr.obj = ExpressionObj::Rendered(quote! {
                                            #lhs.clone() + & #rhs
                                        });

                                        Ok(Transformed::Expression(expr))
                                    }),
                                );
                            }
                            _ => {
                                return Err(Error::AdditionNotFound(self.finalize(ty)).core(loc));
                            }
                        },
                        _ => {
                            return Err(Error::AdditionNotFound(self.finalize(ty)).core(loc));
                        }
                    }

                    Ok(ty)
                } else {
                    let param = self.free();
                    self.check_expr(Ty::Param(param), left)?;
                    self.check_expr(Ty::Cast(Ty::Param(param).into()), right)?;

                    // TODO also need to check if the operation is supported
                    self.unify(expr_ty, Ty::Param(param), loc)
                }
            }
            ast::Operator::Sub
            | ast::Operator::Mul
            | ast::Operator::Mod
            | ast::Operator::FloorDiv
            | ast::Operator::LShift
            | ast::Operator::RShift
            | ast::Operator::BitOr
            | ast::Operator::BitAnd
            | ast::Operator::BitXor => {
                if !assign {
                    let ty = self.check_binop_sides(left, right, loc)?;

                    let ty = self.unify(expr_ty, ty, loc)?;
                    Ok(match ty {
                        Ty::Transformed(ty, _) => *ty,
                        ty => ty,
                    })
                } else {
                    let param = self.free();
                    self.check_expr(Ty::Param(param), left)?;
                    self.check_expr(Ty::Cast(Ty::Param(param).into()), right)?;

                    // TODO also need to check if the operation is supported
                    self.unify(expr_ty, Ty::Param(param), loc)
                }
            }
            // Division always turns into floating-point
            ast::Operator::Div => {
                if !assign {
                    let ty = Ty::prelude(Prelude::RustFloat, vec![]);
                    self.check_expr(Ty::Cast(ty.clone().into()), left)?;
                    self.check_expr(Ty::Cast(ty.clone().into()), right)?;

                    self.unify(expr_ty, ty, loc)
                } else {
                    let ty = Ty::prelude(Prelude::RustFloat, vec![]);
                    self.check_expr(ty.clone(), left)?;
                    self.check_expr(Ty::Cast(ty.clone().into()), right)?;

                    self.unify(expr_ty, ty, loc)
                }
            }
            ast::Operator::Pow => {
                self.check_expr(expr_ty.clone(), left)?;
                if self.finalize(expr_ty.clone()) == Ty::prelude(Prelude::RustFloat, vec![]) {
                    self.check_expr(
                        Ty::Cast(Ty::prelude(Prelude::RustFloat, vec![]).into()),
                        right,
                    )?;
                } else {
                    self.check_expr(
                        Ty::Cast(Ty::prelude(Prelude::RustInt(false, 32), vec![]).into()),
                        right,
                    )
                    .map_err(|_| Error::NonU32Exponent.core(loc))?;
                }

                Ok(expr_ty)
            }
            ast::Operator::Eq
            | ast::Operator::NotEq
            | ast::Operator::Lt
            | ast::Operator::Gt
            | ast::Operator::Lte
            | ast::Operator::Gte => {
                self.check_binop_sides(left, right, loc)?;

                self.unify(expr_ty, Ty::python(Python::Bool, vec![]), loc)
            }
            ast::Operator::And | ast::Operator::Or => {
                // TODO can be more permissive by turning this into Cast(Bool) instead
                let param = self.new_ty(Ty::python(Python::Bool, vec![]));
                self.check_expr(Ty::Param(param), left)?;
                self.check_expr(Ty::Param(param), right)?;

                self.unify(expr_ty, self.base(param), loc)
            }
            ast::Operator::In | ast::Operator::NotIn => todo!(),
        }
    }

    fn check_binop_sides(
        &mut self,
        left: &ast::Expression,
        right: &ast::Expression,
        loc: &Location,
    ) -> CResult<Ty> {
        let param_lhs = self.free();
        let param_rhs = self.free();

        let i_lhs = self.check_expr(Ty::Param(param_lhs).into(), left)?;
        let i_rhs = self.check_expr(Ty::Param(param_rhs).into(), right)?;

        let lhs = self.expr_order[i_lhs].clone();
        let rhs = self.expr_order[i_rhs].clone();

        // If rhs casts as lhs, then lhs is more permissive so the whole expression
        // becomes lhs (and vice-versa)
        let ty = if let Ok(rhs) = self.unify(Ty::Cast(lhs.clone().into()), rhs.clone(), loc) {
            self.expr_order[i_rhs] = rhs;
            lhs
        } else if let Ok(lhs) = self.unify(Ty::Cast(rhs.clone().into()), lhs.clone(), loc) {
            self.expr_order[i_lhs] = lhs;
            rhs
        } else {
            return Err(Error::UnificationBiCast(self.finalize(lhs), self.finalize(rhs)).core(loc));
        };

        return Ok(ty);
    }

    /// Typecheck a function call `f(args)` with a return value that needs to unify to type `expr_ty`.
    fn check_call(
        &mut self,
        expr_ty: Ty,
        function_ty: Ty,
        args: &ast::Args,
        loc: &Location,
    ) -> CResult<Ty> {
        let f = match &function_ty {
            Ty::Function(..) => function_ty,
            Ty::Type(_, Some(constructor)) => *constructor.clone(),
            Ty::ArrayConstructor2 => Ty::ArrayConstructor2,
            _ => {
                return Err(Error::IsNotFunction(function_ty.clone()).core(loc));
            }
        };

        match f {
            Ty::Function(params, return_ty) => {
                let order = order_args(args, &params, loc)?;

                for (arg, (_, arg_ty, _)) in order.into_iter().zip(params.into_iter()) {
                    match arg {
                        OrderedArg::Pos(arg) => {
                            self.check_expr(arg_ty, arg)?;
                        }
                        OrderedArg::Var(args) => {
                            for arg in args.into_iter() {
                                self.check_expr(arg_ty.clone(), arg)?;
                            }
                        }
                        OrderedArg::Kw(Some(arg)) => {
                            self.check_expr(arg_ty, arg)?;
                        }
                        OrderedArg::Kw(None) => {}
                    }
                }

                Ok(self.unify(expr_ty, *return_ty, loc)?)
            }
            Ty::ArrayConstructor1 => {
                let elem_param = self.free();
                let params = vec![
                    (
                        "iterable".to_string(),
                        Ty::Cast(Ty::python(Python::Iter, vec![Ty::Param(elem_param)]).into()),
                        ParamType::Required,
                    ),
                    ("len".to_string(), Ty::Any, ParamType::Required),
                ];

                let order = order_args(args, &params, loc)?;

                for (arg, (_, arg_ty, _)) in order.iter().zip(params.into_iter()) {
                    match arg {
                        OrderedArg::Pos(arg) => {
                            self.check_expr(arg_ty, arg)?;
                        }
                        OrderedArg::Var(args) => {
                            for arg in args.into_iter() {
                                self.check_expr(arg_ty.clone(), arg)?;
                            }
                        }
                        OrderedArg::Kw(Some(arg)) => {
                            self.check_expr(arg_ty, arg)?;
                        }
                        OrderedArg::Kw(None) => {}
                    }
                }

                let len = match order.get(1).unwrap() {
                    OrderedArg::Pos(arg) | OrderedArg::Kw(Some(arg)) => match arg.1 {
                        ast::ExpressionObj::Int(n) if n >= 0 => n as u64,
                        _ => {
                            return Err(Error::ArrayConstructorLen.core(loc));
                        }
                    },
                    _ => panic!(),
                };

                let ty = Ty::Transformed(
                    Ty::prelude(Prelude::Array, vec![Ty::Param(elem_param), Ty::Const(len)]).into(),
                    Transformation::new(move |mut expr| {
                        let iterable = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());

                        let len = proc_macro2::Literal::u64_unsuffixed(len);

                        expr.obj = ExpressionObj::Rendered(quote! {
                            Mutable::new(
                                <_ as TryInto<[_; #len]>>::try_into(
                                    #iterable.collect::<Vec<_>>()
                                ).unwrap()
                            )
                        });

                        Ok(Transformed::Expression(expr))
                    }),
                );

                Ok(self.unify(expr_ty, ty, loc)?)
            }
            Ty::ArrayConstructor2 => {
                let elem_param = self.free();
                let params = vec![(
                    "elements".to_string(),
                    Ty::Param(elem_param),
                    ParamType::Variadic,
                )];

                let order = order_args(args, &params, loc)?;

                for (arg, (_, arg_ty, _)) in order.iter().zip(params.into_iter()) {
                    match arg {
                        OrderedArg::Pos(arg) => {
                            self.check_expr(arg_ty, arg)?;
                        }
                        OrderedArg::Var(args) => {
                            for arg in args.into_iter() {
                                self.check_expr(arg_ty.clone(), arg)?;
                            }
                        }
                        OrderedArg::Kw(Some(arg)) => {
                            self.check_expr(arg_ty, arg)?;
                        }
                        OrderedArg::Kw(None) => {}
                    }
                }

                let len = match order.get(0).unwrap() {
                    OrderedArg::Var(elements) => elements.len() as u64,
                    _ => panic!(),
                };

                let ty = Ty::Transformed(
                    Ty::prelude(Prelude::Array, vec![Ty::Param(elem_param), Ty::Const(len)]).into(),
                    Transformation::new(move |mut expr| {
                        let elements = match1!(expr.obj, ExpressionObj::Call { args, .. } => args.into_iter().next().unwrap());
                        let elements =
                            match1!(elements.obj, ExpressionObj::Vec(elements) => elements);

                        expr.obj = ExpressionObj::Rendered(quote! {
                            Mutable::new([#(#elements),*])
                        });

                        Ok(Transformed::Expression(expr))
                    }),
                );

                Ok(self.unify(expr_ty, ty, loc)?)
            }
            _ => {
                return Err(Error::IsNotFunction(expr_ty.clone()).core(loc));
            }
        }
    }

    fn is_free(&self, param: usize) -> bool {
        match &self.types[param] {
            Ty::Param(i) | Ty::IntParam(i) if *i == param => true,
            _ => false,
        }
    }

    fn base_ty(&self, ty: Ty) -> Ty {
        match ty {
            Ty::Param(i) | Ty::IntParam(i) => self.base(i),
            ty => ty,
        }
    }

    /// Unify types.
    ///
    /// A normal unification algorithm will just make two types the same. Seahorse needs to keep a
    /// line between what the user can do and compiler magic, so the two types here actually have
    /// different meanings.
    ///
    /// The "expected" type is the type that something needs to unify to. The "actual" type is the
    /// type that has been recently found. Basically, the expected type has been passed up, while
    /// the actual type has been passed down.
    fn unify(&mut self, ty_expected: Ty, ty_actual: Ty, loc: &Location) -> CResult<Ty> {
        let ty_expected = self.base_ty(ty_expected);
        let ty_actual = self.base_ty(ty_actual);

        // eprintln!("{} unify {}", ty_expected, ty_actual);

        match (ty_expected, ty_actual) {
            // Ignore casts on the wrong side
            // TODO necessary anymore? this was sort of a bandaid fix i think
            (t, Ty::Cast(c)) => self.unify(t, *c, loc),
            // Ignore transforms - transforms in the expected type get dropped, while transforms in
            // the actual type get preserved
            (Ty::Transformed(t, _), u) => self.unify(*t, u, loc),
            (t, Ty::Transformed(u, f)) => Ok(Ty::Transformed(self.unify(t, *u, loc)?.into(), f)),
            // Eliminate free variables
            (Ty::Param(i), Ty::Param(j)) => {
                let t = Ty::Param(j);
                self.types[i] = t.clone();
                Ok(t)
            }
            (Ty::IntParam(i), Ty::IntParam(j)) => {
                let t = Ty::IntParam(j);
                self.types[i] = t.clone();
                Ok(t)
            }
            (Ty::Param(i), t) | (t, Ty::Param(i)) => {
                self.types[i] = t.clone();
                Ok(t)
            }
            (
                u @ Ty::Generic(
                    TyName::Builtin(Builtin::Prelude(
                        prelude::Prelude::RustInt(..) | prelude::Prelude::RustFloat,
                    )),
                    _,
                ),
                Ty::IntParam(i),
            )
            | (
                Ty::IntParam(i),
                u @ Ty::Generic(
                    TyName::Builtin(Builtin::Prelude(
                        prelude::Prelude::RustInt(..) | prelude::Prelude::RustFloat,
                    )),
                    _,
                ),
            ) => {
                self.types[i] = u.clone();
                Ok(u)
            }
            // Handle any/never
            (Ty::Any, t) => Ok(t),
            (t, Ty::Never) => Ok(t),
            // Handle consts
            (Ty::Const(n), Ty::Const(m)) => {
                if n == m {
                    Ok(Ty::Const(n))
                } else {
                    Err(Error::UnificationConst(n, m).core(loc))
                }
            }
            // Match generics
            (Ty::Generic(x, a), Ty::Generic(y, b)) => {
                if x != y {
                    return Err(Error::UnificationBase(x, y).core(loc));
                }
                if a.len() != b.len() {
                    return Err(Error::UnificationTyParams(x, a.len(), b.len()).core(loc));
                }

                let a = a
                    .into_iter()
                    .zip(b.into_iter())
                    .map(|(t, u)| self.unify(t, u, loc))
                    .collect::<Result<Vec<_>, CoreError>>()?;

                Ok(Ty::Generic(x, a))
            }
            // Match functions
            (Ty::Function(p1, r1), Ty::Function(p2, r2)) => {
                let required = p1
                    .iter()
                    .filter(|(_, _, param)| param == &ParamType::Required)
                    .count();

                let mut p = vec![];
                for ((x, t, p1), (_, u, p2)) in p1.into_iter().zip(p2.into_iter()) {
                    // Only adding this functionality so that things like `filter` and `map` work,
                    // so I'm also only going to bother checking to see if all of the "expected"
                    // required params are good
                    if p1 == ParamType::Required && p2 == ParamType::Required {
                        p.push((x, self.unify(t, u, loc)?, p1));
                    }
                }

                if p.len() != required {
                    return Err(Error::UnificationFunctionParams(required).core(loc));
                }

                let r = self.unify(*r1, *r2, loc)?;

                Ok(Ty::Function(p, r.into()))
            }
            (f @ Ty::Function(..), Ty::Type(_, Some(constructor))) => {
                self.unify(f, *constructor, loc)
            }
            // Match casts
            (Ty::Cast(c), t) => self.unify_cast(*c, t, loc),
            (t, u) => {
                return Err(Error::Unification(t, u).core(loc));
            }
        }
    }

    fn unify_cast(&mut self, ty_cast: Ty, ty_actual: Ty, loc: &Location) -> CResult<Ty> {
        let ty_cast = self.base_ty(ty_cast);
        let ty_actual = self.base_ty(ty_actual);

        // eprintln!("{} cast {}", ty_cast, ty_actual);

        match (ty_cast, ty_actual) {
            (Ty::Transformed(c, _), t) => self.unify_cast(*c, t, loc),
            // Equal types = do nothing
            (c, t) if c == t => Ok(t),
            // Anything can cast as `Any`, and `Never` can cast as anything
            // TODO the only place where `Any` is used is in `print`, which needs the underlying
            // type to decide whether to use {} or {:?} - returning casts to `Any` as `Any` ruins
            // that, but it doesn't make sense to not do it - maybe need something smarter here.
            // in general need better handling for any/never
            (c @ Ty::Any, _) | (c, Ty::Never) => Ok(c),
            (c @ Ty::Param(_) | c @ Ty::IntParam(_), t)
            | (c, t @ Ty::Param(_) | t @ Ty::IntParam(_)) => self.unify(c, t, loc),
            (Ty::Generic(x, a), Ty::Generic(y, b)) if x == y => {
                self.unify(Ty::Generic(x, a), Ty::Generic(y, b), loc)
            }
            (c, Ty::Generic(TyName::Builtin(builtin), params)) if builtin.casted(&c).is_some() => {
                let pair = builtin.casted(&c).unwrap();
                let t = Ty::Generic(TyName::Builtin(builtin), params);
                let (this, casted) = self.deanonymize_pair(pair);

                self.unify(t, this, loc)?;
                self.unify(c, casted, loc)
            }
            // Cast defined accounts as Account
            (
                c @ Ty::Generic(TyName::Builtin(Builtin::Prelude(Prelude::Account)), _),
                Ty::Generic(TyName::Defined(_, DefinedType::Account), _),
            ) => Ok(Ty::Transformed(
                c.into(),
                Transformation::new(|mut expr| {
                    let obj = expr.obj;

                    expr.obj = ExpressionObj::Rendered(quote! {
                        #obj.borrow().__account__
                    });

                    Ok(Transformed::Expression(expr))
                }),
            )),
            (
                c @ Ty::Generic(TyName::Builtin(Builtin::Prelude(Prelude::InitAccount)), _),
                Ty::Generic(TyName::Defined(_, DefinedType::Account), _),
            ) => Ok(c.into()),
            (c, t) => Err(Error::UnificationCast(t, c).core(loc)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CheckOutput {
    pub sign_output: SignOutput,
    pub tree: Tree<Checked>,
}

pub type Checked = HashMap<String, FinalContext>;

impl TryFrom<SignOutput> for CheckOutput {
    type Error = CoreError;

    fn try_from(sign_output: SignOutput) -> CResult<Self> {
        let tree = sign_output
            .namespace_output
            .tree
            .clone()
            .map_with_path(|namespace, path| {
                let mut checked = HashMap::new();

                for (_, def) in namespace.iter() {
                    match def {
                        NamespacedObject::Item(Item::Defined(Located(_, def))) => match def {
                            ast::TopLevelStatementObj::Constant { name, value } => {
                                let output = Context::typecheck_constant(value, &sign_output, path)?;
                                checked.insert(name.clone(), FinalContext::Constant(output));
                            }
                            ast::TopLevelStatementObj::FunctionDef(func) => {
                                let signature = sign_output
                                    .tree
                                    .get_leaf(path)
                                    .unwrap()
                                    .get(&func.name)
                                    .unwrap();
                                let signature = match1!(signature, Signature::Function(signature) => signature);

                                let output = Context::typecheck_func(func, signature, &sign_output, path, None)?;
                                checked.insert(func.name.clone(), FinalContext::Function(output));
                            }
                            ast::TopLevelStatementObj::ClassDef { name, body, .. } => {
                                let mut methods = HashMap::new();

                                for Located(_, statement) in body.iter() {
                                    match statement {
                                        ast::ClassDefStatementObj::MethodDef(func) => {
                                            let class_signature = sign_output
                                                .tree
                                                .get_leaf(path)
                                                .unwrap()
                                                .get(name)
                                                .unwrap();
                                            // Only struct classes can define methods
                                            let class_signature = match1!(class_signature, Signature::Class(ClassSignature::Struct(signature)) => signature);

                                            let (method_type, signature) = *class_signature.methods.get(&func.name).as_ref().unwrap();

                                            let this = if method_type == &MethodType::Instance {
                                                let mut full_name = path.clone();
                                                full_name.push(name.clone());

                                                let defined_type = if class_signature.is_event {
                                                    DefinedType::Event
                                                } else {
                                                    DefinedType::Struct
                                                };

                                                Some(Ty::Generic(
                                                    TyName::Defined(full_name, defined_type),
                                                    vec![]
                                                ))
                                            } else {
                                                None
                                            };

                                            let output = Context::typecheck_func(func, signature, &sign_output, path, this)?;
                                            methods.insert(func.name.clone(), output);
                                        }
                                        _ => {}
                                    }
                                }

                                checked.insert(name.clone(), FinalContext::Class(methods));
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }

                // Find the top-level expressions in the module, typecheck them. These are outside
                // the namespace so they need to be located in this ugly way
                let mut directives = vec![];
                let module = sign_output
                    .namespace_output
                    .preprocessed
                    .tree
                    .get_leaf(path)
                    .unwrap();
                match module {
                    pre::Module::Python(module) => {
                        for statement in module.statements.iter() {
                            match &statement.1 {
                                ast::TopLevelStatementObj::Expression(expression) => {
                                    let mut context = Context::new(&sign_output, path);
                                    let ty = Ty::Param(context.free());
                                    context.check_expr(ty, expression)?;

                                    directives.push((expression.clone(), context.into()));
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }

                checked.insert("".to_string(), FinalContext::Directives(directives));

                Ok(checked)
            })
            .transpose()?;

        return Ok(CheckOutput { sign_output, tree });
    }
}

pub fn check(signed: SignOutput) -> CResult<CheckOutput> {
    signed.try_into()
}
