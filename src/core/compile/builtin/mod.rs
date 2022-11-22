use crate::core::{compile::check::*, util::*};

pub mod prelude;
pub mod pyth;
pub mod python;
pub use prelude::Prelude;
pub use pyth::Pyth;
pub use python::Python;

/// A trait to help keep the API consistent.
///
/// Represents a single source of builtin objects.
pub trait BuiltinSource: std::fmt::Debug + Clone + PartialEq {
    /// Get the name of this builtin.
    fn name(&self) -> String;

    /// Get the (raw) type of this builtin - either a `Ty::Type` or a `Ty::Function`.
    fn ty(&self) -> Ty;

    /// Check if an instance of this type can be created with the given type parameters.
    ///
    /// Returns a `Result` type instead of just a bool to allow for more customized error messages.
    fn as_instance(&self, params: &Vec<Ty>) -> CResult<()>;

    /// Get the type of an attribute of this object. Returns two types: (this, attribute).
    fn attr(&self, attr: &String) -> Option<(Ty, Ty)>;

    /// Get a function for this builtin's index operation. Returns two types: (this, indexed).
    fn index(&self) -> Option<(Ty, Ty)>;

    /// Get the type of a static attribute of this object.
    fn static_attr(&self, attr: &String) -> Option<Ty>;

    /// Get the type of a cast from `ty` to an instance of this builtin. Returns two types: (this,
    /// casted). The original (actual) type gets unified with the this type, and the expected type
    /// gets unified with the casted type and returned.
    fn casted(&self, ty: &Ty) -> Option<(Ty, Ty)>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
    Python(Python),
    Prelude(Prelude),
    Pyth(Pyth),
}

impl From<python::Python> for Builtin {
    fn from(builtin: python::Python) -> Self {
        Self::Python(builtin)
    }
}

macro_rules! match_builtin {
    ($self:expr, $builtin:pat, $func:expr) => {
        match $self {
            Self::Python($builtin) => $func,
            Self::Prelude($builtin) => $func,
            Self::Pyth($builtin) => $func,
        }
    };
}

// Impl `BuiltinSource` here for convenience.
impl BuiltinSource for Builtin {
    fn name(&self) -> String {
        match_builtin!(self, builtin, builtin.name())
    }

    fn ty(&self) -> Ty {
        match_builtin!(self, builtin, builtin.ty())
    }

    fn as_instance(&self, params: &Vec<Ty>) -> CResult<()> {
        match_builtin!(self, builtin, builtin.as_instance(params))
    }

    fn attr(&self, attr: &String) -> Option<(Ty, Ty)> {
        match_builtin!(self, builtin, builtin.attr(attr))
    }

    fn index(&self) -> Option<(Ty, Ty)> {
        match_builtin!(self, builtin, builtin.index())
    }

    fn static_attr(&self, attr: &String) -> Option<Ty> {
        match_builtin!(self, builtin, builtin.static_attr(attr))
    }

    fn casted(&self, ty: &Ty) -> Option<(Ty, Ty)> {
        match_builtin!(self, builtin, builtin.casted(ty))
    }
}
