pub mod ast;
pub mod build;
pub mod builtin;
pub mod check;
pub mod namespace;
pub mod sign;

use crate::core::{preprocess as pre, util::*};

pub fn compile(preprocessed: pre::ModuleRegistry) -> Result<build::BuildOutput, CoreError> {
    let namespaced = namespace::namespace(preprocessed)?;
    let signed = sign::sign(namespaced)?;
    let checked = check::check(signed)?;
    let built = build::build(checked)?;

    return Ok(built);
}
