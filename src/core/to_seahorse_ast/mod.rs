mod from_python_ast;
mod transform;

use crate::core::{python_ast as py, seahorse_ast::*, CoreError};
use from_python_ast::*;
use transform::*;

/// Convert a Python program into a Seahorse program
pub fn from_python_ast(python_ast: py::Program) -> Result<Program, CoreError> {
    let program = python_ast.try_into()?;
    let program = TransformPass::new().transform(program)?;

    return Ok(program);
}
