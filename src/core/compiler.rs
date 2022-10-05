use crate::core::{
    clean::clean, compile::compile as _compile, generate::generate, parse::parse,
    preprocess::preprocess, CoreError,
};

use super::generate::GenerateOutput;

// TODO rename to something a bit clearer, reusing the name "compile" elsewhere
// TODO when modules get better support, this will either need a file handle or an associated file
// path
pub fn compile(python_source: String, program_name: String) -> Result<GenerateOutput, CoreError> {
    let python_source_ = python_source.clone();

    return compile_steps(python_source, program_name)
        .map_err(|err: CoreError| err.with_context(&python_source_));
}

fn compile_steps(python_source: String, program_name: String) -> Result<GenerateOutput, CoreError> {
    let parsed = parse(python_source)?;
    let cleaned = clean(parsed)?;
    let preprocessed = preprocess(cleaned)?;
    let compiled = _compile(preprocessed)?;
    let generated = generate(compiled, program_name)?;
    return Ok(generated);
}
