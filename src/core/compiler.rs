use crate::core::{
    clean::clean, compile::compile as _compile, generate::generate, parse::parse,
    preprocess::preprocess, CoreError,
};
use std::{env::current_dir, path::PathBuf};

use super::generate::GenerateOutput;

// TODO rename to something a bit clearer, reusing the name "compile" elsewhere
pub fn compile(
    python_source: String,
    program_name: String,
    working_dir: Option<PathBuf>,
) -> Result<GenerateOutput, CoreError> {
    let working_dir = match working_dir {
        Some(dir) => dir,
        None => {
            current_dir().map_err(|_| CoreError::make_raw("Could not get current directory", ""))?
        }
    };

    let parsed = parse(python_source.clone())?;
    let cleaned = clean(parsed, python_source)?;
    let preprocessed = preprocess(cleaned, working_dir)?;
    let compiled = _compile(preprocessed)?;
    let generated = generate(compiled, program_name)?;

    return Ok(generated);
}
