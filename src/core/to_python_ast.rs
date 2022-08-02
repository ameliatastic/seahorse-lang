use crate::core::{python_ast::*, CoreError};
use rustpython_parser::{
    error::{ParseError, ParseErrorType},
    parser,
};

pub fn from_python_source(source: String) -> Result<Program, CoreError> {
    let ast = parser::parse_program(&source).map_err(|error| {
        let ParseError { error, location } = error;
        match error {
            ParseErrorType::EOF => {
                CoreError::make_raw("Python parse error", "Unexpected end of file")
                    .located(location)
            }
            ParseErrorType::ExtraToken(tok) => CoreError::make_raw(
                "Python parse error",
                format!("Unexpected extra token ({})", tok),
            )
            .located(location),
            ParseErrorType::InvalidToken => {
                CoreError::make_raw("Python parse error", "Invalid token").located(location)
            }
            ParseErrorType::UnrecognizedToken(tok, Some(msg)) => CoreError::make_raw(
                "Python parse error",
                format!("Unrecognized token ({}, {})", tok, msg),
            )
            .located(location),
            ParseErrorType::UnrecognizedToken(tok, ..) => CoreError::make_raw(
                "Python parse error",
                format!("Unrecognized token ({})", tok),
            )
            .located(location),
            ParseErrorType::Lexical(err) => {
                CoreError::make_raw("Python parse error", format!("{}", err)).located(location)
            }
        }
    })?;
    return Ok(ast);
}
