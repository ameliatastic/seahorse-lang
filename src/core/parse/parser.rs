use crate::core::{parse::ast::*, CoreError};
use rustpython_parser::{
    error::{ParseError, ParseErrorType},
    parser,
};

pub fn parse(source: String) -> Result<Program, CoreError> {
    let ast = parser::parse_program(&source).map_err(|error| {
        let ParseError { error, location } = error;
        match error {
            ParseErrorType::EOF => {
                CoreError::make_raw("Python parse error", "Unexpected end of file")
                    .with_loc(location)
            }
            ParseErrorType::ExtraToken(tok) => CoreError::make_raw(
                "Python parse error",
                format!("Unexpected extra token ({})", tok),
            )
            .with_loc(location),
            ParseErrorType::InvalidToken => {
                CoreError::make_raw("Python parse error", "Invalid token").with_loc(location)
            }
            ParseErrorType::UnrecognizedToken(tok, Some(msg)) => CoreError::make_raw(
                "Python parse error",
                format!("Unrecognized token ({}, {})", tok, msg),
            )
            .with_loc(location),
            ParseErrorType::UnrecognizedToken(tok, ..) => CoreError::make_raw(
                "Python parse error",
                format!("Unrecognized token ({})", tok),
            )
            .with_loc(location),
            ParseErrorType::Lexical(err) => {
                CoreError::make_raw("Python parse error", format!("{}", err)).with_loc(location)
            }
        }
    })?;
    return Ok(ast);
}
