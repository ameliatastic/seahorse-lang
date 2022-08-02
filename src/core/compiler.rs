use crate::core::{
    python_ast::Location, to_python_ast::from_python_source, to_rust_source::from_seahorse_ast,
    to_seahorse_ast::from_python_ast,
};
use owo_colors::OwoColorize;
use std::{error, fmt};

#[derive(Debug, Clone)]
pub enum CoreError {
    Raw {
        message: (String, String),
        location: Option<Location>,
    },
    WithContext {
        message: (String, String),
        location: Option<Location>,
        context: Option<String>,
    },
}

impl CoreError {
    pub fn located(self, location: Location) -> Self {
        match self {
            Self::Raw { message, .. } => Self::Raw {
                message,
                location: Some(location),
            },
            _ => panic!("CoreError::located() is intended for Raw errors"),
        }
    }

    pub fn make_raw<H, F>(header: H, footer: F) -> Self
    where
        H: ToString,
        F: ToString,
    {
        Self::Raw {
            message: (header.to_string(), footer.to_string()),
            location: None,
        }
    }

    fn with_context(self, source: &String) -> Self {
        match self {
            Self::Raw {
                message,
                location: Some(location),
            } => {
                let context = source
                    .lines()
                    .nth(location.row() - 1)
                    .map(|s| s.to_string());
                Self::WithContext {
                    message,
                    context,
                    location: Some(location),
                }
            }
            Self::Raw { message, .. } => Self::WithContext {
                message,
                location: None,
                context: None,
            },
            with_context => with_context,
        }
    }
}

impl fmt::Display for CoreError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WithContext {
                message: (header, footer),
                location: None,
                context: None,
            } => {
                write!(f, "Error: {}.\n{}", header, footer)
            }
            Self::WithContext {
                message: (header, footer),
                location: Some(location),
                context: Some(context),
            } => {
                let line = format!("{} |", location.row());
                if footer.len() > 0 {
                    write!(
                        f,
                        "{}: {}.\n{} {}\n{: >col$}\n{}\n",
                        "Error".red().bold(),
                        header.bold(),
                        line,
                        context,
                        "^".bold(),
                        footer,
                        col = line.len() + 1 + location.column()
                    )
                } else {
                    write!(
                        f,
                        "{}: {}.\n{} {}\n{: >col$}\n",
                        "Error".red().bold(),
                        header.bold(),
                        line,
                        context,
                        "^".bold(),
                        col = line.len() + 1 + location.column()
                    )
                }
            }
            s => {
                panic!(
                    "Attempted to display an unformattable error message ({:?})",
                    s
                )
            }
        }
    }
}

impl error::Error for CoreError {}

pub fn compile(python_source: String, program_name: String) -> Result<String, CoreError> {
    let python_source_ = python_source.clone();
    let contextualize = |err: CoreError| err.with_context(&python_source_);

    let python_ast = from_python_source(python_source).map_err(contextualize)?;

    let seahorse_ast = from_python_ast(python_ast).map_err(contextualize)?;

    let rust_source = from_seahorse_ast(seahorse_ast, program_name).map_err(contextualize)?;

    return Ok(rust_source);
}
