use owo_colors::OwoColorize;
use spinners::{Spinner, Spinners};
use std::{error::Error, fmt::Display};

/// Run a task with a terminal spinner, with a custom success message.
pub fn with_spinner<S1, S2, F, T>(spin_msg: S1, ok_msg: S2, task: F) -> Result<T, Box<dyn Error>>
where
    S1: ToString,
    S2: ToString,
    F: FnOnce() -> Result<T, Box<dyn Error>>,
{
    let mut spinner = Spinner::new(Spinners::Dots, spin_msg.to_string());

    match task() {
        Ok(res) => {
            spinner.stop_and_persist("✔".green().to_string().as_str(), ok_msg.to_string());
            Ok(res)
        }
        err => {
            spinner.stop_with_symbol("✗".red().to_string().as_str());
            err
        }
    }
}

/// Generic error message formatting.
pub fn error_message<D: Display>(msg: D) -> String {
    format!("{}: {}", "Error".red().bold(), msg)
}
