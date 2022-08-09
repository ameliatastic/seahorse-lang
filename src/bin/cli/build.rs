use crate::{
    cli::{util::*, SRC_PATH},
    core::compile,
};
use clap::Args;
use owo_colors::OwoColorize;
use std::{
    error::Error,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    process::Command,
};

#[derive(Args, Debug)]
pub struct BuildArgs {
    /// Name of the program to build. By default, builds all programs
    #[clap(short = 'p', long)]
    program: Option<String>,
}

/// Build a single program.
fn build_program(project_path: &PathBuf, program_name: String) -> Result<String, Box<dyn Error>> {
    let first_build = !project_path
        .join("target")
        .join("deploy")
        .join(format!("{}.so", program_name))
        .exists();

    let output = with_spinner(
        if first_build {
            format!("Compiling {}... (note: if this is your first time building, it might take a few minutes)", program_name.bold())
        } else {
            format!("Compiling {}...", program_name.bold())
        },
        format!("Compiled {}", program_name.bold()),
        || {
            let input_path = project_path
                .join(SRC_PATH)
                .join(format!("{}.py", program_name));
            if !input_path.exists() {
                return Err(
                    error_message(format!("program \"{}\" does not exist", program_name)).into(),
                );
            }

            let mut py_src = String::new();
            let mut input = File::open(input_path)?;
            input.read_to_string(&mut py_src)?;

            let rs_src = compile(py_src, program_name.clone())?;

            let output_path = project_path
                .join("programs")
                .join(program_name.clone())
                .join("src")
                .join("lib.rs");
            let mut output = File::create(output_path)?;
            output.write_all(rs_src.as_bytes())?;

            let anchor_output = Command::new("anchor")
                .args(["build", "-p", program_name.as_str()])
                .output()?;
            if !anchor_output.status.success() {
                return Err(error_message(format!(
                    "{} failed:\n{}",
                    "anchor build",
                    String::from_utf8(anchor_output.stderr)?
                ))
                .into());
            }

            return Ok(anchor_output.stdout);
        },
    )?;

    return Ok(String::from_utf8(output)?);
}

/// Builds a Seahorse program.
pub fn build(args: BuildArgs) -> Result<(), Box<dyn Error>> {
    let root = project_root()?;

    if let Some(program_name) = args.program {
        let output = build_program(&root, program_name)?;
        print!("{}", output);
    } else {
        let src_path = root.join(SRC_PATH);
        let programs = src_path.read_dir()?.filter_map(|entry| {
            let path = entry.unwrap().path();
            let ext = path.extension();
            if ext.is_none() || ext.unwrap() != "py" {
                return None;
            }

            Some(path.file_stem().unwrap().to_str().unwrap().to_string())
        });

        let output: Vec<_> = programs
            .into_iter()
            .map(|program_name| build_program(&root, program_name))
            .collect();
        let has_err = output.iter().any(|result| result.is_err());

        if has_err {
            let errors = output.into_iter().filter_map(|result| result.err());

            let mut concatenated = String::new();
            let mut past_first = false;
            for error in errors {
                if past_first {
                    concatenated.push_str("\n");
                }
                concatenated.push_str(format!("{}", error).as_str());

                past_first = true;
            }

            return Err(concatenated.into());
        } else {
            let mut past_first = false;
            for result in output {
                if past_first {
                    println!();
                }
                println!("{}", result.unwrap());

                past_first = true;
            }
        }
    }

    return Ok(());
}
