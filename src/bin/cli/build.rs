use crate::{
    cli::{util::*, SRC_PATH},
    core::{compile, Tree},
};
use clap::Args;
use owo_colors::OwoColorize;
use std::{
    error::Error,
    fs::{remove_dir_all, DirBuilder, File},
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

/// Write a source code tree to the filesystem.
fn write_src_tree(tree: &Tree<String>, mut path: PathBuf) -> Result<(), Box<dyn Error>> {
    match tree {
        Tree::Node(node) => {
            DirBuilder::new().recursive(true).create(&path)?;

            for (name, tree) in node.iter() {
                let path = path.join(name);
                write_src_tree(tree, path)?;
            }
        }
        Tree::Leaf(src) => {
            path.set_extension("rs");
            let mut output = File::create(path)?;
            output.write_all(src.as_bytes())?;
        }
    }

    return Ok(());
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

            let tree = compile(
                py_src,
                program_name.clone(),
                Some(project_path.join(SRC_PATH)),
            )?;

            let src = project_path
                .join("programs")
                .join(program_name.clone())
                .join("src");

            remove_dir_all(&src)?;
            write_src_tree(&tree.tree, src)?;

            let mut args = vec!["build", "-p", program_name.as_str()];
            if tree.features.len() > 0 {
                args.push("--");
                args.push("--features");
                for feature in tree.features.iter() {
                    args.push(feature.name());
                }
            }

            let mut cmd = "anchor".to_string();
            for arg in args.iter() {
                cmd.push(' ');
                cmd.push_str(*arg);
            }

            let anchor_output = Command::new("anchor").args(args).output()?;
            let stderr = String::from_utf8(anchor_output.stderr)?;

            if !anchor_output.status.success()
                || stderr.contains("error") | stderr.contains("panicked")
            {
                let report_note = concat!(
                    "This is most likely a bug in the Seahorse compiler!\n\n",
                    "If you want to help the project, you can report this:\n",
                    "  - on the Seahorse Discord (http://discord.gg/4sFzH5pus8)\n",
                    "  - or as a Github issue (https://github.com/ameliatastic/seahorse-lang/issues).\n\n",
                    "Thanks!"
                ).bold();
                return Err(error_message(format!(
                    "{} failed:\n\n{}\n\n{}",
                    cmd, report_note, stderr
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
