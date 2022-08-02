use crate::{
    cli::{util::*, LIB_PATH, SRC_PATH},
    data,
};
use clap::{crate_version, Args};
use heck::{ToPascalCase, ToSnakeCase};
use owo_colors::OwoColorize;
use regex::Regex;
use std::{
    error::Error,
    fs::{create_dir_all, File},
    io::{Read, Write},
    path::Path,
    process::Command,
};
use toml_edit::{value, Document, Item};

#[derive(Args, Debug)]
pub struct InitArgs {
    /// Name of your project, which will be created at ./<project_name>
    #[clap(value_parser)]
    project_name: String,
}

/// Do some template text replacements.
fn from_template(mut text: String, project_name: &String) -> String {
    let re_project_name = Regex::new(r"\$project-name").unwrap();
    let re_project_name_snake = Regex::new(r"\$project_name").unwrap();
    let re_project_name_pascal = Regex::new(r"\$ProjectName").unwrap();
    let re_version = Regex::new(r"\$version").unwrap();

    text = re_project_name.replace_all(&text, project_name).to_string();
    text = re_project_name_snake
        .replace_all(&text, project_name.to_snake_case())
        .to_string();
    text = re_project_name_pascal
        .replace_all(&text, project_name.to_pascal_case())
        .to_string();
    text = re_version.replace_all(&text, crate_version!()).to_string();

    return text;
}

/// Initializes a new Seahorse project.
pub fn init(args: InitArgs) -> Result<(), Box<dyn Error>> {
    let path = Path::new(&args.project_name);
    if path.exists() {
        return Err(error_message("project directory already exists").into());
    }

    with_spinner(
        "Checking for dependencies...",
        "All dependencies found",
        || {
            let anchor_installed = Command::new("anchor")
                .args(["-V"])
                .output()
                .unwrap()
                .status
                .success();
            if !anchor_installed {
                return Err(error_message(format!(
                    concat!(
                        "Anchor not found\n\n",
                        "Seahorse depends on Anchor, a framework for writing Solana programs. ",
                        "Installation instructions can be found here:\n",
                        "{}"
                    ),
                    "https://book.anchor-lang.com/getting_started/installation.html".blue()
                ))
                .into());
            }

            let rustfmt_installed = Command::new("rustfmt")
                .args(["-V"])
                .output()
                .unwrap()
                .status
                .success();
            if !rustfmt_installed {
                return Err(error_message(format!(
                    concat!(
                        "rustfmt not found\n\n",
                        "Seahorse depends on rustfmt, a code formatter that comes as part of the Rust toolchain. ",
                        "Installation instructions can be found here:\n",
                        "{}"
                    ),
                    "https://github.com/rust-lang/rustfmt".blue()
                )).into());
            }

            Ok(())
        },
    )?;

    with_spinner(
        "Initializing Anchor project...",
        "Anchor project initialized",
        || {
            let anchor_output = Command::new("anchor")
                .args(["init", path.to_str().unwrap()])
                .output()
                .unwrap();
            if !anchor_output.status.success() {
                return Err(error_message(format!(
                    "{} failed:\n{}",
                    "anchor init".underline(),
                    String::from_utf8(anchor_output.stderr)?
                ))
                .into());
            }

            Ok(())
        },
    )?;

    with_spinner(
        "Adding Seahorse project files...",
        "Project files added",
        || {
            let src_path = path.join(SRC_PATH);
            let lib_path = src_path.join(LIB_PATH);
            let program_path = path.join("programs").join(&args.project_name);
            let src_name = format!("{}.py", args.project_name.to_snake_case());

            create_dir_all(src_path.clone())?;
            create_dir_all(lib_path.clone())?;

            let mut src = File::create(src_path.join(src_name))?;
            let text = data::SEAHORSE_SRC_TEMPLATE.to_string();
            src.write_all(from_template(text, &args.project_name).as_bytes())?;

            File::create(lib_path.join("__init__.py"))?;

            let mut prelude = File::create(lib_path.join("prelude.py"))?;
            prelude.write_all(data::SEAHORSE_PRELUDE.as_bytes())?;

            let mut readme = File::create(path.join("README.md"))?;
            let text = data::README.to_string();
            readme.write_all(from_template(text, &args.project_name).as_bytes())?;

            // Add anchor-spl as a dependency
            let cargo_path = program_path.join("Cargo.toml");
            let mut cargo = String::new();
            File::open(&cargo_path)?.read_to_string(&mut cargo)?;
            let mut cargo = cargo.as_str().parse::<Document>().unwrap();

            let anchor_version = cargo["dependencies"]["anchor-lang"].clone();
            cargo["dependencies"]["anchor-lang"] = anchor_version.clone();
            cargo["dependencies"]["anchor-spl"] = anchor_version;

            File::create(&cargo_path)?.write_all(cargo.to_string().as_bytes())?;

            // Add Anchor seeds feature
            let anchor_path = path.join("Anchor.toml");
            let mut anchor = String::new();
            File::open(&anchor_path)?.read_to_string(&mut anchor)?;
            let mut anchor = anchor.as_str().parse::<Document>().unwrap();

            anchor["features"]["seeds"] = "true".parse::<Item>().unwrap();

            File::create(&anchor_path)?.write_all(anchor.to_string().as_bytes())?;

            Ok(())
        },
    )?;

    println!(
        "Project {} successfully generated!",
        args.project_name.bold()
    );
    let seahorse_src_path = path
        .join(SRC_PATH)
        .join(format!("{}.py", args.project_name.to_snake_case()));
    println!(
        "Open {} to get started.",
        seahorse_src_path.to_str().unwrap().blue()
    );

    return Ok(());
}
