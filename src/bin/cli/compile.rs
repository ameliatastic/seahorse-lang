use crate::{
    cli::util::*,
    core::{compile as seahorse_compile, generate::GenerateOutput, Tree},
};
use clap::Args;
use std::{
    error::Error,
    fs::File,
    io::{stdin, stdout, BufReader, BufWriter, Read, Write},
    path::Path,
};

#[derive(Args, Debug)]
pub struct CompileArgs {
    /// Input file. If not specified, reads from stdin
    #[clap(value_parser)]
    input_file: Option<String>,

    /// Output file. If not specified, writes to stdout
    #[clap(value_parser)]
    output_file: Option<String>,
}

/// Flatten a generated filetree into a single string with filename headers.
fn flatten(tree: &Tree<String>) -> String {
    _flatten(tree, &mut vec![])
}

fn _flatten(tree: &Tree<String>, path: &mut Vec<String>) -> String {
    match tree {
        Tree::Node(dir) => {
            let mut cat = String::new();

            // Order for consistent output
            let mut parts = vec![];
            for (name, tree) in dir.iter() {
                path.push(name.clone());
                parts.push((name.clone(), _flatten(tree, path)));
                path.pop();
            }

            parts.sort();
            for (_, text) in parts.into_iter() {
                cat.push_str(text.as_str());
            }

            cat
        }
        Tree::Leaf(text) => {
            let mut filename = path[0].clone();
            for part in path.iter().skip(1) {
                filename.push_str(format!("/{}", part).as_str());
            }
            filename.push_str(".rs");

            format!("// ===== {} =====\n\n{}\n", filename, text)
        }
    }
}

pub fn compile(args: CompileArgs) -> Result<(), Box<dyn Error>> {
    let input_file = args.input_file.map(|path| Path::new(&path).to_path_buf());
    let working_dir = input_file.clone().map(|mut path| {
        path.pop();
        path
    });
    let output_file = args.output_file.map(|path| Path::new(&path).to_path_buf());

    let program_name = match (&input_file, &output_file) {
        (Some(path), _) => path.file_stem().unwrap().to_str().unwrap().to_string(),
        (None, Some(path)) => path.file_stem().unwrap().to_str().unwrap().to_string(),
        (None, None) => "seahorse_program".to_string(),
    };

    let mut input: BufReader<Box<dyn Read>> = match input_file {
        Some(path) => {
            if !path.exists() {
                return Err(
                    error_message(format!("input file {} does not exist", path.display())).into(),
                );
            }

            BufReader::new(Box::new(File::open(path)?))
        }
        None => BufReader::new(Box::new(stdin())),
    };

    let mut output: BufWriter<Box<dyn Write>> = match output_file {
        Some(path) => {
            if !path.exists() {
                return Err(error_message(format!(
                    "output file {} does not exist",
                    path.display()
                ))
                .into());
            }

            BufWriter::new(Box::new(File::create(path)?))
        }
        None => BufWriter::new(Box::new(stdout())),
    };

    let mut py_src = String::new();
    input.read_to_string(&mut py_src)?;

    let rs_src = seahorse_compile(py_src, program_name, working_dir)?;

    output.write_all(flatten(&rs_src.tree).as_bytes())?;

    Ok(())
}
