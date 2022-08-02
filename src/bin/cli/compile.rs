use crate::{cli::util::*, core::compile as seahorse_compile};
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

pub fn compile(args: CompileArgs) -> Result<(), Box<dyn Error>> {
    let input_file = args.input_file.map(|path| Path::new(&path).to_path_buf());
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

    let rs_src = seahorse_compile(py_src, program_name)?;

    output.write_all(rs_src.as_bytes())?;

    Ok(())
}
