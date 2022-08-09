use crate::cli::*;
use clap::{Parser, Subcommand};
use std::process::exit;

pub const SRC_PATH: &str = "programs_py";
pub const LIB_PATH: &str = "seahorse";

#[derive(Debug, Parser)]
#[clap(name = "Seahorse")]
#[clap(version, about)]
pub struct Cli {
    #[clap(subcommand)]
    command: CliCommand,
}

#[derive(Debug, Subcommand)]
pub enum CliCommand {
    /// Initializes a new Seahorse project
    Init(InitArgs),
    /// Builds a Seahorse program
    Build(BuildArgs),
    /// Compiles a single Seahorse file
    Compile(CompileArgs),
    /// Updates the Seahorse Python libraries
    Update(UpdateArgs),
}

/// Run the CLI.
pub fn run() {
    let args = Cli::parse();

    let res = match args.command {
        CliCommand::Init(args) => init(args),
        CliCommand::Build(args) => build(args),
        CliCommand::Compile(args) => compile(args),
        CliCommand::Update(args) => update(args),
    };

    if let Err(err) = res {
        println!("{}", err);
        exit(1);
    }
}
