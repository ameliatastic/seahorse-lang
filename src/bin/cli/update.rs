use crate::{
    cli::{util::*, LIB_PATH, SRC_PATH},
    data,
};
use clap::Args;
use std::{
    error::Error,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

#[derive(Args, Debug)]
pub struct UpdateArgs {}

pub fn update_prelude(root: PathBuf) -> Result<(), Box<dyn Error>> {
    let lib_path = root.join(SRC_PATH).join(LIB_PATH);

    let mut prelude = File::create(lib_path.join("prelude.py"))?;
    prelude.write_all(data::SEAHORSE_PRELUDE.as_bytes())?;

    let mut pyth = File::create(lib_path.join("pyth.py"))?;
    pyth.write_all(data::SEAHORSE_PYTH.as_bytes())?;

    return Ok(());
}

pub fn update(args: UpdateArgs) -> Result<(), Box<dyn Error>> {
    let root = project_root()?;

    with_spinner("Updating...", "Updated!", || {
        update_prelude(root)?;

        return Ok(());
    })?;

    return Ok(());
}
