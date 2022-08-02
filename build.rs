use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::{
    env,
    fs::{read_dir, File},
    io::{Read, Write},
    path::Path,
};

fn main() {
    let entries: Vec<_> = read_dir("data")
        .unwrap()
        .map(|entry| entry.unwrap())
        .collect();

    for entry in entries.iter() {
        println!("cargo:rerun-if-changed=data/{}", entry.path().display());
    }

    // Write the contents of all files in /data to constants.
    let mut tokens = TokenStream::new();
    for entry in entries.iter() {
        let mut contents = String::new();
        let mut file = File::open(entry.path()).unwrap();
        file.read_to_string(&mut contents).unwrap();

        let name = entry
            .path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_uppercase();
        let name = format_ident!("{}", name);

        tokens.extend(quote! {
            pub const #name: &str = #contents;
        });
    }

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let data_path = Path::new(&out_dir).join("data.rs");
    let mut file = File::create(data_path).unwrap();
    file.write_all(tokens.to_string().as_bytes()).unwrap();
}
