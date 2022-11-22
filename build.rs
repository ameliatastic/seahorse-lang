use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::{
    env,
    ffi::OsString,
    fs::{read_dir, File},
    io::{Read, Write},
    path::Path,
};

fn build_consts(out_dir: &OsString) {
    let mut tokens = TokenStream::new();

    let entries = read_dir("data/const").unwrap().map(|entry| entry.unwrap());
    for entry in entries {
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

    let data_path = Path::new(out_dir).join("data.rs");
    let mut file = File::create(data_path).unwrap();
    file.write_all(tokens.to_string().as_bytes()).unwrap();
}

fn build_pyth_table(out_dir: &OsString) {
    let mut tokens = TokenStream::new();

    let mut pyth_csv = String::new();
    // Note on this data: copy-pasted from here (https://pyth.network/developers/price-feed-ids#solana-mainnet-beta)
    // and formatted it into a CSV
    let mut file = File::open("data/pyth.csv").unwrap();
    file.read_to_string(&mut pyth_csv).unwrap();

    let rows = pyth_csv.lines().filter_map(|line| {
        let cols = line.split(',').collect::<Vec<_>>();

        // Finally found a use for slice matching, ty mr rust
        if let &[cluster, _, product, addr] = &cols[..] {
            let product = format!("{}-{}", cluster, product);
            Some(quote! {
                #product => Some(#addr)
            })
        } else {
            None
        }
    });

    tokens.extend(quote! {
        pub fn get_pyth_price_address(product: &str) -> Option<&'static str> {
            match product {
                #(#rows),*,
                _ => None
            }
        }
    });

    let data_path = Path::new(out_dir).join("pyth.rs");
    let mut file = File::create(data_path).unwrap();
    file.write_all(tokens.to_string().as_bytes()).unwrap();
}

fn main() {
    let entries = read_dir("data").unwrap().map(|entry| entry.unwrap());

    for entry in entries {
        println!("cargo:rerun-if-changed=data/{}", entry.path().display());
    }

    let out_dir = env::var_os("OUT_DIR").unwrap();
    // Write the contents of all files in /data/const to constants.
    build_consts(&out_dir);
    // Write the table in /data/pyth.csv to something Rust-readable.
    build_pyth_table(&out_dir);
}
