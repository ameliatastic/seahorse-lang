macro_rules! map_const {
    ($name:ident, $path:literal) => {
        pub const $name: &'static str = include_str!(concat!("../data/const/", $path));
    };
}

// Text data (see /data/const), included as string constants.
map_const!(README, "readme.md");
map_const!(SEAHORSE_PRELUDE, "seahorse_prelude.py");
map_const!(SEAHORSE_PYTH, "seahorse_pyth.py");
map_const!(SEAHORSE_SRC_TEMPLATE, "seahorse_src_template.py");

// Pyth price addresses
include!(concat!(env!("OUT_DIR"), "/pyth.rs"));
