---
description: Install Seahorse and get started with writing Solana programs!
---

# Installation

### Dependencies <a href="#dependencies" id="dependencies"></a>

#### Anchor <a href="#anchor" id="anchor"></a>

Seahorse uses the Anchor framework to create Solana projects.

[Installation instructions here.](https://book.anchor-lang.com/getting\_started/installation.html) You may also need to install Rust, Solana, and Yarn if you haven't already.

#### Rustfmt <a href="#rustfmt" id="rustfmt"></a>

Seahorse uses rustfmt to help generate better-looking Rust code.

[Installation instructions here.](https://github.com/rust-lang/rustfmt)

### Seahorse <a href="#seahorse" id="seahorse"></a>

From your terminal run:

```
cargo install seahorse-dev
```

This will install the `seahorse` binary.

If you want, make sure that the installation worked by printing out the version:

```
seahorse -V
```

You should be ready to start now! Let's make a simple program with Seahorse.
