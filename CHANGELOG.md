# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Access to Pyth `Price.publish_time` (#91)
- Accounts can now store complex types (lists, user-defined classes, multi-dimensional arrays, etc.)

### Fixed

- Bug with unary not operator
- Pyth compile error on latest version (#91)
- Bug that prevented lists from being used in events
- Bug that prevented users from importing accounts from other files
- Conditionally generate Pyth import (#93)

## [0.2.7]

### Added

- Constants may be defined in source files

### Fixed

- `sum` now works with floats (and also handles type errors properly)
- Bug that caused compilation errors based on the order that modules were imported in
- Error when using account key() as seed
- Bug caused by array/list indexing

### Changed

- Import handling is different: `prelude` is now imported automatically, and objects imported from it are not re-exported

## [0.2.6]

### Added

- Getters for `TokenAccount`'s mint key and `TokenMint`'s freeze authority, decimals, and supply
- Type hints for comparison operators for numeric types

### Fixed

- Bug when assigning to variables in deeper scopes
- Bug that caused strings to not be converted to owned types in generate code
- Bug that kept clones from appearing in generated code, leading to borrow checker errors

## [0.2.5]

### Added

- `Pubkey.find_program_address()` function, to find a PDA given a list of seeds and the program key
- Decorator @dataclass for classes to automatically generate a default constructor

### Fixed

- `TokenMint.authority()` works
- Support strings in events
- Bug where syntactic transformations would be duplicated in some arithmetic expressions
- Make token mints and accounts mutable in instruction contexts

## [0.2.4]

### Added

- Pyth integration

### Fixed

- System clock is now immutable in instruction contexts (a.k.a. it works now)
- Fix accessing index 0 of arrays

## [0.2.3]

### Added

- Allow storing arrays of enums on an account
- New `size` function that returns the size of a string (for now) in bytes
- Allow setting `space` or `padding` on account init to customise account size
- Support for Anchor events
- Allow using pubkeys as seeds
- Support for importing from local files

### Changed

- `len(str)` now has the same behaviour as Python, and returns character count. This may be different to the size in bytes

## [0.2.2]

### Fixed

- Bug that prevented numeric constant types from being unified (impacted array handling)
- Ambiguous error messages caused by unresolved imports

### Added

- Allow casting `Array`s as iterables

## [0.2.1]

### Fixed

- Removed some of the unnecessary `borrow()`s from appearing in seed lists.

### Added

- Support for compiling to WASM
- Allow non-enum classes to define methods, including static methods
- Allow non-account classes to define constructors

## [0.2.0] - 2022-10-05

- Added changelog.
