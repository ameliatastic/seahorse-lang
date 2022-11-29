# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `Pubkey.find_program_address()` function, to find a PDA given a list of seeds and the program key

### Fixed

- `TokenMint.authority()` works
- Support strings in events

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
