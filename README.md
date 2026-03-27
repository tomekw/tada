# Tada [![Tests](https://github.com/tomekw/tada/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/tomekw/tada/actions/workflows/test.yml)

Abracadabra was too long. Tomek's Ada, too obvious. An opinionated package  management tool for Ada.

Tada handles building, testing, and running Ada packages. It wraps GPRbuild
with sensible defaults and a simple package manifest (`tada.toml`), so you
spend less time writing build scripts and more time writing Ada.

## Rationale

Tada is a project for personal use. I know [Alire](https://alire.ada.dev/) exists, is more feature rich and has hundreds of packages.
And that's fine. Tada is something I always wanted to build. I write Ada for fun and decided to build many projects in it to understand
how the foundational pieces work under the hood. I plan to release more projects in Ada in the near future, and I want to create my own
little programming world around the language. I hope someone finds it useful.

## Status

This is alpha software. I'm actively working it. YMMV.

Tested on Linux x86_64, MacOS ARM, OpenBSD x86_64 and Windows x86_64.

Note on OpenBSD: you have to:

* install `gcc` / `gnat` > 15.2.0 from ports

* install `gprbuild` manually

* symlink `gcc` to `egcc`:

``` shell
# as root
ln -s /usr/local/bin/egcc /usr/local/bin/gcc
```

## Prerequisites

* curl
* gnat
* gprbuild

By default, `tada` looks for `gnat` and `gprbuild` on the system `PATH`. You can set up local and global toolchain paths.

* local: `.tada/config.toml` (should be in `.gitignore`)
* global: `~/.config/tada/config.toml` (`%LOCALAPPDATA%\tada\config.toml` on Windows)

The discovery order is: `local` -> `global` -> `PATH`.

Example `config.toml`:

``` toml
[toolchain]
gnat_root = "~/.local/share/alire/toolchains/gnat_native_15.2.1_4640d4b3"
gprbuild_root = "~/.local/share/alire/toolchains/gprbuild_25.0.1_9a2e6cfb"
```

## Installation

See [Releases](https://github.com/tomekw/tada/releases).

```bash
curl -L https://github.com/tomekw/tada/releases/download/VERSION/tada-VERSION-PLATFORM -o tada && chmod +x tada
```

Copy it somewhere on your `PATH`:

```bash
cp tada ~/.local/bin/
```

Once installed, Tada can build itself:

```bash
tada build --profile release
```

## Usage

```bash
Usage: tada [command] [options]

Commands:
    build [--profile <p>]               Compile the package
    cache [--force]                     Install package to the local cache, use --force to overwrite
    clean                               Remove build artifacts
    config                              Display configuration
    help                                Show this message
    init <name> [--exe|--lib]           Create a new package
    install                             Install dependencies
    run [--profile <p>] [-- <args>...]  Build and run the executable
    test [--profile <p>]                Build and run the tests
    version                             Display version
```

1. Create a new package with `tada init`, either a binary with `--exe` or a library with `--lib`.
1. Run `tada install` to install dependencies.
1. Build with `tada build`. Profile is either `--debug` or `--release`. `--debug` is the default.
1. Run with `tada run`. `--` separates arguments passed to the target binary.
1. Test with `tada test`.
1. Remove `target/` with `tada clean`.

To add a new dependency:

1. Add the dependency to your `tada.toml`, e.g. `bar = "0.5.2"` under `[dependencies]` or `[dev-dependencies]`.
1. Run `tada install`.
1. Use `with` to import the dependency's units in your Ada code. Build, test, and run as usual.

Use `tada cache` to install the current package into the local cache for use as a dependency.

Tada lets you own generated GPR files. `PROJECT_config.gpr` exposes three variables you can use:

* `Build_Profile` - `debug`, `release`
* `Tada_OS` - `bsd`, `linux`, `windows`, `macos`, `unknown`
* `Tada_Arch` - `x86_64`, `aarch64`, `unknown`

## Manifest file

```toml
[package]
name = "foo"
version = "0.1.0"

[dependencies]
bar = "0.5.2"
baz = "1.2.1"

[dev-dependencies]
testy = "0.1.0"
```

## Package naming rules

* can't be empty
* is all lower-case
* letters, numbers and underscores only
* can't be Ada's reserved word
* can't start with a number or underscore
* can't end with an underscore
* can't have two consecutive underscores

## Package versioning rules

* [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`
* optional prerelease tag, example: `0.1.0-dev`

## Testing

Tada's own test suite uses [Testy](https://github.com/tomekw/testy):

```bash
tada test
```

## Packages

See the [Packages Index](https://github.com/tadapm/tada-packages).
  
If there's something you have built with Tada, and would like to have it included in the index, let me know!

## Disclaimer

This codebase is written by hand. Claude Code is used for Socratic design exploration and code review.

## License

[EUPL](LICENSE)
