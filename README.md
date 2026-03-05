# Tada [![Tests](https://github.com/tomekw/tada/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/tomekw/tada/actions/workflows/test.yml)

Abracadabra was too long. Tomek's Ada, too obvious. An opinionated package  management tool for Ada.

Tada handles building, testing, and running Ada packages. It wraps GPRbuild
with sensible defaults and a simple package manifest (`tada.toml`), so you
spend less time writing build scripts and more time writing Ada.

## Status

This is alpha software. I'm actively working it. YMMV.

Tested on Linux x86_64, MacOS ARM and Windows x86_64.

## Prerequisites

### Debian / Ubuntu

```bash
sudo apt install curl gnat gprbuild
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
    init <name> [--exe|--lib]           Create a new package
    build [--profile <p>]               Compile the package
    run [--profile <p>] [-- <args>...]  Build and run the executable
    test [--profile <p>]                Build and run the tests
    install                             Install dependencies
    clean                               Remove build artifacts
    help                                Show this message
    version                             Display version
```

The workflow, for now, is still somewhat manual:

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

## Testing

Tada's own test suite uses [Testy](https://github.com/tomekw/testy):

```bash
tada test
```

## Packages

For now, there are only two that I know of:

* tackle (https://github.com/tomekw/tackle)

  Or TACkLe. Or Tomek's Ada Class Library. Everything I find useful and I think should be a part of the (extended) standard library.

* testy (https://github.com/tomekw/testy)

  Ada testing framework
  
If there's something you have built with Tada, and would like to have it included here, let me know!

## Disclaimer

This codebase is written by hand. Claude Code is used for Socratic design exploration and code review.

## License

[EUPL](LICENSE)
