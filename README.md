# Tada

Abracadabra was too long. Tomek's Ada, too obvious. An opinionated project management tool for Ada.

Tada handles building, testing, and running Ada projects. It wraps GPRbuild
with sensible defaults and a simple project manifest (`tada.toml`), so you
spend less time writing build scripts and more time writing Ada.

## Prerequisites

### Debian / Ubuntu

```bash
sudo apt install gnat gprbuild libaunit-dev
```

- **gnat** -- GNAT Ada compiler
- **gprbuild** -- GPR-based build system for Ada
- **libaunit-dev** -- AUnit testing framework (needed to build and run tests)

## Installation

Clone the repository and build:

```bash
git clone https://github.com/tomekw/tada.git
cd tada
gprbuild -P tada.gpr -XBUILD_PROFILE=release -p
```

The binary is at `target/release/bin/tada`. Copy it somewhere on your `PATH`:

```bash
cp target/release/bin/tada ~/.local/bin/
```

Once installed, Tada can build itself:

```bash
tada build --profile release
```

## Usage

### Create a new project

```bash
tada init my_project          # executable project (default)
tada init my_project --lib    # library project
```

### Build

```bash
tada build                      # debug build (default)
tada build --profile release    # release build
```

### Run

```bash
tada run                                  # build and run (debug)
tada run --profile release                # build and run (release)
tada run --profile debug -- --flag arg    # pass arguments to the executable
```

### Test

```bash
tada test                       # build and run tests (debug)
tada test --profile release     # build and run tests (release)
```

### Clean

```bash
tada clean    # remove the target/ directory
```

### Help

```bash
tada help
```

## Testing

Tada's own test suite uses AUnit:

```bash
tada test
```

## License

[MIT](LICENSE)
