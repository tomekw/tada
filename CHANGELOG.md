## unreleased

* bump `testy` to `0.3.0`
* allow to set test seed via `tada test --seed SEED`, requires `testy >= 0.3.0`
* describe default values in `--help`
* change the package index to `https://index.tada.pm/index`

## 0.11.0

* breaking: reworked command line options
* breaking: `init` moved from: `tada init PROJECT_NAME --lib|--exe` to `tada init --name PROJECT_NAME --type lib|exe`

## 0.10.0

* distinct FreeBSD and OpenBSD support

## 0.9.0

* generate `.gitattributes`
* adjust `-static` to Binder switches on all platforms
* add `-gnatX` to Compiler switches
* add BSD support

## 0.8.0

* expose Tada_OS (linux, macos, windows, unknown) variable in GPR files
* expose Tada_Arch (x86_64, aarch64, unknown) variable in GPR files
* set Linker switches for stripping dead code in the release profile
* fix: `tada install` and `tada cache` now cache all package contents instead of only known files, so packages with extra directories (e.g., `vendor/`) work correctly

## 0.7.0

* validate package versions: Semver + optional prerelease tag, example: `0.1.0-dev`
* add `tada config` command: display configuration
* support local and global toolchain configuration

## 0.6.0

* `tada help`: commands in the alphabetical order
* bring back `tada cache`: install package to the local cache

## 0.5.0

* add `tada install` command: install packages from index
* breaking: remove `tada cache` command
* breaking: test runner executable renamed to `tests`

## 0.4.0

* local (cached) dependencies support
* add `tada cache` command: add package to the local cache
* breaking: scaffold empty `PACKAGE_deps.gpr` file

## 0.3.0

* breaking: change `tada.toml` format
* breaking: change `LICENSE` to `EUPL`

## 0.2.0

* parameterize `Library_Dir` with build profile

## 0.1.0

* initial release
