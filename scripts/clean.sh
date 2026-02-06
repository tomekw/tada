#!/bin/bash
set -e

echo "Cleaning..."

gprclean -P tada_tests.gpr -XBUILD_PROFILE=debug 2>/dev/null || true
gprclean -P tada_tests.gpr -XBUILD_PROFILE=release 2>/dev/null || true
gprclean -P tada.gpr -XBUILD_PROFILE=debug 2>/dev/null || true
gprclean -P tada.gpr -XBUILD_PROFILE=release 2>/dev/null || true

rm -rf target/

echo "Done."
