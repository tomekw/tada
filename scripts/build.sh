#!/bin/bash
set -e

PROFILE="${1:-debug}"

if [[ "$PROFILE" != "debug" && "$PROFILE" != "release" ]]; then
  echo "Usage: $0 [debug|release]"
  echo "  Default: debug"
  exit 1
fi

echo "Building (profile: $PROFILE)..."
echo ""
gprbuild -P tada.gpr -XBUILD_PROFILE="$PROFILE" -p

echo ""
echo "Building tests (profile: $PROFILE)..."
echo ""
gprbuild -P tada_tests.gpr -XBUILD_PROFILE="$PROFILE" -p

echo ""
echo "Done. See: target/$PROFILE/bin/"
