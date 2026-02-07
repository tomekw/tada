#!/bin/bash
set -e

PROFILE="${1:-debug}"

./scripts/build.sh $PROFILE

echo ""
echo "Running tests (profile: $PROFILE)..."

./target/$PROFILE/bin/run_tests
