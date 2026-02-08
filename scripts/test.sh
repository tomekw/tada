#!/bin/bash
set -e

PROFILE="${1:-debug}"

echo "Running tests (profile: $PROFILE)..."

./target/$PROFILE/bin/run_tests
