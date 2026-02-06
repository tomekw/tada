#!/bin/bash
set -e

PROFILE="${1:-debug}"

./scripts/build.sh $PROFILE

echo ""
echo "Running (profile: $PROFILE)..."
echo ""

./target/$PROFILE/bin/tada
