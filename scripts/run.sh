#!/bin/bash
set -e

PROFILE="${1:-debug}"

echo "Running (profile: $PROFILE)..."
echo ""

./target/$PROFILE/bin/tada
