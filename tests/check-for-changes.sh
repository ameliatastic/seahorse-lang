#!/usr/bin/env bash

# This script checks git status, and exits with an error if there are any changes
# It's intended to ensure that all changes to compiled-examples/ are checked in

status=$(git status --porcelain)

if [ -n "$status" ]
then
    echo "Found unexpected changes. You probably need to run ./tests/compile-examples.sh and check in any resulting changes"
    # log the git status and diff
    git status
    git diff
    exit 1
fi
