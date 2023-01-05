#!/usr/bin/env bash

# get script directory, and root directory (which is one level higher)
# this allows the script to be run from any directory
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

for f in ${ROOT_DIR}/examples/*.py
do
    # get just the filename without directories or file extension (eg calculator)
    name="$(basename -- $f .py)"
    ${ROOT_DIR}/target/debug/seahorse compile $f > ${SCRIPT_DIR}/compiled-examples/$name.rs
done

for f in ${SCRIPT_DIR}/test-cases/*.py
do
    # get just the filename without directories or file extension (eg calculator)
    name="$(basename -- $f .py)"
    ${ROOT_DIR}/target/debug/seahorse compile $f > ${SCRIPT_DIR}/compiled-test-cases/$name.rs
done