#!/bin/bash

# Capture the dir this script is in.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Change into that dir.
cd "$DIR"

# Make sure the dependencies are all downloaded.
lein deps

# Start the GUI.
lein run
