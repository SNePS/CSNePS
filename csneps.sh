#!/bin/bash

# Capture the dir this script is in.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Change into that dir.
cd "$DIR"

# Run the project with the -c argument (for CLI)
lein run -c
