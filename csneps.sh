#!/bin/bash

# Capture the dir this script is in.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Change into that dir.
cd "$DIR"

# Start the leiningen REPL.
lein repl
