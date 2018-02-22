#!/bin/sh

################################################################################
# Wrapper around the edify executable that points it at the correct
# data-dir so it can find its migrations and other necessary files.
set -e

################################################################################
top=$(realpath "$(dirname "$0")/..")
export edify_datadir=$top
"$top"/dist/build/edify/edify "$@"
