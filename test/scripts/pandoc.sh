#!/bin/sh

SELF=$(dirname "$0")
pandoc -f markdown -t markdown \
       --filter "$SELF"/edify.sh \
       < "${1:-$SELF/../data/a.md}"
