#!/bin/sh

TOP=$(dirname "$0")/../..
exec "$TOP"/dist/build/edify/edify filter
