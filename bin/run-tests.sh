#!/bin/sh

echo "=== Running Unit Tests ==="
echo
echo "--- running common.lisp tests ---"
sbcl --noinform --load tests/run-common-tests.lisp --no-linedit --no-swank;
