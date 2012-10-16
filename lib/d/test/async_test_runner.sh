#!/bin/bash
# Runs the async test in both SSL and non-SSL mode.
./async_test > /dev/null || exit 1
echo "Non-SSL tests done."
./async_test --ssl > /dev/null || exit 1
echo "SSL tests done."
