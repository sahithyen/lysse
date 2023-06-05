#!/bin/sh

stack run -- code.ly ly aarch64 -s
echo "\n--- start output ---"
./ly
ec=$?
echo "--- end output ---"
echo "Exit code: $ec"
