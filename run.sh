#!/bin/sh

stack run -- code.ly ly amd64
echo "\n--- start output ---"
./ly
ec=$?
echo "--- end output ---"
echo "Exit code: $ec"
