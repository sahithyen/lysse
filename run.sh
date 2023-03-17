#!/bin/sh

stack run
echo "\n--- start output ---"
./ly
ec=$?
echo "--- end output ---"
echo "Exit code: $ec"
