#!/bin/sh

gcc some.S -o some -nostdlib -nodefaultlibs
chmod u+x ./some
./some
echo "Exit code: $?"
# rm some