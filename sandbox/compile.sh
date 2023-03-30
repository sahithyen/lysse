#!/bin/sh

gcc hello.S -o hello -nostdlib -nodefaultlibs -no-pie 
chmod u+x ./hello
objcopy -O binary -j .text hello hello.hex
ndisasm -b 64 hello.hex > hello.dis
