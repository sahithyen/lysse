#!/bin/sh

x86_64-linux-gnu-gcc hello.S -o hello -nostdlib -nodefaultlibs -no-pie 
chmod u+x ./hello
x86_64-linux-gnu-objcopy -O binary -j .text hello hello.hex
ndisasm -b 64 hello.hex > hello.dis
