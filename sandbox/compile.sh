#!/bin/sh

gcc hello.S -o hello -nostdlib -nodefaultlibs -no-pie 
chmod u+x ./hello
