#!/bin/bash

A=$1
B=$2

diff -q $A $B > /dev/null 2>&1
ret=$?
if test $ret -eq 0; then
    echo "  OK"
else
    echo "FAIL"
fi