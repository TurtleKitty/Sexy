#!/bin/sh

./sexy clean

for i in tests/compiler/*.sxy; do
    echo $i
    echo
    ./sexy run compiler/sexy.sxy compile $i 2>&1;
    echo
done

