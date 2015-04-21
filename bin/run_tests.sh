#!/bin/sh

for i in tests/*.sex; do
    echo $i
    echo
    ./sexy run $i;
    echo
done

