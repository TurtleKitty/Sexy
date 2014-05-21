#!/bin/sh

for i in tests/*; do
    echo $i
    echo
    ./sexy run $i;
    echo
done

