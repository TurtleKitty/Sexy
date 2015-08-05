#!/bin/sh

for i in ../tests/*.sex; do
    echo $i
    echo
    csi -script main.scm run $i;
    echo
done

