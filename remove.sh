#!/usr/bin/env bash

rm $1/*.o $1/*.s $1/*myout

for input in $1/*.lat ; do
    exe=$1/$(basename $input .lat)
    rm $exe
done