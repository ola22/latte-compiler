#!/bin/bash

for f in $(find tests/bad/ -name '*.lat')
do
	echo $f
    stack exec latc_x86_64 $f
done

