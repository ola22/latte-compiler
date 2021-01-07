#!/bin/bash

for f in $(find tests/good/ -name '*.lat')
do
	echo $f
    stack exec latc_x86_64 $f
done

