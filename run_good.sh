#!/bin/bash

for f in $(find tests/good/ -name '*.lat')
do
	echo $f
    stack exec latc $f
done

