#!/bin/bash

for f in $(find tests/bad/ -name '*.lat')
do
	echo $f
    cabal new-exec latc $f
done

