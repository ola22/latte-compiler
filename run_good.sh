#!/bin/bash

for f in $(find tests/good/ -name '*.lat')
do
	echo $f
    cabal new-exec latc $f
done

