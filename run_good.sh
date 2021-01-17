#!/bin/bash

for f in $(find tests/good/ -name '*.lat')
do
    stack exec latc_x86_64 $f

    exe=tests/good/$(basename $f .lat)
    in=$exe.input
    myout=$exe.myout
    output=$exe.output

    echo $exe
    if [ -e $in ]
    then
        $exe < $in > $myout
    else
        $exe > $myout
    fi
    diff $myout $output
done


for f in $(find tests/arrays1/ -name '*.lat')
do
    stack exec latc_x86_64 $f

    exe=tests/arrays1/$(basename $f .lat)
    in=$exe.input
    myout=$exe.myout
    output=$exe.output

    echo $exe
    if [ -e $in ]
    then
        $exe < $in > $myout
    else
        $exe > $myout
    fi
    diff $myout $output
done


for f in $(find tests/struct/ -name '*.lat')
do
    echo $f
    stack exec latc_x86_64 $f

done

