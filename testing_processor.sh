#!/bin/bash

image=$1 # the path to an image file
tolerance=$2 # a (probably small) integer, like 10
# should return an int between 0 and, say, 32767

fakeResult=$RANDOM
stamp=$(date)

echo "${stamp} - fake result ${fakeResult}" >> ./tmp/testing.log
echo $fakeResult
