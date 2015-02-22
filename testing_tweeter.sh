#!/bin/bash

image=$1 # the path to an image file
stamp=$(date)

cp $image ./tmp/${stamp}.jpg
echo "${stamp} - tweeting a success" >> ./tmp/testing.log
