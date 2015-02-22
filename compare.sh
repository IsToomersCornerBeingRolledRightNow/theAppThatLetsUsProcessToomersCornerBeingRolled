#!/bin/bash

myRandom=$(echo "$RANDOM % 10" | bc)

echo $myRandom

