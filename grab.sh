#!/bin/bash

# Grabs $GRABS frames, one per $FREQ seconds, from $STREAM.

myStream="$1" # the raw video stream
myImage="$2" # the name of the output file
myGrabs="1" # number of frames to grab
myFreq="1" # frequency in seconds

avconv -i "$myStream" -t $myGrabs -r $myFreq -vsync 1 -qscale 1 -f image2 "$2"

