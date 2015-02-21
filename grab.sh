#!/bin/bash

# Grabs $GRABS frames, one per $FREQ seconds, from $STREAM.

STREAM="rtsp://68.152.51.100/axis-media/media.amp" # source stream
GRABS="1" # number of frames to grab
FREQ="1" # frequency in seconds
OUT_DIR="./images" # no trailing slash

TIME=$(date +%s) # gets the system time in seconds

if [ ! -d $OUT_DIR ]; then
	mkdir $OUT_DIR
fi

avconv -i $STREAM -t $GRABS -r $FREQ -vsync 1 -qscale 1 -f image2 ${OUT_DIR}/${TIME}-%09d.jpg
