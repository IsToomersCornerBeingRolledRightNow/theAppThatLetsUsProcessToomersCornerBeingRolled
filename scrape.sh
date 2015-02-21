#!/bin/bash

STREAM="rtsp://68.152.51.100/axis-media/media.amp" # source stream
FREQ="1" # frequency in seconds
OUT_DIR="./images" # no trailing slash

TIME=$(date +%s) # gets the system time in seconds

if [ -d "OUT_DIR" ]; then
	mkdir $OUT_DIR
fi

avconv -i $STREAM -r $FREQ -vsync 1 -qscale 1 -f image2 ${OUT_DIR}/${TIME}-%09d.jpg

