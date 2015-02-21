#!/bin/bash

STREAM="rtsp://68.152.51.100/axis-media/media.amp" # source stream
FREQ="1" # frequency in seconds
OUT_DIR="./out" # no trailing slash

TIME=$(date +%s) # gets the system time in seconds

avconv -i $STREAM -r $FREQ -vsync 1 -qscale 1 -f image2 ${OUT_DIR}/${TIME}-%09d.jpg

