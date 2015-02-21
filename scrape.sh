#!/bin/bash

STREAM="rtsp://68.152.51.100/axis-media/media.amp" # source stream
FREQ="1" # frequency in seconds
OUT_DIR="./out" # no trailing slash

avconv -i $STREAM -r $FREQ -vsync 1 -qscale 1 -f image2 ${OUT_DIR}/images%09d.jpg

