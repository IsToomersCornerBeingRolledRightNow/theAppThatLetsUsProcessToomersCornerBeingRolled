#!/bin/bash

# Establishes a persistent connection.
# Grabs one frame every 20 seconds.

stream="rtsp://68.152.51.100/axis-media/media.amp" # source stream
freq="20" # frequency in seconds
grabs="10" # number of frames to grab

timestamp=$(date +%s) # gets the system time in seconds

avconv -i "$stream" -t $grabs  -r $freq -vsync 1 -qscale 1 -f image2 ./persistent/${timestamp}-%09d.jpg
