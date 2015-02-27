#!/bin/bash

# Connects once every 15 seconds.
# Grabs one frame and disconnnects.

stream="rtsp://68.152.51.100/axis-media/media.amp" # source stream
freq="15" # frequency in seconds
# grabs="10" # number of frames to grab

#i="0"

#while [[ $i -lt $grabs ]]; do
while [[ true ]]; do

  timestamp=$(date +%s) # gets the system time in seconds

  avconv -i "$stream" -t 1 -r 1 -vsync 1 -qscale 1 -f image2 ./intermitent/${timestamp}.png
  
  #let i=i+1
  sleep 15
done

