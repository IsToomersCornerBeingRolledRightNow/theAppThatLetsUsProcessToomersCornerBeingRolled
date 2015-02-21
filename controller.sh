#!/bin/bash

myStream="rtsp://68.152.51.100/axis-media/media.amp"
myProcessor="random.sh"
myAction="tweet.sh"
mySleep="50"

if [ -d ./tmp ]; then
	rm -Rf ./tmp
fi
mkdir ./tmp

myOldState="unrolled"

while true; do
  myDate=$(date +%s)
  avconv -i $myStream -t 1 -r 1 -vsync 1 -qscale 1 -f image2 ./tmp/${myDate}.bmp
  myImage=./tmp/${myDate}.bmp
  
  if [ $myProcessor $myImage ]; then
    myNewState="rolled"
  else
    myNewState="unrolled"
  fi
  
  if [ $myOldState="unrolled" && $myNewState="rolled" ]; then
    $myAction $myImage
  fi
  
  myOldState=$myNewState
  rm $myImage
  sleep $mySleep
done

