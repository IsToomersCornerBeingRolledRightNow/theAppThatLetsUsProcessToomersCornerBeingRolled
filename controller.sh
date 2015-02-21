#!/bin/bash

myGrabber="./grab.sh"
myStream="rtsp://68.152.51.100/axis-media/media.amp"
myImage="./tmp/image.bmp"
myProcessor="./compare.sh"
myAction="./tweet.sh"
mySleep="1"

if [[ -d ./tmp ]]; then
	rm -rf ./tmp
fi
mkdir ./tmp

myOldState="unrolled"

while true; do
  $myGrabber $myStream $myImage
  
  myNewState=$($myProcessor $myImage)
    
  if [[ $myOldState == "unrolled" && $myNewState == "rolled" ]]; then
    $myAction $myImage &
  fi
  
  myOldState=$myNewState
  rm $myImage
  sleep $mySleep
done

