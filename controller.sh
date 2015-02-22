#!/bin/bash

myStream="rtsp://68.152.51.100/axis-media/media.amp"
myImage="./tmp/image.bmp"
myProcessor="./compare.sh"
myAction="./tweet.sh"
mySleep="1"

function grab {
  # $1 is the video stream
  # $2 is the output file

  if [[ -f $2 ]]; then
    rm $2
  fi
  
  avconv -i "$1" -t 1 -r 1 -vsync 1 -qscale 1 -f image2 "$2"
  return 0
}

function compare {
  # $1 is the image processor
  # $2 is the captured image
  
  myResult=$($1 $2)
  
  if [[ $myResult -eq 0 ]]; then
    myReturn="rolled"
  else
    myReturn="unrolled"
  fi
  
  echo $myReturn
  return 0
}

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
  sleep $mySleep
  rm $myImage
done

