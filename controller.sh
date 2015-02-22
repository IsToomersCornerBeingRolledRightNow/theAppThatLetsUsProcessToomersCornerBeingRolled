#!/bin/bash

myStream="rtsp://68.152.51.100/axis-media/media.amp"
myImage="./tmp/image.bmp"
myProcessor="./compare.sh"
myAction="./tweet.rb"
mySleep="1"

function grab {
  # $1 is the video stream
  # $2 is the output file

  if [[ -f $2 ]]; then
    rm $2
  fi
  
  avconv -i "$1" -t 1 -r 1 -vsync 1 -qscale 1 -f image2 "$2"
}

function process {
  # $1 is the image processor
  # $2 is the captured image
  
  myResult=$($1 $2)
  
  if [[ $myResult -eq 0 ]]; then
    myReturn="rolled"
  else
    myReturn="unrolled"
  fi
  
  echo $myReturn
}

function act {
  # $1 is the action to be taken
  # $2 is the captured image

  myOldImage="$2"
  myNewImage="${myOldImage/%.bmp/.png}"
  
  if [[ -f $myNewImage ]]; then
    rm $myNewImage
  fi
  
  avconv -i $myOldImage $myNewImage
  $1 $myNewImage
}

function main {

  if [[ -d ./tmp ]]; then
    rm -rf ./tmp
  fi
  mkdir ./tmp

  myOldState="unrolled"

  while true; do
    grab $myStream $myImage
    process $myProcessor $myImage
    if [[ $myOldState == "unrolled" && $myNewState == "rolled" ]]; then
      act $myAction $myImage &
    fi
    
    myOldState=$myNewState
    sleep $mySleep
    rm $myImage
  done
}

main

