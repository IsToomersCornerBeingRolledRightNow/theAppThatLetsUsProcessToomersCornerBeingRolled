#!/bin/bash

myStream="rtsp://68.152.51.100/axis-media/media.amp" # video stream
myImage="./tmp/image.bmp" # path to captured image
myFfmpeg="avconv" # ubuntu replaced ffmpeg with avconv
#wtfName="theAppThatLetsUsProcessToomersCornerBeingRolled" # wtf
#myProcessor="./dist/build/${wtfName}/${wtfName}" # image processor
myProcessor="./testing_processor.sh" #DEBUG
myTolerance="10" # determines what angles contribute to image score
myThreshold="26000" # minimum score for inclusion
myTweeter="./tweet.rb" # what to do when successful
myTweeter="./testing_tweeter.sh" #DEBUG
mySleep="1" # how long to sleep between image captures
myCooldownReset="10" # how many consecutive failures it takes to update state

function grab_image {
  if [[ -f $myImage ]]; then
    rm $myImage
  fi
  
  $myFfmpeg -i $myStream -t 1 -r 1 -vsync 1 -qscale 1 -f image2 $myImage
}

function score_image {  
  score=$($myProcessor $myImage $myTolerance)
  
  if [[ $score -gt $myThreshold ]]; then
    result="rolled"
  else
    result="unrolled"
  fi
  
  echo $result
}

function tweet_out {
  myOldImage=$myImage
  myNewImage=${myOldImage/%.bmp/.png}
  
  if [[ -f $myNewImage ]]; then
    rm $myNewImage
  fi
  
  $myFfmpeg -i $myOldImage $myNewImage
  $myTweeter $myNewImage &
}

function main {
  if [[ ! -d ./tmp ]]; then
    mkdir ./tmp
  fi
  
  cooldown="0"
  
  while true; do
    grab_image
    state=$(score_image)
    if [[ $state == "rolled" ]]; then
      if [[ $cooldown == "0" ]]; then
        tweet_out
      fi
      cooldown=$myCooldownReset
    fi
    
    if [[ $state == "unrolled" && $cooldown -gt "0" ]]; then
      let cooldown=cooldown-1
    fi
    
    sleep $mySleep
    rm $myImage
  done
}

main
