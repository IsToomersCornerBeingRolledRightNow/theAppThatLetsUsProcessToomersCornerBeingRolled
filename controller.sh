#!/bin/bash

myStream="rtsp://68.152.51.100/axis-media/media.amp" # video stream
myImage="./tmp/image.bmp" # path to captured image
myFfmpeg="avconv" # ubuntu replaced ffmpeg with avconv
wtfName="theAppThatLetsUsProcessToomersCornerBeingRolled" # wtf
myProcessor="./dist/build/${wtfName}/${wtfName}" # image processor
myTolerance="10" # determines what angles contribute to image score
myThreshold="26000" # minimum score for inclusion
myTweeter="ruby tweet.rb" # what to do when successful
mySleep="1" # how long to sleep between image captures
myTweetTimeout="10800" # seconds between allowable tweets

function grab_image {
  if [[ -f $myImage ]]; then
    rm $myImage
  fi
  
  $myFfmpeg -i $myStream -t 1 -r 1 -vsync 1 -qscale 1 -f image2 $myImage
}

function score_image {  
  score=$($myProcessor $myImage $myTolerance)
  echo $score
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
  if [[ ! -d ./images ]]; then
    mkdir ./images
  fi
  
  rollTime="0"
  state="unrolled"
  
  while true; do
    #echo "rollTime = ${rollTime}" >> ./tmp/testing.log #DEBUG
    grab_image
    score=$(score_image)
    
    if [[ $score -gt $myThreshold ]]; then
      state="rolled"
    else
      state="unrolled"
    fi
    
    timestamp=$(date +%s)
    #echo "timestamp = ${timestamp}" >> ./tmp/testing.log #DEBUG
    let timeSinceRolled=timestamp-rollTime
    #echo "timeSinceRolled = ${timeSinceRolled}" >> ./tmp/testing.log #DEBUG
    cp $myImage ./images/${timestamp}-${score}-${state}.bmp
    echo "images saved as ${timestamp}-${score}-${state}.bmp" >> ./tmp/testing.log

    #echo "state = ${state}" >> ./tmp/testing.log #DEBUG
    if [[ $state == "rolled" ]]; then
      if [[ $timeSinceRolled -gt $myTweetTimeout ]]; then
        tweet_out
      fi
      rollTime=$timestamp
    fi
    
    sleep $mySleep
    rm $myImage
  done
}

main
