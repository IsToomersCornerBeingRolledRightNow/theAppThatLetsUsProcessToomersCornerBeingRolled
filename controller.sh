#!/bin/bash

## CONFIGURATION
myStream="rtsp://68.152.51.100/axis-media/media.amp" # video stream
myFfmpeg="avconv" # ubuntu replaced ffmpeg with avconv
myProcessor="./theapp.bin" # image processor
myTolerance="10" # determines what angles contribute to image score
myThreshold="26000" # minimum score for inclusion
myTweeter="ruby tweet.rb" # what to do when successful
mySleep="1" # how long to sleep between image captures
myTimeout="10800" # seconds between allowable tweets

function main {
  
  ## INIT SOME VARS
  if [[ ! -d ./tmp ]]; then
    mkdir ./tmp
  fi
  if [[ ! -d ./images ]]; then
    mkdir ./images
  fi
  rollTime="0" # last time the tree was rolled (seconds since epoch)
  state="unrolled" # state of the tree
  
  while true; do
    
    ## GRAB AN IMAGE
    if [[ -f ./tmp/image.bmp ]]; then
      rm ./tmp/image.bmp
    fi
    $myFfmpeg -i "$myStream" -t 1 -r 1 -vsync 1 -qscale 1 -f image2 ./tmp/image.bmp
    
    ## SCORE THE IMAGE
    score=$($myProcessor ./tmp/image.bmp $myTolerance)
    if [[ $score -gt $myThreshold ]]; then
      state="rolled"
    else
      state="unrolled"
    fi
    
    ## ARCHIVE THE IMAGE
    timestamp=$(date +%s)
    cp ./tmp/image.bmp ./images/${timestamp}-${score}-${state}.bmp
    echo "Image saved as ${timestamp}-${score}-${state}.bmp." >> ./tmp/controller.log
    
    ## TO TWEET OR NOT TO TWEET
    let timeSinceRolled=timestamp-rollTime
    if [[ $state == "rolled" ]]; then
      if [[ $timeSinceRolled -gt $myTimeout ]]; then
        ## WAR DAMN TWEET IT
        if [[ -f ./tmp/image.png ]]; then
          rm ./tmp/image.png
        fi
        $myFfmpeg -i ./tmp/image.bmp ./tmp/image.png # convert to png
        $myTweeter ./tmp/image.png & # and tweet
        echo "Tweeted! ${timestamp}-${score}-${state}." >> ./tmp/controller.log
      fi
      rollTime=$timestamp
      echo "Image passed, timeout reset." >> ./tmp/controller.log
    fi
    
    sleep $mySleep
  done
}

main
