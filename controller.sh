#!/bin/bash

## CONFIGURATION
myStream="rtsp://68.152.51.100/axis-media/media.amp" # video stream
myFfmpeg="avconv" # ubuntu replaced ffmpeg with avconv
myProcessor="./theapp.bin" # image processor
myTolerance="10" # determines what angles contribute to image score
myThreshold="26000" # minimum score for inclusion
myTweeter="ruby tweet.rb" # what to do when successful
mySleep="60" # how long to sleep between image captures
myTimeout="10800" # seconds between allowable tweets
myCacheTime="10800" # number of seconds an image is kept

function main {
  startTime=$(date -%s)

  ## INIT SOME DIRS AND VARS
  if [[ ! -d tmp/ ]]; then
    mkdir tmp/
  fi
  if [[ ! -d images/ ]]; then
    mkdir images/
  fi
  if [[ ! -d log/ ]]; then
    mkdir log/
  fi
  rollTime="0" # last time the tree was rolled (seconds since epoch)
  state="unrolled" # state of the corner

  ## START LOGGING
  log="log/${startTime}.log"
  echo "Log $log created by controller.sh." >> $log
  echo "Started logging at $(date)." >> $log

  ## START IMAGE SCRAPER
  $myFfmped -rtsp_transport tcp -y -i "$myStream" -r 1/$mySleep tmp/image%03d.png &
  echo "Image scraper $myFfmpeg started at $(date)." >> $log

  while true; do
  
    ## FIND AND SCORE THE LATEST IMAGE
    friendlyTime=$(date)
    currentTime=$(date +%s)
    image=$(ls -1t tmp/* | head -1)
    score=$($myProcessor $image $myTolerance)
    if [[ $score -gt $myThreshold ]]; then
      state="rolled"
    else
      state="unrolled"
    fi
    echo "At time $friendlyTime, image $image scored $score, interpreted as state $state." >> $log

    ## TO TWEET OR NOT TO TWEET
    let timeout=currentTime-rollTime
    if [[ $state == "rolled" ]]; then
      if [[ $timeout -gt $myTimeout ]]; then
        ## IT'S AWAY!
        $myTweeter $image &
        echo "Tweeted! $friendlyTime - $image - $score - $state." >> $log
      else
        ## NEGATIVE! IT DIDN'T GO IN.
        echo "Image was $state, but timeout $timeout was too small. Not tweeting." >> $log
      fi
      ## SOME BOOKKEEPING
      rollTime=$currentTime
      echo "Image was $state, so timeout was reset." >> $log
      cp $image images/${currentTime}-${score}.png
      echo "Image saved as images/${currentTime}-${score}.png." >> $log
    fi

    ## DELETE FRAMES OLDER THAN myCacheTime AND SLEEP
    find tmp/ -not -newermt "-$myCacheTime seconds" -delete
    sleep $mySleep
  done
}

main

