#!/bin/bash

myStream="rtsp://68.152.51.100/axis-media/media.amp" # video stream #PRODUCTION
myImage="tmp/image.jpg" # path to captured image
myFfmpeg="avconv" # ubuntu replaced ffmpeg with avconv
wtfName="theAppThatLetsUsProcessToomersCornerBeingRolled" # wtf
myProcessor="./dist/build/${wtfName}/${wtfName}" # image processor
myTolerance="10" # determines what angles contribute to image score
myThreshold="26000" # minimum score for inclusion #PRODUCTION
myTweeter="ruby tweet.rb" # what to do when successful
mySleep="1" # how long to sleep between image captures
myTweetTimeout="10800" # seconds between allowable tweets

scriptStartTime=$(date +%s)
imagePath="examplevideos/fall-2015/images0000000"

function grab_image {
  if [[ -f $myImage ]]; then
    rm $myImage
  fi
  
  grabTime=$(date +%s)
  let scriptRunTime=grabTime-scriptStartTime+12
  targetImage="${imagePath}${scriptRunTime}.jpg"

  #echo "cp ${targetImage} ${myImage}"
  cp $targetImage $myImage
}

function score_image {  
  score=$($myProcessor $myImage $myTolerance)
  echo $score
}

function tweet_out {
  #myOldImage=$myImage
  #myNewImage=${myOldImage/%.bmp/.png}
  
  #if [[ -f $myNewImage ]]; then
  #  rm $myNewImage
  #fi
  
  #$myFfmpeg -i $myOldImage $myNewImage
  $myTweeter $myImage &
  echo "WAR DAMN TWEETED!"
  exit 0
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
    eog $myImage 2> /dev/null &
    score=$(score_image)
    echo $score
    sleep 2
    kill %1

    if [[ $score -gt $myThreshold ]]; then
      state="rolled"
    else
      state="unrolled"
    fi
    
    timestamp=$(date +%s)
    #echo "timestamp = ${timestamp}" >> ./tmp/testing.log #DEBUG
    let timeSinceRolled=timestamp-rollTime
    #echo "timeSinceRolled = ${timeSinceRolled}" >> ./tmp/testing.log #DEBUG
    #cp $myImage ./images/${timestamp}-${score}-${state}.bmp
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
