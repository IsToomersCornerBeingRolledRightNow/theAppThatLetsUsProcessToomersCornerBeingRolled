#!/bin/bash

theFlip=$(($(($RANDOM%10))%2))

if [[ $theFlip -eq 0 ]]; then
  myReturn="rolled"
else
  myReturn="unrolled"
fi

echo $myReturn

