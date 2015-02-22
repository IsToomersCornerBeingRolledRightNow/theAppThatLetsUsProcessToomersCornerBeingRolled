#!/bin/bash

echo $1

myOldImage="$1"
myNewImage="${myOldImage/%.bmp/.png}"

if [[ -f $myNewImage ]]; then
  rm $myNewImage
fi

avconv -i $myOldImage $myNewImage
xdg-open $myNewImage

notify-send "Rolled!" "Toomer's Corner is being rolled!"
echo "Rolled, bitches!"

exit 0

