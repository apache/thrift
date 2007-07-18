#!/bin/sh


if [ $# -lt 2 ]; then
	echo "usage: substitute.sh <variable to replace> <value to replace with> <filename | STDIN>"
	exit 1
fi

if [ $# -eq 3 ]; then
	VARIABLE=$1
	VALUE=$2
	FILENAME=$3
	
	echo "replacing $VARIABLE with $VALUE in $FILENAME"
	sed -e "s/$VARIABLE/$VALUE/" $FILENAME > "$FILENAME"_tmp 
	mv "$FILENAME"_tmp $FILENAME
	exit 0
fi

if [ $# -eq 2 ]; then
    while read line; 
    do
		VARIABLE=$1
		VALUE=$2
		FILENAME=${line}
	
		echo "replacing $VARIABLE with $VALUE in $FILENAME"
		sed -e "s/$VARIABLE/$VALUE/" $FILENAME > "$FILENAME"_tmp 
		mv "$FILENAME"_tmp $FILENAME
    done
    exit 0
fi

