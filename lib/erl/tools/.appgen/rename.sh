#!/bin/sh

if [ $# -eq 3 ]; then
	OLD_PREFIX=$1
	NEW_PREFIX=$2
	FILENAME=$3
	NEW_FILENAME=$(echo $FILENAME | sed -e "s/$OLD_PREFIX/$NEW_PREFIX/")
	echo "moving $FILENAME to $NEW_FILENAME"
	mv $FILENAME $NEW_FILENAME
	exit 0;
fi


if [ $# -eq 2 ]; then
	while read line; 
	do 
		OLD_PREFIX=$1
		NEW_PREFIX=$2
		NEW_FILENAME=$(echo ${line} | sed -e "s/$OLD_PREFIX/$NEW_PREFIX/")
		echo "moving ${line} to $NEW_FILENAME"
		mv ${line} $NEW_FILENAME
	done
	exit 0
fi

