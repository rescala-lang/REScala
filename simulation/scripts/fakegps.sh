#!/bin/bash

while true
do
    #streaming_pos_dump | while read -r line
    position_dump | while read -r line
    do
        echo GOT $line
        FILENAME=$(echo $line | cut -d '=' -f 1)
        COORD=$(echo $line | cut -d '=' -f 2 | cut -d ',' -f 1,2)
        echo $COORD > /tmp/$FILENAME.xy
    done
    sleep 1
done