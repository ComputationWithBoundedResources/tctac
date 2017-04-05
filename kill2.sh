#!/bin/bash
# script to kill z3, minismt and minisat orphans

TIMEOUT=1

IFS=$'\n'       # make newlines the only separator


while true; do
  sleep 10
  for line in $(ps -aux | grep "z3\|minismt\|minisat"); do
	time=$(echo $line | awk '{print $10}' | cut -d':' -f1)
	if [ $time -ge $TIMEOUT ]; then
		pid=$(echo $line | awk '{print $2}')
		echo "Killed $pid with time: $time"
    	kill -9 $pid
	fi	
  done
done

