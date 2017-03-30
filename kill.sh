#!/bin/bash
# script to kill z3, minismt and minisat orphans

while true; do
  sleep 4
  for pid in $(ps -elf | awk '{if ($5 == 1) {print $0}}' | grep "z3\|minismt\|minisat" | awk '{print $4}'); do 
    kill -9 $pid
  done
done

