#!/bin/bash

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")
REV=$(git rev-parse)
REVSHORT=$(git rev-parse --short)

OUTPUT="log/$REVSHORT-$DATE-$HOST.txt"
OUTPUT-LONG="log/$REVSHORT-$DATE-$HOST-long.txt"

echo "log file: $OUTPUT"

touch $OUTPUT
touch $OUTPUT-LONG

git pull | tee -a $OUTPUT-LONG
./sbt compile | tee $a $OUTPUT-LONG

./sbt run | tee -a $OUTPUT-LONG $OUTPUT
