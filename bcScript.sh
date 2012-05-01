#!/bin/bash

git pull

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")
REV=$(git rev-parse)
REVSHORT=$(git rev-parse --short)

echo $REV
echo $REVSHORT

OUTPUT="log/$REVSHORT-$DATE-$HOST.txt"
OUTPUTLONG="log/$REVSHORT-$DATE-$HOST-long.txt"

echo "log file: $OUTPUT"

touch $OUTPUT
touch $OUTPUTLONG

./sbt compile | tee $a $OUTPUTLONG

./sbt run | tee -a $OUTPUT-LONG $OUTPUT
