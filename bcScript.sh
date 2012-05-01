#!/bin/bash

GITOUT=$(git pull)

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")
REV=$(git rev-parse HEAD)
REVSHORT=$(git rev-parse --short HEAD)

echo $REV
echo $REVSHORT

OUTPUT="log/$REVSHORT-$DATE-$HOST.txt"
OUTPUTLONG="log/$REVSHORT-$DATE-$HOST-long.txt"

echo "log file: $OUTPUT"

touch $OUTPUT
touch $OUTPUTLONG

$GITOUT >> $OUTPUTLONG

./sbt compile | tee $a $OUTPUTLONG

./sbt run | tee -a $OUTPUTLONG $OUTPUT
