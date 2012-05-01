#!/bin/bash

#Gets the latest version from git
GITOUT=$(git pull)

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")
REV=$(git rev-parse HEAD)
REVSHORT=$(git rev-parse --short HEAD)


OUTPUT="log/$REVSHORT-$DATE-$HOST.txt"
OUTPUTLONG="log/$REVSHORT-$DATE-$HOST-long.txt"

echo "log file: $OUTPUT"

#Creates the output files
touch $OUTPUT
touch $OUTPUTLONG

#Saves what was outputed from Git
echo $GITOUT >> $OUTPUTLONG

echo "Commit: $REV" >> $OUTPUTLONG $OUTPUT

#Saves who is on the computer
who | tee -a $OUTPUTLONG $OUTPUT

./sbt compile | tee -a $OUTPUTLONG

./sbt run | tee -a $OUTPUTLONG $OUTPUT
