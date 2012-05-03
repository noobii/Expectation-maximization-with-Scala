#!/bin/bash

# A bit messy but it works !

#Gets the latest version from git
GITOUT=$(git pull)
#Gets the hostname of
HOST=$(hostname)
#Usable date
DATE=$(date +"%Y-%m-%d-%H_%M_%S")
#Revision number
REV=$(git rev-parse HEAD)
#Short revision number
REVSHORT=$(git rev-parse --short HEAD)

#Two output files, on for everything, one only with important info
OUTPUT="log/$REVSHORT-$DATE-$HOST.txt"
OUTPUTLONG="log/$REVSHORT-$DATE-$HOST-long.txt"
e
echo "log file: $OUTPUT"

#Creates the output files
touch $OUTPUT
touch $OUTPUTLONG

#Saves what was outputed from Git
echo $GITOUT >> $OUTPUTLONG

echo "Commit: $REV" >> $OUTPUTLONG $OUTPUT

#Saves who is on the computer
who | tee -a $OUTPUTLONG $OUTPUT

#Compiles the code, shows what is going on and saves it to long output
./sbt compile | tee -a $OUTPUTLONG

#Runs the code, shows what is going on and saves to both outputs
./sbt run | tee -a $OUTPUTLONG $OUTPUT
