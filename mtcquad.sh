#!/bin/bash

#gets the latest version from git
git pull

#packs everything in a jar
./sbt one-jar

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")

OUTPUT="benchmark/$DATE-$REVSHORT-$HOST.txt"

#will now loop through all configs

for i in {0..15}
do
  echo "$i cores"
  taskset -c 0-$i java -jar target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar | tee -a $OUTPUT
done
