#!/bin/bash

#gets the latest version from git
git pull

#packs everything in a jar
./sbt one-jar
#we remove the scala jar inside that anoys us
zip -d target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar lib/scala-*

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")

OUTPUT="benchmark/$DATE-$REVSHORT-$HOST.txt"
touch OUTPUT

#will now loop through all configs

for i in {0..15}
do
  echo "$i cores"
  if [ $# -eq 2 ]
  then
    taskset -c 0-$i java -jar target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar $1 $2 | tee -a $OUTPUT
  else
    taskset -c 0-$i java -jar target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar | tee -a $OUTPUT
  fi
done
