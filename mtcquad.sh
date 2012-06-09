#!/bin/bash

#gets the latest version from git
git pull

#packs everything in a jar
./sbt one-jar
#we remove the scala jar inside that annoys us
zip -d target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar lib/scala-*

HOST=$(hostname)
DATE=$(date +"%Y-%m-%d-%H_%M_%S")

RES="benchmark/$DATE-$REVSHORT-$HOST/"
mkdir $RES

OUTPUT="$RES/log.txt"
touch OUTPUT

#will now loop through all configs

for i in {0..7}
do
  echo "$i cores"
  
  mkdir "$RES/$i"
  
  
  if [ $# -eq 2 ]
  then
    taskset -c 0-$i java -jar target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar $1 $2 | tee -a $OUTPUT
  else
    taskset -c 0-$i java -jar target/scala-2.9.2/em_2.9.2-0.1-SNAPSHOT-one-jar.jar | tee -a $OUTPUT
  fi

#move the benchmark results
mv benchmark/*.txt "$RES/$i"

done
