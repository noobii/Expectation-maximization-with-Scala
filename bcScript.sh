#!/bin/bash

DATE=$(date +"%Y-%m-%d-%H_%M_%S")
echo "$DATE: Running script..."
git pull
./sbt compile
./sbt run | tee "$DATE.txt"
