#!/bin/bash

DATE=$(date +"%Y-%m-%d-%H_%M_%S")
echo "$DATE: Running script..."
./sbt compile
./sbt run | tee "$DATE.txt"
