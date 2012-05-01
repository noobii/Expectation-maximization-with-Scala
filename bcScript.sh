#!/bin/bash

DATE=$(date +"%Y-%m-%d-%H_%M_%S")
echo $DATE
./sbt run | tee "$DATE"
