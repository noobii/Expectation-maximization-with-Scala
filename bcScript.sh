#!/bin/bash

DATE=`date`

./sbt run | tee $DATE
