#!/bin/bash

# Date=$(date +%b-%d-%Y %H:%M)
Date=$(date +'%b-%d-%Y_%H%M')
# Scenario="${1}_$Date"
Scenario=$1
Scenario+="_"
Scenario+=$Date
mkdir $Scenario
cp -r BASINAVG1/* $Scenario
echo $Scenario
