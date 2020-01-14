#!/bin/bash

set -e # Will cause the script to stop if it encounters an error

# Remove existing output files (-f in case they don't exist)
rm -f BASINAVG1/* CLASSOUT/* abserr.txt drms.txt function_out.txt Metrics_Out.txt MonteCarlo.txt NS*.txt

# Create the BASINAVG1 folder if it doesn't exist
# if [[ ! -f BASINAVG1 ]] ; then
#   mkdir BASINAVG1
# fi

# Obtain start date/time and time in seconds for diff
DateStart=$(date)
START=$(date +%s)
echo "===================="
echo "MESH starting at $DateStart"
echo "===================="

# Run MESH
../MESH_Code/r1593/sa_mesh || true

# Obtain end date/time and time in seconds for diff

DateEnd=$(date)
END=$(date +%s)
DIFF=$(( $END - $START ))
echo "===================="
echo "The model ended at $DateEnd"
echo "The model took $DIFF seconds to run"
echo "===================="
echo ""
