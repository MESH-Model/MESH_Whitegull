#!/bin/bash
# Sample Slurm Script for use with OpenMPI on Plato
# Begin Slurm directives with #SBATCH

#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=4G
#SBATCH --time=1:00:00
#SBATCH --job-name=RunAll

echo "Starting run at: `date`"

./RunAll.sh

echo "Program finished with exit code $? at: `date`"
exit 0
