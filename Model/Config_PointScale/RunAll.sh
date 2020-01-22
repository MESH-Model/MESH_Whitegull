#!/bin/bash

# ===================================================================================================================================
# This file executes pre-processing scripts and makefiles, runs MESH, and then runs post-processing scripts to generate outputs.

# ****TO RUN****: A scenario or run identifier should be passed as an arguement when calling the script (example: ./RunAll.sh ScenarioA ). A folder with this name will be created, and the outputs copied there after running MESH.

# ===================================================================================================================================

set -e # Will cause the script to stop if it encounters an error

# 1. Obtain the directory where this script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# 2. Process the driving data by running the python notebook file via the makefile in the "Code" folder
cd $DIR
cd ../../Code

if pip list | grep -o 'runipy' ; then
  echo "runipy exists"
else
  echo "runipy does not exist"
  pip install runipy # installs runipy (to execute the jupyter notebook) if not already installed
fi
make

# 3. Run the makefile to update the CLASS.ini and hydrology.ini files based on the "ParamValues.csv" file, and copy the files to the "Model/Scenario_X/Input" folder
# cd $DIR
# cd ../../Code
# Rscript Find_Replace_in_Text_File.R
# cp MESH_parameters_CLASS.ini MESH_parameters_CLASS.tpl MESH_parameters_hydrology.ini MESH_parameters_hydrology.tpl $DIR/Input

# 4. Create symbolic links to the driving data
cd $DIR
pwd

if [[ $(ls -l *.csv | wc -l) -ne 0 ]] ; then
  rm basin_*.csv
fi

ln -s ../../Data/Processed/Driving/basin_humidity.csv basin_humidity.csv
ln -s ../../Data/Processed/Driving/basin_longwave.csv basin_longwave.csv
ln -s ../../Data/Processed/Driving/basin_pres.csv basin_pres.csv
ln -s ../../Data/Processed/Driving/basin_rain.csv basin_rain.csv
ln -s ../../Data/Processed/Driving/basin_shortwave.csv basin_shortwave.csv
ln -s ../../Data/Processed/Driving/basin_temperature.csv basin_temperature.csv
ln -s ../../Data/Processed/Driving/basin_wind.csv basin_wind.csv

# and change the executable file permissions
chmod +x run_mesh.sh

# 5. Check if MESH is compiled, and if not, compile with "make"
cd $DIR

# a)
cd ../MESH_Code/r1606
if [[ ! -x sa_mesh ]] ; then # If sa_mesh is not found or not executable
  make
fi

# b)
# cd $DIR
# cd ../Ostrich
# if [[ ! -x OstrichGCC ]] ; then # If OstrichGCC is not found or not executable
#   make GCC
# fi

# 6. Run MESH
cd $DIR

  # a) To run on local machine
./run_mesh.sh
    # Should really move the code from ./run_mesh.sh here to avoid duplication

  # b) To run on Plato or Graham
# ./submitjob.sh

  # c) To submit the job to SLURM
# sbatch submitjob.sh

# 7. Copy the outputs from BASINAVG1 to a new folder named for the scenario given when running
Date=$(date +'%b-%d-%Y_%H%M')
Scenario=$1
Scenario+="_"
Scenario+=$Date
mkdir $Scenario
cp -r BASINAVG1/* MESH_*.ini $Scenario

echo "The model results are saved in folder $Scenario"

# 7. Run the post-processing script
  # First, copy the file from the "Code" folder to the results folder
cd $DIR
cp ../../Code/MESHPostProcess.ipynb $Scenario

  # BROKEN - need to run the post processing notebook from Jupyter directly (from the restuls folder)
  # Then navigate into the folder and run the script
# cd $DIR/$Scenario
# runipy MESHPostProcess.ipynb
# jupyter nbconvert --execute --allow-errors MESHPostProcess.ipynb
