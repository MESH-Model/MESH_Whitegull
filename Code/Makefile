# This makefile runs the python notebook code to convert the raw data into processed driving data for the POINT MODE MESH Model

objects1 = ../Data/Processed/Driving/basin_humidity.csv\
     ../Data/Processed/Driving/basin_longwave.csv\
     ../Data/Processed/Driving/basin_pres.csv\
		 ../Data/Processed/Driving/basin_rain.csv\
		 ../Data/Processed/Driving/basin_shortwave.csv\
		 ../Data/Processed/Driving/basin_temperature.csv\
		 ../Data/Processed/Driving/basin_wind.csv

objects2 = ForcingFilePrep.ipynb

# If the notebook or files have chnaged, run the notebook to generate the 7 forcing data files for the model (csv)

# Note: select one of the output files as the target; putting in a "dummy" target will run the makefile every time make is called, whether or not the prerequesite files have changed or not since the last run

$(objects1): $(objects2)
	runipy ForcingFilePrep.ipynb
	echo "Driving Data Makefile Run: Success"
