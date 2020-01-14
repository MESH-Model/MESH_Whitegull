# File Preparation Notes

This configuration is for the point mode model - 1 grid, 1 GRU at the Old Black Spruce site

## MESH_drainage_database.r2c
- Not needed -> point mode enabled via RUNMODE and SHDFILEFLAG

*However, still have an "old method" point mode version where:
This file was manually modified based on the r2c file from Herbert's setup.*

*The grid containing the basin was selected so that:*
- *The station and the streamflow gauge were both within the grid cell*
- *The xDelta=yDelta*
- *The grid in the file was 3x3*
- *See the document "DrainageDatabaseCalcs.xlsx" for more detail and for calculations.*

*Dummy values were used for:*
- *channel slope*
- *channel length*
- *IntSlope*
- *Chnl*
- *Reach*

*Elevation set to the surface elvation at the OBS tower; elevation of the outlet set to elevation at the streamflow gauge.*

*Note that "Grid Area" is the same as the total drainage area (DA), but in m2 instead of km2*

# MESH_input_reservoir.txt
- Not needed -> point mode enabled via RUNMODE and SHDFILEFLAG

# MESH_input_soil_levels.txt
- Preserved the default 3 soil layers

# MESH_input_streamflow
- Not needed -> point mode enabled via RUNMODE and SHDFILEFLAG

# MESH_Parameters_CLASS
- Lat/Long is of the Old Black Spruce station (53.98717, - 105.11779) http://giws.usask.ca/meta/Metadata_BERMS.html

# MESH_parameters_hydrology
- WFR2: default value shown in the MESH wiki example used

# MESH_input_run_options
- Control flags: same as Herbert and Bruce's WhiteGull setups (IPCP, ITCG, IDISP, IZREF)
  - Could play around with these
- IWF
  =0 to enable use flat CLASS (interflow = 0)
  =1 to enable WATROF (aka sloped CLASS)
- Start date: day 274 (Oct. 1)
- IDISP: CLASS manual (p. 24) states that value should be set to 1 for use of field studies


# Key Publications
- Barr et al 2012: doi:10.1016/j.agrformet.2011.05.017
- Barr et al 2006: doi:10.1016/j.agrformet.2006.08.007
- Jarvis et al. (1997) and Gower et al. (1997)
