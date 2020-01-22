# File Preparation Notes

This configuration is for the point mode model - 1 grid, 1 GRU at the Old Black Spruce site

## MESH_drainage_database.r2c
- Not needed -> point mode enabled via RUNMODE=noroute and SHDFILEFLAG=2 in *MESH_input_run_options.ini* file

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

*Elevation set to the surface elevation at the OBS tower; elevation of the outlet set to elevation at the streamflow gauge.*

*Note that "Grid Area" is the same as the total drainage area (DA), but in m2 instead of km2*

# MESH_input_reservoir.txt
- Not needed -> point mode enabled via RUNMODE=noroute and SHDFILEFLAG=2 in *MESH_input_run_options.ini* file

# MESH_input_soil_levels.txt
- Preserved the default 3 soil layers (0.10, 0.25, and 3.75 m thicknesses)

# MESH_input_streamflow
- Not needed -> point mode enabled

# MESH_Parameters_CLASS
- Lat/Long is of the Old Black Spruce station (53.98717, - 105.11779) http://giws.usask.ca/meta/Metadata_BERMS.html

# MESH_parameters_hydrology
- WFR2: default value shown in the MESH wiki example used

# MESH_input_run_options
- Control flags: same as Herbert and Bruce's WhiteGull setups (IPCP, ITCG, IDISP, IZREF)
  - Could play around with these
- IWF
  = 0 to enable use flat CLASS (interflow = 0)
  (could use = 1 to enable WATROF (aka sloped CLASS))
- Start day: 274 (Oct. 1)
- Start and end years:
- IDISP: CLASS manual (p. 24) states that value should be set to 1 for use of field studies


# Key Publications
- Barr et al 2012: doi:10.1016/j.agrformet.2011.05.017
- Barr et al 2006: doi:10.1016/j.agrformet.2006.08.007
- Jarvis et al. (1997) and Gower et al. (1997)
- Gray et al 2001 [](https://www.usask.ca/hydrology/papers/Gray_et_al_2001_2.pdf)
- FROZEN algorithm guidance document on the wiki: [](https://wiki.usask.ca/display/MESH/MESH_input_run_options.ini?preview=%2F424738901%2F507019274%2FFROZEN.pdf)
- Davison et al 2016: doi: 10.1175/JHM-D-15-0172.1
