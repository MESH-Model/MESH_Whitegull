# White Gull Creek Benchmark MESH Model

## Team
  Haley Brauner, Menna Elrashidy, Sujan Basnet, Andrew Ireson

## Date
Dec. 2019 through 2020

## Purpose
  To develop a benchmark MESH configuration for research modelling work in the White Gull Creek basin as part of a number of Master's projects.

## Objectives and Main Tasks
  *List the objectives of the modelling, as well as a summary of the project structure and tasks completed*

## Configuration Notes
Model/ReadMe.md
Model/Config_PointScale/ReadMe.md

## Basin Meta-data
*Include a link to the MESH Wiki "Basin Meta-data" page, if created. Otherwise, link to a "Basin_Metadata.md" file in the "Site" folder*

[Basin Meta-data on MESH Wiki Page](https://wiki.usask.ca/display/MESH/White+Gull+Creek+Basin) *Still to update*

[Basin Meta-data File](https://github.com/HaleyBrauner/MESH_Repo_Template/blob/master/Site/BasinMetaData.md) *Not yet populated*

## Progress / Conclusion
- Point mode at the OBS site is in progress as of Dec. 20, 2019
  - Will test out frozen soil algorithm
- GRU-based setup to be developed at a later date

### Data Sources used in Davison et al 2016
| Data Type     | Source | Notes  |
| :------------ |:-------------| :-----|
| Forcing      | OJP and/or OBS Data | Missing data filled based on regression relationship with other nearby BERMS towers |
| Landcover  | NRCan LCC200-V  | Can we get the processed data from somewhere?? |
| Topographic | CDED 1:50,000  |     |
| Soil texture | AAFC | |
| Grid resolution | 15km x 15 km | |

## References
- List any scripts, packages, etc. used
- Related research papers and other information (theory, parameter selection, etc.); could include a copy in Model>Justification folder
- Manuals (ex. CLASS, Ostrich, MESH Wiki, other)

___
___
# Folder Structure
Given the file size limitations of GitHub, only smaller files are stored here and the rest are stored on Graham. The files can be synced with the local machine via the respective push/pull bash scripts included in the Data/Raw and Model folders.

## Code
- Includes pre- and post-processing scripts used in the project

## Data

### Raw
- Include raw data files here. It is best to change the permissions to "Read Only".

### Processed
- Includes processed driving data, spatial data, and validation data (ex. streamflow).

#### Driving
- *Processed driving data used in the model*
- *Scripts used to generate the files should be included in the "Code" folder*

#### Spatial
- *Ex. GIS files, Green Kenue files*

#### Validation
- *ex. streamflow*

## Model
- *The ReadMe files should give an overview of the modelling methodology, as well as the differences between scenarios/runs*
- *Include model notes in this main "Model" folder, as available*

### Justification
- *Include files related to scenario configuration choice, parameter selection, initial conditions, etc.*

### MESH_Code
- *Holds the MESH code used for running the mode; use a new folder for each version (include the version as a suffix)*
- *If any modification of the code were made, or more than one version was used, include a text file listing the MESH code versions, the main differences and reason for use, and details of the modifications*

### Ostrich
- *This folder contains a copy of the Ostrich program (uncompiled); be sure to put the compiled file in .gitignore*

### ConfigurationX*
*1 folder for each configuration, each with input and output sub-folders*

- Input  
  - *Includes the MESH inputs files, driving data files / symbolic links, and scripts for running the model*

- Output
  - *Includes the output files for the model configuration / each run*

## Papers
- *Contains literature used to inform the model configuration*

## Presentations
- *Contains powerpoint (or similar), poster, or other presentations or reports related to the project (formal or otherwise)*

## Site
*Can include:
- Maps
- PhotosVideos
- Site Meta-data
- etc.*
