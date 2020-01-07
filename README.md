# An analysis of air-soil coupling in the permafrost region
#### code edited by Heather Kropp
A repository associated with a Pan-Arctic vegetation and air-soil temperature data synthesis of 235 sites across the permafrost region. This repository contains setup to the permafrost vegetation synthesis database and data analyses. Statistical models of annual temperature were fit to a subset of sites that met basic standards and analyzed for patterns between plant functional types.
![Sites in pan-Arctic soil temperature database](https://github.com/kroppheather/synthesis_database/blob/master/documentation/Supp_vege_site_agg_all.png)

# Guide to repository 
## Temp_model
Statistical model for raw temperature data that fits a phenomenological model that describes annual variability in daily temperature. This model fills in missing data (only sites with <25% of a year missing included) and estimates parameters that describe annual variability in temperature such as maximum and minimum temperatures. Sites were run in a linux HPC environment.

## Analyses
Statistical analysis of model parameters and annual summary statistics that uses output from the Temp_model run. Analysis include patterns in temperature parameters across vegetation type and seasons. 

## documentation
Contains database schema and descritption of data tables for data used in synthesis.

## archive
Contains scripts from analyses not included in the paper for openness in analysis that could not be included in manuscript writing. Many of these analysis were abandoned due to low data availability in plant functional types that limited the geographic scope.  
