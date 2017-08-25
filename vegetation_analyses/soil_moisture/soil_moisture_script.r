##########################################################
########Soil moisture analysis script          ###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script extracts soil moisture from the        ###
### database output, matches it to temperature parms   ###
### and runs a linear model to analyze patterns in     ###
### air and soil temperature coupling                  ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
##########################################################
##########################################################


##########################################
##first grab soil temperature parameters##
##########################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\vegetation_analyses\\temp_parm_extract.r")


###########################################
## read in soil moisture data from the db##
###########################################
#read in soil moisture
datVW <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil_moist.csv", na.strings=c("NaN","NA")) 

#read in soil dataset
Sdesc <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil.csv", na.strings=c("NaN","NA"))

#get a count of daily observations per site, year, depth

VWcount <- aggregate(datVW$swc, by=list(datVW$year_sm, datVW$sw_depth,datVW$site_id), FUN="length")
colnames(VWcount) <- c("year", "depth", "siteid", "count")

#get the unique number of sites with data

VWsiteall <- unique(VWcount$siteid)