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

datSM<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil_moist.csv", na.strings=c("NaN","NA")) 