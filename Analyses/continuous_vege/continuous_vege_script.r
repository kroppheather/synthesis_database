##########################################################
########Continuous vegetation soil temp        ###########
########Heather Kropp started July 2018        ###########
##########################################################
##########################################################
##########################################################
### This script uses vegetation cmeasures  in tundra   ###
### to look at patterns in                             ###
### air and shallow soil temperature coupling          ###
### focus only on key soil variables: min, max, and    ###
### the timing of the minimum                          ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### AirRepID,SoilRepID, datCSM(list), datCAM (list)    ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
##########################################################
##########################################################

#######################################
#####read in data                 ##### 
#######################################

                    
#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#load libraries

#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")

#read in continuous vege cover

datSP <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\spcov.csv")
#match datSP siteid name
colnames(datSP)[5] <- "siteid"

#read in lai/ndvi
datL <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\LAI.csv")

datM <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\moss.csv")
colnames(datM)[6] <- "siteid"
#join to vegetation info so that only sites with vegetation info are included

vegeSP <- join(datV,datSP, by=c("siteid"), type="right")
vegeL <- join(datV,datL, by=c("siteid"), type="right")
vegeM <- join(datV, datM, by=c("siteid"), type="right")


#look at only non wetland tundra classes (1-5)
vegeSP <- vegeSP[vegeSP$vegeclass <=5,]
vegeL <- vegeL[vegeL$vegeclass <=5,]
vegeM <- vegeM[vegeM$vegeclass <=5,]



#get count of number of sites
length(unique(vegeSP$siteid))
length(unique(vegeL$siteid))
length(unique(vegeM$siteid))



