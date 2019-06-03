##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in                   ###
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
### DDsoil, DDair: degree days                         ###
##########################################################


#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")



#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")

#######################################
#####libraries                    ##### 
#######################################

library(plyr)

#######################################
#####organize data                ##### 
#######################################
#join vegeclass to data

DDsoil <- join(DDsoil,datV, by="siteid",type="left")
DDair <- join(DDair,datV, by="siteid",type="left")
AirParm <- join(AirParm,datV, by="siteid",type="left")

plot(DDair$Mean[DDair$parm=="FDDA"]~as.factor(DDair$vegeclass[DDair$parm=="FDDA"]))


#plot air temps
par(mfrow=c(2,1))
plot(AirParm$Mean[AirParm$parm=="TmaxA"]~as.factor(AirParm$vegeclass[AirParm$parm=="TmaxA"]),
		xlab = "vegeclass", ylab="Maximum air temperature")
		
abline(h=12, col="tomato3",lwd=2, lty=3)		
plot(DDair$Mean[DDair$parm=="TDDA"]~as.factor(DDair$vegeclass[DDair$parm=="TDDA"]),
		xlab = "vegeclass", ylab="Thawing degree days")
