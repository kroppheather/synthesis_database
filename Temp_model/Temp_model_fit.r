##########################################################
##########################################################
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
### ThawParm: # of days above or at zero               ###
##########################################################
##########################################################


#######################################
#####read in data                 ##### 
#######################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")



#get unique sites
sitesAll <- unique(SoilRepID$siteid)
soilL <- list()
soilRepIDL <- list()
#subset data into list
for(i in 1:length(sitesAll)){
	soilL[[i]] <- datSM[datSM$siteid==sitesAll[i],]
	soilRepIDL[[i]] <- SoilRepID[SoilRepID$siteid==sitesAll[i],]
	soilRepIDL[[i]]$MeasT <- soilL[[i]]$T[soilRepIDL[[i]]$repID]
}

repCompS <- ldply(soilRepIDL,data.frame)

plot(repCompS$MeasT,repCompS$Mean)

fitS <- lm(repCompS$Mean~repCompS$MeasT)

summary(fitS)
abline(0,1,col="red")
abline(fitS,col="cornflowerblue")


airL <- list()
airRepIDL <- list()
#subset data into list
for(i in 1:length(sitesAll)){
	airL[[i]] <- datAM[datAM$siteid==sitesAll[i],]
	airRepIDL[[i]] <- AirRepID[AirRepID$siteid==sitesAll[i],]
	airRepIDL[[i]]$MeasT <- airL[[i]]$A[airRepIDL[[i]]$repID]
}

repCompA <- ldply(airRepIDL,data.frame)

plot(repCompA$MeasT,repCompA$Mean)

fitA <- lm(repCompA$Mean~repCompA$MeasT)

summary(fitA)
abline(0,1,col="red")
abline(fitS,col="cornflowerblue")