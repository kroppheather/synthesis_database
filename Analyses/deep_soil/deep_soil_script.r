##########################################################
########Deep soil measurement sites            ###########
########Heather Kropp started September 2017   ###########
##########################################################
##########################################################
### This script uses vegetation classes and biomes     ###
###  to look at patterns in deep soil                  ###
###  temperature and coupling with air                 ###
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

#libraries loaded in source script: plyr


##########################################
##first grab soil temperature parameters##
##########################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\deep_soil_parms_extract.r")

#read in vegetation class data
#note sites 199-222 not confirmed yet

datVC <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\vege_class\\new_class.csv")

#read in site info

datSite <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\siteinfo.csv",
						na.strings=c("NaN", "NA"))
						

#make sub table
newC <- data.frame(siteid=datVC$siteid, Vclass=datVC$new.class)

#make a subset site info table
Sitesub <- data.frame(siteid=datSite$site_id, biome=datSite$vege_z)

newC <- join(newC, Sitesub, by="siteid", type="left")

#join vegetation class to each parameter table
NfactorV <- join(Nfactor, newC, by="siteid", type="left")
SoilParmV <- join(SoilParm, newC, by="siteid", type="left")

#check to see how many observations in each vege class
ParmsCountVC <- aggregate(SoilParmV$Mean[SoilParmV$parm=="TmaxS"],
						by=list(SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]), FUN="length")
#see how many sites are unique
SiteCountLVC <-unique(data.frame(siteid=SoilParmV$siteid[SoilParmV$parm=="TmaxS"],
								Vclass=SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]))
#count how many sites in each class
SiteCountVC <- aggregate(SiteCountLVC$siteid, by=list(SiteCountLVC$Vclass), FUN="length")

#check the biome
#check to see how many observations in each vege class
ParmsCountB <- aggregate(SoilParmV$Mean[SoilParmV$parm=="TmaxS"],
						by=list(SoilParmV$biome[SoilParmV$parm=="TmaxS"]), FUN="length")
#see how many sites are unique
SiteCountLB <-unique(data.frame(siteid=SoilParmV$siteid[SoilParmV$parm=="TmaxS"],
								Vclass=SoilParmV$biome[SoilParmV$parm=="TmaxS"]))
#count how many sites in each class
SiteCountB <- aggregate(SiteCountLB$siteid, by=list(SiteCountLB$Vclass), FUN="length")

#make seperate data frames fore each soil parameter

datNF<-NfactorV [NfactorV $parm=="Fn",]
datNT<-NfactorV [NfactorV $parm=="Tn",]
datTmax<-SoilParmV[SoilParmV$parm=="TmaxS",]
datTmin<-SoilParmV[SoilParmV$parm=="TminS",]
datDZ<-SoilParmV[SoilParmV$parm=="DayZero",]
datPS<-SoilParmV[SoilParmV$parm=="peakSS",]
datPW<-SoilParmV[SoilParmV$parm=="peakWS",]

#now seperate out air to match
datTmaxA<-AirParm[AirParm$parm=="TmaxA",]
colnames(datTmaxA)[1:4]<-paste0(colnames(datTmaxA)[1:4],"A")
datTminA<-AirParm[AirParm$parm=="TminA",]
colnames(datTminA)[1:4]<-paste0(colnames(datTminA)[1:4],"A")
datPSA<-AirParm[AirParm$parm=="peakSA",]
colnames(datPSA)[1:4]<-paste0(colnames(datPSA)[1:4],"A")
datPWA<-AirParm[AirParm$parm=="peakWA",]
colnames(datPWA)[1:4]<-paste0(colnames(datPWA)[1:4],"A")

#now combine air measure with matching parm
datNF<-join(datNF, datTminA, by=c("siteid","height","wyear"), type="left")
datNT<-join(datNT, datTmaxA, by=c("siteid","height","wyear"), type="left")

datTmax<-join(datTmax, datTmaxA, by=c("siteid","wyear"), type="left")
datTmin<-join(datTmin, datTminA, by=c("siteid","wyear"), type="left")

datPS<-join(datPS,datPSA,by=c("siteid","wyear"), type="left")
datPW<-join(datPW,datPWA,by=c("siteid","wyear"), type="left")

datDZ<-join(datDZ,datTminA, by=c("siteid","wyear"), type="left")

datAll<-list(datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ)



###########