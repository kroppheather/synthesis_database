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
library(rjags)
library(coda)
library(mcmcplots)
#set up directories
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\model\\run4"
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

siteinfo <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\siteinfo.csv")
colnames(siteinfo)[1] <- "siteid"
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


#join siteinfo in
vegeSP <- join(vegeSP, siteinfo, by="siteid",type="left")
vegeL<- join(vegeL, siteinfo, by="siteid",type="left")
vegeM <- join(vegeM, siteinfo, by="siteid",type="left")
unique(vegeSP$loc)
unique(vegeSP$site_name)



unique(vegeL$loc)
unique(vegeL$site_name)


unique(vegeM$loc)
unique(vegeM$site_name)



#######################################
#####aggregate vegetation         ##### 
#######################################

#need to add up all vegetation covers and normalize ones that add up to over
#100%. THose are likely based on canopy ground cover rather than mix
#of species abundance
percT <- aggregate(vegeSP$perc_cov, by=list(vegeSP$siteid), FUN="sum")
colnames(percT) <- c("siteid","percTot")

vegeSP <- join(vegeSP, percT, by="siteid", type="left")

#normalize sites over 100 percent
vegeSP$perc_covN <- ifelse(vegeSP$percTot >= 100, (vegeSP$perc_cov/vegeSP$percTot)*100,vegeSP$perc_cov)

#total up species cover for each site

totC <- aggregate(vegeSP$perc_covN, by=list(vegeSP$func_type,vegeSP$siteid), FUN="sum")
colnames(totC) <- c("func_type","siteid", "percCN")
#grab any shrub


#get all shrub observations
shrub <- totC[grepl("shrub",totC$func_type)==TRUE,]
#summ up all shrub observations
shrubA <- aggregate(shrub$percCN, by=list(shrub$siteid), FUN="sum")
colnames(shrubA) <- c("siteid","shrubC")

nonvascular <- totC[grepl("moss",totC$func_type)==TRUE|grepl("lichen",totC$func_type)==TRUE|grepl("liverwort",totC$func_type)==TRUE,]

nonvascularA <-  aggregate(nonvascular$percCN, by=list(nonvascular$siteid), FUN="sum")
colnames(nonvascularA) <- c("siteid","nonvascularC")

grass <- totC[grepl("gramminoid",totC$func_type)==TRUE|grepl("sedges",totC$func_type)==TRUE|grepl("grasses",totC$func_type)==TRUE|grepl("rush",totC$func_type)==TRUE|grepl("Tall sedges",totC$func_type)==TRUE,]


grassA <-  aggregate(grass$percCN, by=list(grass$siteid), FUN="sum")
colnames(grassA) <- c("siteid","grassC")

#join all together

coverAll <- join(shrubA, grassA, by="siteid", type="full")
coverAll <- join(coverAll, nonvascularA, by="siteid", type="full")

#grass is missing a lot of observations.

coverAll2 <- join(shrubA, nonvascularA, by="siteid", type="full")
#three sites are missing shrub cover values
#four different sites are missing moss cover

#the shrub missing values are from a study where onlly plot contents were recorded so can assume not there
coverAll2$shrubC <- ifelse(is.na(coverAll2$shrubC),0,coverAll2$shrubC)

#sites 65, and 67 do not have cover estimates for moss
#site 101 does not have moss
#site 190 no moss

coverAll2$nonvascularC[coverAll2$siteid==101] <- 0
coverAll2$nonvascularC[coverAll2$siteid==190] <- 0


#omit sites with missing cover data
coverAll2 <- na.omit(coverAll2)



#######################################
#####organize LAI NDVI data       ##### 
#######################################

#get counts of ndvi and lai measurements across sites to get a feel for data
datLAI <- datL[is.na(datL$lai)==FALSE,]
countL <- aggregate(datLAI$lai, by=list(datLAI$siteid), FUN="length")

datNDVI <- datL[is.na(datL$ndvi)==FALSE,]
countN <- aggregate(datNDVI$ndvi, by=list(datNDVI$siteid), FUN="length")
colnames(countN) <- c("siteid", "ndviCount")
datNDVI <- join(datNDVI,countN, by="siteid",type="left")
#subset out sites with only one measurement
datNDVIS <- datNDVI[datNDVI$ndviCount==1,]


datNDVIM <- datNDVI[datNDVI$ndviCount>1,]

#only take average NDVI during a period where typical ndvi is measured
#at maximumal vegetation activity
#166-227
datNDVIMG <- datNDVIM[datNDVIM$doy_m>=166&datNDVIM$doy_m<=227,]
#now aggregate by site
datNDVIAV <- aggregate(datNDVIMG$ndvi, by=list(datNDVIMG$siteid),FUN="mean")
colnames(datNDVIAV) <- c("siteid","ndvi")


#make a dataframe with all of the measurements
NDVI <- data.frame(siteid=c(datNDVIS$siteid,datNDVIAV$siteid),
					ndvi=c(datNDVIS$ndvi,datNDVIAV$ndvi))
					

#######################################
#####organize soil data           ##### 
#######################################


#create unique names for air
colnames(AirParm)[1:4] <- paste0("A",colnames(AirParm)[1:4])
colnames(AirParm)[8] <- paste0("A",colnames(AirParm)[8])
#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$Aparm)

#pull out relevant parameters
#for analysis: TminS, TmaxS, peakWS
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "peakWS") 

parmAs <- c("TminA","TmaxA", "peakWA") 


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmVs)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmVs[i]&SoilParm$depth<=20,]
	#calculate mean
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
	#add a regression ID
	SoilL[[i]]$regID <- rep(i,dim(SoilL[[i]])[1])
}

AirL <- list()
AirMs <- numeric(0)
for(i in 1:length(parmAs)){
	AirL[[i]] <- AirParm[AirParm$Aparm==parmAs[i],]
	#calculate mean
	AirMs[i] <- round(mean(AirL[[i]]$AMean),3)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}


#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)


#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")

#join each vegetation dataset to soil and air parameters

ParmPC <- join(ParmAll, coverAll2, by="siteid", type="inner")
ParmNDVI <- join(ParmAll, NDVI, by="siteid", type="inner")
ParmMLT <- join(ParmAll, datM, by="siteid", type="inner")

#need to join vegeclass data
ParmPC <- join(ParmPC, datV, by=c("siteid"), type="left")

#check how many observations in each vegeclass and regression
vegeCount <- aggregate(ParmPC$Mean, by=list(ParmPC$vegeclass,ParmPC$regID),FUN="length")
colnames(vegeCount) <- c("vegeclass","regID","count")
vegeCount$regvegeID <- seq(1,dim(vegeCount)[1])
#join back into parmpc
ParmPC <- join(ParmPC, vegeCount, by=c("vegeclass","regID"),type="left")



AirMean <- aggregate(ParmPC$AMean, by=list(ParmPC$regID), FUN="mean")
colnames(AirMean) <- c("regID", "Abar")

#generate dataset for monitoring regression means

mu.monitor <- data.frame(regID = rep(seq(1,3),each=100), 
				monitorAir = c(seq(-45,0, length.out=100),seq(0,35,length.out=100),seq(0,.65,length.out=100)),
				monitorDepth=rep(seq(0,20,length.out=100),times=3),
				monitorShrub=rep(seq(0,100,length.out=100),times=3),
				monitorMoss=rep(seq(0,80,length.out=100),times=3))


#######################################
#####organize model run           ##### 
#######################################	
datalist <- list( Nobs=dim(ParmPC)[1],SoilP=ParmPC$Mean,regvegeID=ParmPC$regvegeID,
				depth=ParmPC$depth, AirP=ParmPC$AMean,AirPbar=AirMean$Abar,
				regID=ParmPC$regID, sigMod=ParmPC$SD, sig.Air=ParmPC$ASD,
				Nregvege=dim(vegeCount)[1],shrubC=ParmPC$shrubC,
				mossC=ParmPC$nonvascularC,Nmonitor=dim(mu.monitor)[1],monitorAir=mu.monitor$monitorAir,
				monitordepth=mu.monitor$monitorDepth,EregID=mu.monitor$regID,
				monitorShrub=mu.monitor$monitorShrub,monitorMoss=mu.monitor$monitorMoss)

parms <- c("sig	SoilV","beta0","beta1","beta2","beta3","beta4","repSoilP",
			"mu.site.air","mu.site.depth","mu.site.shrub","mu.site.moss")


#organize data for the model
#start model 
vege.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\continuous_vege\\continuous_vege_model_codePC.r",
						data=datalist,
						n.adapt=10000,
						n.chains=3)

vege.sample <- coda.samples(vege.modI,variable.names=parms,
                       n.iter=60000, thin=30)	
					
#model history
mcmcplot(vege.sample, parms=c("sig	SoilV","beta0","beta1","beta2","a0","a1","a2","b0","b1","b2",	
			"c0","c1","c2"),
			dir=paste0(modDI,"\\history"))


Xcomp <- round(0.05/((3*3	*3)-1)	,3)	
#model output							   
mod.out <- summary(vege.sample,  quantiles = c(Xcomp,0.025, 0.25, 0.5, 0.75, 0.975,1-Xcomp))

write.table(mod.out$statistics,paste0(modDI,"\\vege_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(mod.out$quantiles,paste0(modDI,"\\vege_mod_quant.csv"),
			sep=",",row.names=TRUE)

#coda output
chain1<-as.matrix(vege.sample [[1]])
write.table(chain1,paste0(modDI,"\\chain1_coda.csv"), sep=",")
chain2<-as.matrix(vege.sample [[2]])
write.table(chain2,paste0(modDI,"\\chain2_coda.csv"), sep=",")
chain3<-as.matrix(vege.sample [[3]])
write.table(chain3,paste0(modDI,"\\chain3_coda.csv"), sep=",")				