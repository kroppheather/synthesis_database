##########################################################
########Vegetation interannual soil temp       ###########
########Heather Kropp started August 2018      ###########
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
##########################################################
##########################################################


#######################################
#####read in data                 ##### 
#######################################

                    
#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")


#######################################
#####libraries                    ##### 
#######################################
library(rjags)
library(coda)
library(mcmcplots)
library(plyr)


#######################################
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\plots\\model"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run6"
Nrun <- 6
#indicate if a model run is occuring
modRun <- 1


#######################################
#####organize data                ##### 
#######################################


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)


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


#get number of years for each site and depth

YearAll <- unique(data.frame(siteid=ParmAll$siteid,depth=ParmAll$depth,wyear=ParmAll$wyear))
#count the number of years for each site and depth
YearCount <- aggregate(YearAll$wyear, by=list(YearAll$depth,YearAll$siteid), FUN="length")
colnames(YearCount) <- c("depth","siteid","nYear")
#subset sites with at least 2 years
YearSub <- YearCount[YearCount$nYear >=2,]

#join vegeclass to see how many
YearSub <- join(YearSub,datV, by="siteid",type="left")
#find out number of sites in each vegeclass
SubsiteN <- unique(data.frame(siteid=YearSub$siteid,vegeclass=YearSub$vegeclass))
SubsiteN <- SubsiteN[order(SubsiteN$vegeclass),]
SubdepthN <-  unique(data.frame(depth=YearSub$depth,vegeclass=YearSub$vegeclass))
SubdepthN <- SubdepthN[order(SubdepthN$vegeclass),]

#find out how many sites in each vegeclass
siteNl <- aggregate(SubsiteN$siteid, by=list(SubsiteN$vegeclass), FUN="length")
colnames(siteNl) <- c("vegeclass","count")
#most vegeclasses only have 1-3 sites with at least 2 years of data
#need to subset vege classes to focus on ones with more sites
#subset so that there are at least 5 different sites in a vegeclass
siteNl <- siteNl[siteNl$count>=5,]
#join back into Yearsub
YearSub <- join(YearSub, siteNl, by="vegeclass", type="inner")
#make a dataframe with only info needed for joining
YearSub2 <- data.frame(depth=YearSub$depth, siteid=YearSub$siteid,vegeclass=YearSub$vegeclass)
#subset ParmAll to only focus on interannual sites
ParmAlls <- join(ParmAll, YearSub2, by=c("depth","siteid"), type="inner") 


#organize previous air components

parmAs2 <- c("TmaxA","TminA", "TmaxA") 
subAT <- c(1,0,1)
AirL2 <- list()
AirMs2 <- numeric(0)
for(i in 1:length(parmAs2)){
	AirL2[[i]] <- AirParm[AirParm$Aparm==parmAs2[i],]
	#calculate mean
	AirMs2[i] <- round(mean(AirL[[i]]$AMean),3)
	#add a regression ID
	AirL2[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
	#add 1 from wyear to match the previous season in winter to current
	AirL2[[i]]$wyear <- AirL2[[i]]$wyear+subAT[i]
}

AirR2 <- ldply(AirL2,data.frame)

colnames(AirR2)[1:4] <- paste0(colnames(AirR2)[1:4],"c")

#join to ParmAlls

ParmAlls <- join(ParmAlls,AirR2, by=c("siteid","wyear","regID"),type="left")

#organize past soil temp focusing on past 1 year
TsoilAve <- SoilParm[SoilParm$parm=="TaverageS",]
#matching for 1 year into the past
TsoilAve1 <- TsoilAve
TsoilAve1$wyear <- TsoilAve$wyear+1
colnames(TsoilAve1)[1:4] <- paste0(colnames(TsoilAve1)[1:4],"M1")



#join past into the the regressions
ParmAlls2 <- join(ParmAlls, TsoilAve1, by=c("siteid","depth","wyear"), type="left")


#omit any data with NA because that means there aren't enough preceding years
ParmAlls3 <- na.omit(ParmAlls2)

#make regVege table
regVegeDF <- unique(data.frame(regID=ParmAlls3$regID,vegeclass=ParmAlls3$vegeclass)) 
regVegeDF$regvegeID <- seq(1,dim(regVegeDF)[1])

#join into dataframe
ParmAlls4 <- join(ParmAlls3,regVegeDF, by=c("regID","vegeclass"), type="left")

#calculate average air temp in each regression
airTempCurrentm <- aggregate(ParmAlls4$AMean,by=list(ParmAlls4$regID),FUN="mean")
airTempPastm <- aggregate(ParmAlls4$AMeanc,by=list(ParmAlls4$regID),FUN="mean")

colnames(airTempCurrentm) <- c("regID","meanA")
colnames(airTempPastm) <- c("regID","meanA")

#calculate average past temp across sites
		
aveAnt <- aggregate(ParmAlls4$MeanM1, by=list(ParmAlls4$regvegeID), FUN="mean")
colnames(aveAnt) <- c("regvegeID","tempAve")
#######################################
#####set up model run             ##### 
#######################################
datalist <- list(Nobs=dim(ParmAlls4)[1],
					SoilP=ParmAlls4$Mean,
					regVege=ParmAlls4$regvegeID,
					depth=ParmAlls4$depth,
					AirPbar=airTempCurrentm$meanA,
					pastairbar=airTempPastm$meanA,
					reg=ParmAlls4$regID,
					sigMod=ParmAlls4$SD,
					AirP=ParmAlls4$AMean,
					sig.Air=ParmAlls4$ASD,
					pastAir=ParmAlls4$AMeanc,
					sig.pastAir=ParmAlls4$ASDc,
					#pastSoil=ParmAlls4$MeanM1,
					#meanpastSoilT=aveAnt$tempAve,
					#sig.pastSoil=ParmAlls4$SDM1,
					NregVege=dim(regVegeDF)[1],
					lower=c(-40,0,0),
					upper=c(0,35,.65),
					regV=regVegeDF$regID)
								
parms <- c("beta0","beta1","beta2","beta3","beta4","sigSoilV","repSoilP")								

#look at a covariance plot
plot(~ParmAlls4$depth[ParmAlls4$regID==1]+ParmAlls4$AMean[ParmAlls4$regID==1]+ParmAlls4$AMeanc[ParmAlls4$regID==1]+ParmAlls4$MeanM1[ParmAlls4$regID==1])
plot(~ParmAlls4$depth[ParmAlls4$regID==2]+ParmAlls4$AMean[ParmAlls4$regID==2]+ParmAlls4$AMeanc[ParmAlls4$regID==2]+ParmAlls4$MeanM1[ParmAlls4$regID==2])

cor(ParmAlls4$AMean[ParmAlls4$regID==2],ParmAlls4$AMeanc[ParmAlls4$regID==2])
cor(ParmAlls4$AMean[ParmAlls4$regID==2],ParmAlls4$MeanM1[ParmAlls4$regID==2])
if(modRun==1){
#start model 
inter.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\inter_annual\\inter_annual_code.r",
						data=datalist,
						n.adapt=5000,
						n.chains=3)

inter.sample <- coda.samples(inter.modI,variable.names=parms,
                       n.iter=2000, thin=1)	
					
#model history
mcmcplot(inter.sample, parms=c("beta0","beta1","beta2","beta3","beta4","sigSoilV"),
			dir=paste0(modDI,"\\history"))								
					
Xcomp <- round(0.05/((dim(regVegeDF)[1]-1)),3)		
#model output							   
mod.out <- summary(inter.sample,  quantiles = c(Xcomp,0.025, 0.25, 0.5, 0.75, 0.975,1-Xcomp))

write.table(mod.out$statistics,paste0(modDI,"\\inter_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(mod.out$quantiles,paste0(modDI,"\\inter_mod_quant.csv"),
			sep=",",row.names=TRUE)

#coda output
chain1<-as.matrix(inter.sample [[1]])
write.table(chain1,paste0(modDI,"\\chain1_coda.csv"), sep=",")
chain2<-as.matrix(inter.sample [[2]])
write.table(chain2,paste0(modDI,"\\chain2_coda.csv"), sep=",")
chain3<-as.matrix(inter.sample [[3]])
write.table(chain3,paste0(modDI,"\\chain3_coda.csv"), sep=",")		
}

#see if correlation between effects is causing mixing problems

cnames <- colnames(chain1)
dexps <- "\\[*[[:digit:]]*\\]"
cparms <- gsub(dexps,"", cnames)

beta0 <- chain1[,cparms=="beta0"]
beta1 <- chain1[,cparms=="beta1"]
beta2 <- chain1[,cparms=="beta2"]
beta3 <- chain1[,cparms=="beta3"]
beta4 <- chain1[,cparms=="beta4"]


plot(~beta0[,1]+beta1[,1]+beta3[,1]+beta4[,1])
plot(~beta0[,2]+beta1[,2]+beta3[,2]+beta4[,2])
plot(~beta0[,4]+beta1[,4]+beta3[,4]+beta4[,4])