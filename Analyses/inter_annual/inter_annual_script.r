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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run9"
Nrun <- 9
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
#subset sites with at least 5 years
YearSub <- YearCount[YearCount$nYear >=5,]

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


#organize previous soil components
#organize past soil maximum focusing on past 4 years
Tmax <- SoilParm[SoilParm$parm=="TmaxS",]


#matching for 1 year into the past
Tmax1 <- Tmax
Tmax1$wyear <- Tmax1$wyear+1
colnames(Tmax1)[1:4] <- paste0(colnames(Tmax1)[1:4],"Max1")
#matching for 2 year into the past
Tmax2 <- Tmax
Tmax2$wyear <- Tmax2$wyear+2
colnames(Tmax2)[1:4] <- paste0(colnames(Tmax2)[1:4],"Max2")
#matching for 3 year into the past
Tmax3 <- Tmax
Tmax3$wyear <- Tmax3$wyear+3
colnames(Tmax3)[1:4] <- paste0(colnames(Tmax3)[1:4],"Max3")
#matching for 4 year into the past
Tmax4 <- Tmax
Tmax4$wyear <- Tmax4$wyear+4
colnames(Tmax4)[1:4] <- paste0(colnames(Tmax4)[1:4],"Max4")

#join to ParmAlls

ParmAlls1 <- join(ParmAlls,Tmax1, by=c("siteid","wyear","depth"),type="left")
ParmAlls2 <- join(ParmAlls1, Tmax2, by=c("siteid","wyear","depth"), type="left")
ParmAlls3 <- join(ParmAlls2, Tmax3, by=c("siteid","wyear","depth"), type="left")
ParmAlls4 <- join(ParmAlls3, Tmax4, by=c("siteid","wyear","depth"), type="left")


#organize past soil maximum focusing on past 4 years
Tmin <- SoilParm[SoilParm$parm=="TminS",]
#matching same year for Tmax model
Tmin0 <- Tmin
Tmin0$wyear <- Tmin0$wyear
colnames(Tmin0)[1:4] <- paste0(colnames(Tmin0)[1:4],"MinT0")
#matching previous year 
Tmin1 <- Tmin
Tmin1$wyear <- Tmin1$wyear+1
colnames(Tmin1)[1:4] <- paste0(colnames(Tmin1)[1:4],"MinT1")
#matching previous 2 year 
Tmin2 <- Tmin
Tmin2$wyear <- Tmin2$wyear+2
colnames(Tmin2)[1:4] <- paste0(colnames(Tmin2)[1:4],"MinT2")
#matching previous 3 year 
Tmin3 <- Tmin
Tmin3$wyear <- Tmin3$wyear+3
colnames(Tmin3)[1:4] <- paste0(colnames(Tmin3)[1:4],"MinT3")
#matching previous 2 year 
Tmin4 <- Tmin
Tmin4$wyear <- Tmin4$wyear+4
colnames(Tmin4)[1:4] <- paste0(colnames(Tmin4)[1:4],"MinT4")

#join to Parms

#first get the temp years joined
ParmAlls5 <- join(ParmAlls4, Tmin0, by=c("siteid","wyear","depth"), type="left")
ParmAlls6 <- join(ParmAlls5, Tmin1, by=c("siteid","wyear","depth"), type="left")
ParmAlls7 <- join(ParmAlls6, Tmin2, by=c("siteid","wyear","depth"), type="left")
ParmAlls8 <- join(ParmAlls7, Tmin3, by=c("siteid","wyear","depth"), type="left")
ParmAlls9 <- join(ParmAlls8, Tmin4, by=c("siteid","wyear","depth"), type="left")




ParmAlls9$MeanMin1 <- ifelse(ParmAlls9$regID==2,ParmAlls9$MeanMinT0,ParmAlls9$MeanMinT1)
ParmAlls9$MeanMin2 <- ifelse(ParmAlls9$regID==2,ParmAlls9$MeanMinT1,ParmAlls9$MeanMinT2)
ParmAlls9$MeanMin3 <- ifelse(ParmAlls9$regID==2,ParmAlls9$MeanMinT2,ParmAlls9$MeanMinT3)
ParmAlls9$MeanMin4 <- ifelse(ParmAlls9$regID==2,ParmAlls9$MeanMinT3,ParmAlls9$MeanMinT4)

#omit any data with NA because that means there aren't enough preceding years
ParmAlls9 <- na.omit(ParmAlls9)

#make regVege table
regVegeDF <- unique(data.frame(regID=ParmAlls5$regID,vegeclass=ParmAlls5$vegeclass)) 
regVegeDF$regvegeID <- seq(1,dim(regVegeDF)[1])

#join into dataframe
ParmAlls10 <- join(ParmAlls9,regVegeDF, by=c("regID","vegeclass"), type="left")

#calculate average air temp in each regression
airTempCurrentm <- aggregate(ParmAlls10$AMean,by=list(ParmAlls10$regID),FUN="mean")

colnames(airTempCurrentm) <- c("regID","meanA")

#calculate average past temp across sites
		
maxAnt <- aggregate(c(ParmAlls10$MeanMax1,ParmAlls10$MeanMax2,ParmAlls10$MeanMax3,ParmAlls10$MeanMax4), 
						by=list(c(ParmAlls10$regID,ParmAlls10$regID,ParmAlls10$regID,ParmAlls10$regID)),
						FUN="mean")
colnames(maxAnt) <- c("regID","tempAve")

minAnt <- aggregate(c(ParmAlls10$MeanMin1,ParmAlls10$MeanMin2,ParmAlls10$MeanMin3,ParmAlls10$MeanMin4), 
						by=list(c(ParmAlls10$regID,ParmAlls10$regID,ParmAlls10$regID,ParmAlls10$regID)),
						FUN="mean")
colnames(minAnt) <- c("regID","tempAve")

#######################################
#####set up model run             ##### 
#######################################
datalist <- list(Nobs=dim(ParmAlls10)[1],
					SoilP=ParmAlls10$Mean,
					regVege=ParmAlls10$regvegeID,
					depth=ParmAlls10$depth,
					AirPbar=airTempCurrentm$meanA,
					reg=ParmAlls10$regID,
					sigMod=ParmAlls10$SD,
					AirP=ParmAlls10$AMean,
					meanpastMax=maxAnt$tempAve,
					meanpastMin=minAnt$tempAve,
					NregVege=dim(regVegeDF)[1],
					a.Tmax= matrix(c(ParmAlls10$MeanMax1,ParmAlls10$MeanMax2,ParmAlls10$MeanMax3,ParmAlls10$MeanMax4),
									byrow=FALSE,ncol=4),
					a.Tmin= matrix(c(ParmAlls10$MeanMin1,ParmAlls10$MeanMin2,ParmAlls10$MeanMin3,ParmAlls10$MeanMin4),
									byrow=FALSE,ncol=4),															
					Nlag=4,				
					lower=c(-40,0,0),
					upper=c(0,35,.65),
					regV=regVegeDF$regID)
								
parms <- c("beta0","beta1","beta2","beta3","beta4","wTmax","sigSoilV","repSoilP","wTmin")								


if(modRun==1){
#start model 
inter.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\inter_annual\\inter_annual_code.r",
						data=datalist,
						n.adapt=10000,
						n.chains=3)

inter.sample <- coda.samples(inter.modI,variable.names=parms,
                       n.iter=40000, thin=20)	
					
#model history
mcmcplot(inter.sample, parms=c("beta0","beta1","beta2","beta3","beta4","wTmax","wTmin","sigSoilV"),
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