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

library(coda)
library(mcmcplots)
library(plyr)
library(snow)
library(snowfall)
library(R2OpenBUGS)

#######################################
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\plots\\model"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run3"
Nrun <- 3
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
#most vegeclasses only have 1-3 sites with at least 5 years of data
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

#organize past soil temp focusing on past 4 years
TsoilAve <- SoilParm[SoilParm$parm=="TaverageS",]
#matching for 1 year into the past
TsoilAve1 <- TsoilAve
TsoilAve1$wyear <- TsoilAve$wyear+1
colnames(TsoilAve1)[1:4] <- paste0(colnames(TsoilAve1)[1:4],"M1")
#matching for 2 year into the past
TsoilAve2 <- TsoilAve
TsoilAve2$wyear <- TsoilAve$wyear+2
colnames(TsoilAve2)[1:4] <- paste0(colnames(TsoilAve2)[1:4],"M2")
#matching for 3 year into the past
TsoilAve3 <- TsoilAve
TsoilAve3$wyear <- TsoilAve$wyear+3
colnames(TsoilAve3)[1:4] <- paste0(colnames(TsoilAve3)[1:4],"M3")
#matching for 4 year into the past
TsoilAve4 <- TsoilAve
TsoilAve4$wyear <- TsoilAve$wyear+4
colnames(TsoilAve4)[1:4] <- paste0(colnames(TsoilAve4)[1:4],"M4")

#join past into the the regressions
ParmAlls2 <- join(ParmAlls, TsoilAve1, by=c("siteid","depth","wyear"), type="left")
ParmAlls3 <- join(ParmAlls2, TsoilAve2, by=c("siteid","depth","wyear"), type="left")
ParmAlls4 <- join(ParmAlls3, TsoilAve3, by=c("siteid","depth","wyear"), type="left")
ParmAlls5 <- join(ParmAlls4, TsoilAve4, by=c("siteid","depth","wyear"), type="left")

#omit any data with NA because that means there aren't enough preceding years
ParmAlls5 <- na.omit(ParmAlls5)

#make regVege table
regVegeDF <- unique(data.frame(regID=ParmAlls5$regID,vegeclass=ParmAlls5$vegeclass)) 
regVegeDF$regvegeID <- seq(1,dim(regVegeDF)[1])

#join into dataframe
ParmAlls6 <- join(ParmAlls5,regVegeDF, by=c("regID","vegeclass"), type="left")

#calculate average air temp in each regression
airTempCurrentm <- aggregate(ParmAlls6$AMean,by=list(ParmAlls6$regID),FUN="mean")
airTempPastm <- aggregate(ParmAlls6$AMeanc,by=list(ParmAlls6$regID),FUN="mean")

colnames(airTempCurrentm) <- c("regID","meanA")
colnames(airTempPastm) <- c("regID","meanA")

#######################################
#####set up model run             ##### 
#######################################
datalist <- list(Nobs=dim(ParmAlls6)[1],
					SoilP=ParmAlls6$Mean,
					regVege=ParmAlls6$regvegeID,
					depth=ParmAlls6$depth,
					AirPbar=airTempCurrentm$meanA,
					pastairbar=airTempPastm$meanA,
					reg=ParmAlls6$regID,
					sigMod=ParmAlls6$SD,
					AirP=ParmAlls6$AMean,
					sig.Air=ParmAlls6$ASD,
					pastAir=ParmAlls6$AMeanc,
					sig.pastAir=ParmAlls6$ASDc,
					NregVege=dim(regVegeDF)[1],
					Nlag=4,
					a.T=matrix(c(ParmAlls6$MeanM1,
								ParmAlls6$MeanM2,
								ParmAlls6$MeanM3,
								ParmAlls6$MeanM4),ncol=4,byrow=FALSE))
								
parms <- c("beta0","beta1","beta3","beta4","sigSoilV","wT","antSoil","repSoilP")								


if(modRun==1){

# set the number of CPUs to be 3


sfInit(parallel=TRUE, cpus=3)

# assign the R2OpenBUGS library to each CPU
sfLibrary(R2OpenBUGS)	

#creating separate directory for each CPU process

modCode <- "c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\inter_annual\\inter_annual_code.r"

folder1 <- paste0(modDI, "\\chain1")
folder2 <- paste0(modDI, "\\chain2")
folder3 <- paste0(modDI, "\\chain3")
dir.create(folder1); dir.create(folder2); dir.create(folder3)	
folderALL <- c(folder1, folder2, folder3)
#copy model code
for (i in 1:length(folderALL)){

	file.copy(modCode, paste0(folderALL[i], "\\model_code.txt"), overwrite=TRUE) 

}	

#get model started but run manually
parallel.bugs <- function(chain, x.data, params){
	folder <- ifelse(chain==1,"c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run2\\chain1",
				ifelse(chain==2,"c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run2\\chain2",
					"c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run2\\chain3"))
 	
	 if(chain==1){
	 inits <- list(list(beta0=c(-10,-10,10,10,.5,.5), beta1=c(.1,.1,-.1,-.1,.1,.1), beta2=c(.2,.2,.2,.2,.2,.2),
					beta3=c(.2,.2,.2,.2,.2,.2),beta4=c(.2,.2,.2,.2,.2,.2)))
	 
	 }
	 if(chain==2){
	 inits <- list(list(beta0=c(-15,-15,15,15,.55,.55), beta1=c(.15,.15,-.15,-.15,.15,.15), beta2=c(.1,.1,.1,.1,.1,.1),
					beta3=c(.25,.25,.25,.25,.25,.25),beta4=c(.25,.25,.25,.25,.25,.25)))
	 
	 }
	 	 if(chain==3){
	 inits <- list(list(beta0=c(-20,-20,20,20,.45,.45), beta1=c(.01,.01,-.01,-.01,.01,.01), beta2=c(.02,.02,.02,.02,.02,.02),
					beta3=c(.02,.02,.02,.02,.02,.02),beta4=c(.02,.02,.02,.02,.02,.02)))
	 
	 }
	 
	# 5b. call openbugs
	bugs(data=x.data, inits=inits,parameters.to.save=params,
             n.iter=10, n.chains=1, n.burnin=1, n.thin=1,
             model.file="model_code.txt", codaPkg=TRUE,
             OpenBUGS.pgm="C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe",debug=TRUE,
             working.directory=folder)	
}	
	#finish running with slice updater by hand.	 
	#updated 10 to get model going. Then did 3000 thinning by 50
	#realized I forgot to set samples on two chains so I reran for another 3000 thinning by 100
# parallel.bugs on each of the 3 CPUs
sfLapply(1:3, fun=parallel.bugs,x.data=datalist, params=parms)


folder1 <- paste0(modDI, "\\CODA\\chain1")
folder2 <- paste0(modDI, "\\CODA\\chain2")
folder3 <- paste0(modDI, "\\CODA\\chain3")




# 9. pull coda back out
codaobj1 <- read.bugs(c(paste0(folder1, "\\CODAchain1.txt"),
						paste0(folder2, "\\CODAchain1.txt")
						,paste0(folder3, "\\CODAchain1.txt")
						))

		
#model history
mcmcplot(mcmc.list(codaobj1),parms=c("beta0","beta1","beta2","beta3","beta4","sigSoilV",
								"wT"),	
			dir=paste0(modDI,"\\history"))								
					
Xcomp <- round(0.05/((dim(regVegeDF)[1]-1)),3)		
#model output							   
mod.out <- summary(codaobj1,  quantiles = c(Xcomp,0.025, 0.25, 0.5, 0.75, 0.975,1-Xcomp))

write.table(mod.out$statistics,paste0(modDI,"\\inter_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(mod.out$quantiles,paste0(modDI,"\\inter_mod_quant.csv"),
			sep=",",row.names=TRUE)

#coda output
chain1<-as.matrix(codaobj1[[1]])
write.table(chain1,paste0(modDI,"\\chain1_coda.csv"), sep=",")
chain2<-as.matrix(codaobj1[[2]])
write.table(chain2,paste0(modDI,"\\chain2_coda.csv"), sep=",")
chain3<-as.matrix(codaobj1[[3]])
write.table(chain3,paste0(modDI,"\\chain3_coda.csv"), sep=",")		
}