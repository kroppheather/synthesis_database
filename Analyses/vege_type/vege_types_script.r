##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
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
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\plots\\model_all"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run9"
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
#for analysis: TminS, TmaxS, average
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "TaverageS") 

parmAs <- c("TminA","TmaxA", "TaverageA") 


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
	AirMs[i] <- round(mean(AirL[[i]]$AMean),0)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}


#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)



#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")
#remove outlier year and depth
ParmAll <- ParmAll[-which(ParmAll$wyear == 1996 & ParmAll$siteid == 13 & ParmAll$depth ==2.5),]
#get unique veg regression id

regvegeID <- unique(data.frame(vegeclass=ParmAll$vegeclass,regID=ParmAll$regID))
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#now join back into parmall
ParmAll <- join(ParmAll,regvegeID, by=c("vegeclass","regID"),type="left")




#get range of precip and air temps in each vegetation group for each regression
#get minimumAir
minAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="min")
maxAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="max")
meanAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="mean")




regvegeID$minAir <- minAir$x
regvegeID$maxAir <- maxAir$x
regvegeID$meanAir <- round(meanAir$x,0)



#get min and max of data for plotting
#air
regMinA <- aggregate(ParmAll$AMean,by=list(ParmAll$regID),FUN="min")
regMinA$x <- round_any(regMinA$x,5,floor)

regMaxA <- aggregate(ParmAll$AMean,by=list(ParmAll$regID),FUN="max")
regMaxA$x <- round_any(regMaxA$x,5,ceiling)



#set up in matrix for plotting
regPlotA <- matrix(rep(NA,dim(regvegeID)[1]*200),ncol=dim(regvegeID)[1])
regTempA <- numeric(0)

for(i in 1:dim(regvegeID)[1]){
	regTempA <- seq(regMinA$x[regvegeID$regID[i]],regMaxA$x[regvegeID$regID[i]],length.out=200)
	regPlotA[,i] <- regTempA

}





#######################################
#####prepare model run            ##### 
#######################################
#data
datalist <- list(Nobs=dim(ParmAll)[1],
				regVege=ParmAll$regvegeID,
				depth=ParmAll$depth,
				SoilP=ParmAll$Mean,
				AirPbar=regvegeID$meanAir,
				sigMod=ParmAll$SD,
				AirP=ParmAll$AMean,
				Acomp=AirMs,
				reg=regvegeID$regID,
				sig.Air=ParmAll$ASD,
				NregVege=dim(regvegeID)[1],
				regV=regvegeID$regID,
				Nreg=3,
				#put in dummy number if not used
				AcompPlot=regPlotA,tau.Air=ifelse(ParmAll$ASD == 0, 10000000, ParmAll$ASD^-2)
				)
				
#paramters of interest
parms <- c("beta0","beta1","beta2","sigSoilV","repSoilP",
		"mu.beta0","mu.beta1","mu.beta2",
		"sig.beta0","sig.beta1","sig.beta2","meanComp","plotRegA",
		"mu.AirP")	
#each regression has 3 parameters for each 9 vegetation types		
Xcomp <- round(0.05/((9*3)-1),3)
if(modRun==1){
#start model 
vege.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\vege_type\\vege_types_model_code.r",
						data=datalist,
						n.adapt=50000,
						n.chains=3)

vege.sample <- coda.samples(vege.modI,variable.names=parms,
                       n.iter=300000, thin=100)	
					
#model history
mcmcplot(vege.sample, parms=c("beta0","beta1","beta2","sigSoilV",
								"mu.beta0","mu.beta1","mu.beta2",
								"sig.beta0","sig.beta1","sig.beta2"),
			dir=paste0(modDI,"\\history"))


			
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

}	



#######################################
#####look at data                 ##### 
#######################################


#read in model results 

datM <- read.csv(paste0(modDI,"\\vege_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\vege_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))

