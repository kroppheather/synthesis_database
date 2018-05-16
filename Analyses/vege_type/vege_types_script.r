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
#world clim 2 precip in mm
datWC <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\WCprecSites.csv")
colnames(datWC)[1] <- "siteid"
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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run1"
Nrun <-1
#indicate if a model run is occuring
modRun <- 1


#######################################
#####organize data                ##### 
#######################################


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)


#join world clim data
SoilParm <- join(SoilParm, datWC, by=c("siteid"), type="left")

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

#sum up winter months Oct-Mar
#sum up summer months Aprl-Sept
SoilR$precW <- rowSums(SoilR[,11:13])+rowSums(SoilR[,20:22])
SoilR$precS <- rowSums(SoilR[,14:19])

#set up a vector for the matching timeperiod
SoilR$precR <- ifelse(SoilR$regID==2,SoilR$precS,SoilR$precW)


#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#get unique veg regression id

regvegeID <- unique(data.frame(vegeclass=ParmAll$vegeclass,regID=ParmAll$regID))
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#now join back into parmall
ParmAll <- join(ParmAll,regvegeID, by=c("vegeclass","regID"),type="left")

#precip mean
precMs <- c(mean(ParmAll$precW),mean(ParmAll$precS),mean(ParmAll$precW))

#######################################
#####prepare model run            ##### 
#######################################
#data
datalist <- list(Nobs=dim(ParmAll)[1],
				regVege=ParmAll$regvegeID,
				depth=ParmAll$depth,
				SoilP=ParmAll$Mean,
				reg=ParmAll$regID,
				AirPbar=AirMs,
				precip=ParmAll$precR,
				precipbar=precMs,
				sigMod=ParmAll$SD,
				AirP=ParmAll$AMean,
				sig.Air=ParmAll$ASD,
				NregVege=dim(regvegeID)[1]
				)
				
#paramters of interest
parms <- c("beta0","beta1","beta2","beta3","sigSoilV","repSoilP")			

if(modRun==1){
#start model 
vege.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\vege_type\\vege_types_model_code.r",
						data=datalist,
						n.adapt=50000,
						n.chains=3)

vege.sample <- coda.samples(vege.modI,variable.names=parms,
                       n.iter=300000, thin=100)	
					
#model history
mcmcplot(vege.sample, parms=c("beta0","beta1","beta2","beta3","sigSoilV"),
			dir=paste0(modDI,"\\history"))


			
#model output							   
mod.out <- summary(vege.sample,  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

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

