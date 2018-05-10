##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
##########################################################
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in                   ###
### air and shallow soil temperature coupling          ###
### but also needs to account for depth                ###
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
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\plots"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model\\run1"
Nrun <-1
#indicate if a model run is occuring
modRun <- 1


#######################################
#####organize data                ##### 
#######################################


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)
AirParm <- join(AirParm, datV, by=c("siteid"), type="left")

#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$parm)

#pull out relevant parameters
#for analysis: TminS, TmaxS, peakWS
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "peakWS") 


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmVs)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmVs[i]&SoilParm$depth<=20,]
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
}

plot(SoilL[[1]]$depth,SoilL[[1]]$Mean)

#need to see how many depths measured for each site

DepthID <- unique(data.frame(depth=SoilL[[1]]$depth, siteid=SoilL[[1]]$siteid))

#get the count of depths in a site
DepthCount <- aggregate(DepthID$depth,by=list(DepthID$siteid), FUN="length")
colnames(DepthCount) <- c("siteid", "DepthCount")

#look at sites with at least 3 depths measured
DepthCountM <- DepthCount[DepthCount$DepthCount>=3,]



#start by looking at the slopes for sites with 3 or more depths for each year and site

SoilLM <- list()

for(i in 1:length(parmVs)){
	SoilLM[[i]] <- join(SoilL[[i]], DepthCountM, by="siteid", type="inner")
	SoilLM[[i]]$parmID <- rep(i,dim(SoilLM[[i]])[1])
}

#see how many sites for each vegetation class are represented
VCid <- unique(data.frame(siteid=SoilLM[[1]]$siteid,vegeclass=SoilLM[[1]]$vegeclass))

VegeCount <- aggregate(VCid$vegeclass, by=list(VCid$vegeclass), FUN="length")
colnames(VegeCount) <- c("vegeclass","vegeCount")

#turn soilparms into a dataframe

Msoil <- ldply(SoilLM,data.frame)

#get unique id for year 
SYPid <- unique(data.frame(siteid=Msoil$siteid,wyear=Msoil$wyear,parmID=Msoil$parmID,vegeclass=Msoil$vegeclass))
SYPid$SYP <- seq(1,dim(SYPid)[1])

#join id back into soil dataframe
Msoil2 <- join(Msoil,SYPid, by=c("siteid","wyear","parmID","vegeclass"), type="left")


#######################################
#####prepare model run            ##### 
#######################################
#data
datalist <- list(Nobs=dim(Msoil2)[1],
				tempParm=Msoil2$Mean,
				parmID=Msoil2$parmID, 
				SYP=Msoil2$SYP,
				depth=Msoil2$depth,
				sig.mod=Msoil2$SD,
				NSYP=dim(SYPid)[1],
				Nparm=length(parmVs))
				
#paramters of interest
parms <- c("beta0","beta1","sigP")			

if(modRun==1){
#start model 
vege.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\vege_type\\depth_slopes_model_code.r",
						data=datalist,
						n.adapt=10000,
						n.chains=3)

vege.sample <- coda.samples(vege.modI,variable.names=parms,
                       n.iter=30000, thin=10)	
					
#model history
mcmcplot(vege.sample, parms=c("beta0","beta1","sigP"),
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


#look at slopes
slope <- datC[datC$parms=="beta1",]
slope <- cbind(slope,SYPid)


plot(slope$SYP,slope$Mean, pch=19)
arrows(slope$SYP,slope$X2.5.,slope$SYP,slope$X97.5.,code=0)
