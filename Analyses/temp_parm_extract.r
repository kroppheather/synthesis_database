##########################################################
########Temperature parameter extraction script###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script extracts output from the temperature   ###
### model output obtained from the Temp_model scripts. ###
### The script is structured for bring together output ###
### from parallel runs of the temperature model. Inputs###
### associated with the run conditions will change and ###
### will be specified at the start of this script. This###
### script is meant to be run at the start of all      ###
### all analyses and plotting scripts that analyzer    ###
### n factors or temperature model parameters.         ###
##########################################################
##########################################################
### Outputs: dataframes of parameters with mean, sd,   ###
### CI, and all accompanying IDs and info              ###
### dataframes are Nfactor, SoilParm, AirParm,         ###
### AirRepID,SoilRepID, datCSM, datCAM (lists)         ###
### DDsoil,DDair                                       ###
##########################################################
##########################################################

#libraries
library(plyr)

############################
##Model run specifications##
############################
#directory of output
mrunDir <- "z:\\projects\\synthesis"


#read in model run file
#indicates the runs that 
#had convergence issues

AllmrunF <- read.csv(paste0(mrunDir,"\\mod_run.csv"))

#get directory names

WDRF <- list.dirs(paste0(mrunDir),recursive=FALSE,full.names=FALSE)

#get directory names for site names within each 
SDRF <- list()
WDR <- list()
mrun <- list()
for(i in 1:length(WDRF)){
	SDRF[[i]] <- list.dirs(paste0(mrunDir,"\\",WDRF[i]),recursive=FALSE,full.names=FALSE)
	WDR[[i]] <- paste0(mrunDir,"\\",WDRF[i],"\\",SDRF[[i]])
	mrun[[i]] <- data.frame(model.run=rep(as.numeric(gsub("\\D","",WDRF[i])),length(SDRF[[i]])), 
								siteid=as.numeric(gsub("\\D","",SDRF[[i]])),
								dir=WDR[[i]],
								run.dir=rep(paste0(mrunDir,"\\",WDRF[i]),length(SDRF[[i]])))
}

mrunF <- ldply(mrun,data.frame)

mrunF <- mrunF[order(mrunF$siteid),]	

#pull out sites that had convergence issues
mrunF <- join(mrunF,AllmrunF, by="siteid",type="left")
mrunF <- mrunF[is.na(mrunF$noConv),]
	
############################
##Read in data            ##
############################

#working directory for data
#same data saved in each model run
#so just grab it from first one
dataWD <-paste0(mrunDir,"\\",WDRF[1])

#read in data
datAI <- read.csv(paste0(dataWD,"\\AirIDS.csv"))
datSI <- read.csv(paste0(dataWD,"\\SoilIDS.csv"))

datNI <- read.csv(paste0(dataWD,"\\ncomboIDS.csv"))

datAT <- read.csv(paste0(dataWD,"\\AirTaveIDS_SD.csv"))
datST <- read.csv(paste0(dataWD,"\\SoilTaveIDS_SD.csv"))

datAM <- read.csv(paste0(dataWD,"\\Tair_model.csv"))
datSM <- read.csv(paste0(dataWD,"\\Tsoil_model.csv"))

datSM$decdateA <- datSM$decdate-1991
datAM$decdateA <- datAM$decdate-1991

datAveIS <- read.csv(paste0(dataWD,"\\SoilTaveIDS_SD.csv"))
datAveIA <- read.csv(paste0(dataWD,"\\AirTaveIDS_SD.csv"))

#now organize rep IDS, need to pull correct ones for the site
#in the run it is actually used in


datSRALL<-list()
datARALL<-list()

for(i in 1:dim(mrunF)[1]){
	datSRALL[[i]]<-read.csv(paste0(mrunF$run.dir[i],"\\SoilrepIDS.csv"))
	datARALL[[i]]<-read.csv(paste0(mrunF$run.dir[i],"\\AirrepIDS.csv"))
}

#get the correct IDS for each site generated in the site run
datSR<-list()
datAR<-list()
for(i in 1:dim(mrunF)[1]){
	datSR[[i]]<-datSRALL[[i]][datSRALL[[i]]$siteid==mrunF$siteid[i],]
	datAR[[i]]<-datARALL[[i]][datARALL[[i]]$siteid==mrunF$siteid[i],]
}

datSRdf<-ldply(datSR, data.frame)
datARdf<-ldply(datAR, data.frame)

#expression to remove bracket and vector number from parm names
dexps<-"\\[*[[:digit:]]*\\]"

datM<-list()
datQ<-list()
datC<-list()
for(i in 1:dim(mrunF)[1]){
	datM[[i]]<-read.csv(paste0(mrunF$dir[i],"\\Temp_mod_stats.csv"))
	datQ[[i]]<-read.csv(paste0(mrunF$dir[i],"\\Temp_mod_quant.csv"))
	#join means and quantiles
	datC[[i]]<-cbind(datM[[i]],datQ[[i]])
	#make parms vectors
	datC[[i]]$parms1<-gsub(dexps,"",rownames(datC[[i]]))
}

#now pull out more ID info

AirSDW<-list()
SoilSDW<-list()

AirTA<-list()
SoilTA<-list()
datNIS<-list()
datAvAID <- list()
datAvSID <-list()

for(i in 1:dim(mrunF)[1]){

	AirSDW[[i]]<-datAI[datAI$siteid==mrunF$siteid[i],]
	AirSDW[[i]]$siteSDW<-seq(1,dim(AirSDW[[i]])[1])
	
	SoilSDW[[i]]<-datSI[datSI$siteid==mrunF$siteid[i],]
	SoilSDW[[i]]$siteSDW<-seq(1,dim(SoilSDW[[i]])[1])
	
	AirTA[[i]]<-datAT[datAT$siteid==mrunF$siteid[i],]
	SoilTA[[i]]<-datST[datST$siteid==mrunF$siteid[i],]
	
	#pull out N factor IDs by site
	datNIS[[i]]<-datNI[datNI$siteid==mrunF$siteid[i],]
	#ave IDs
	datAvSID[[i]] <- datAveIS[datAveIS$siteid==mrunF$siteid[i],]
	datAvAID[[i]] <- datAveIA[datAveIA$siteid==mrunF$siteid[i],]
	
}


#########################################
######pull out parameters for mean model

datT1p1<-list()
datT1p2<-list()
datT1t1<-list()
datT1t2<-list()
datMax<-list()
datMin<-list()
datWpeak<-list()
datSpeak<-list()
datZero<-list()
datNfreeze<-list()
datNthaw<-list()
datTaverageS <- list()
datSTav<- list()
datATav<- list()
datAreps <- list()
datSreps <- list()
datCSM<-list()
datCAM<-list()
datThawN <- list()
datTDDS <- list()
datFDDS <- list()
datTDDA <- list()
datFDDA <- list()

#now pull out id number
dexps2<-"\\D"

#subset first to only look at soil parms
for(i in 1:dim(mrunF)[1]){

	#max
	datMax[[i]]<-datC[[i]][datC[[i]]$parms1=="TmaxS",]
	datMax[[i]]$siteSDW<-seq(1,dim(datMax[[i]])[1])
	#min
	datMin[[i]]<-datC[[i]][datC[[i]]$parms1=="TminS",]
	datMin[[i]]$siteSDW<-seq(1,dim(datMin[[i]])[1])
	#peak timing
	datWpeak[[i]]<-datC[[i]][datC[[i]]$parms1=="peakWS",]
	datWpeak[[i]]$siteSDW<-seq(1,dim(datWpeak[[i]])[1])
	datSpeak[[i]]<-datC[[i]][datC[[i]]$parms1=="peakSS",]	
	datSpeak[[i]]$siteSDW<-seq(1,dim(datSpeak[[i]])[1])
	#zero curtain
	datZero[[i]]<-datC[[i]][datC[[i]]$parms1=="DayZero",]
	datZero[[i]]$siteSDW<-seq(1,dim(datZero[[i]])[1])
	#Nfactor
	datNthaw[[i]]<-datC[[i]][datC[[i]]$parms1=="Tn",]
	datNthaw[[i]]$Nseq<-seq(1,dim(datNthaw[[i]])[1])
	#
	datNfreeze[[i]]<-datC[[i]][datC[[i]]$parms1=="Fn",]
	datNfreeze[[i]]$Nseq<-seq(1,dim(datNfreeze[[i]])[1])
	#average temperature
	datTaverageS[[i]]<-datC[[i]][datC[[i]]$parms1=="TaverageS",]
	datTaverageS[[i]]$siteSDW <- seq(1,dim(datTaverageS[[i]])[1])
	#number of days thawed
	datThawN[[i]] <- datC[[i]][datC[[i]]$parms1=="ThawCountN",]
	datThawN[[i]]$siteSDW<-seq(1,dim(datThawN[[i]])[1])
	
	#degree days Soil
	#freezing
	datFDDS[[i]] <- datC[[i]][datC[[i]]$parms1=="FDDS",]
	datFDDS[[i]]$siteSDW<-seq(1,dim(datFDDS[[i]])[1])
	#thawing
	datTDDS[[i]] <- datC[[i]][datC[[i]]$parms1=="TDDS",]
	datTDDS[[i]]$siteSDW<-seq(1,dim(datTDDS[[i]])[1])
	#degree days Air
	#freeezing
	datFDDA[[i]] <- datC[[i]][datC[[i]]$parms1=="FDDA",]
	datFDDA[[i]]$siteSDW<-seq(1,dim(datFDDA[[i]])[1])	
	#thawing
	datTDDA[[i]] <- datC[[i]][datC[[i]]$parms1=="TDDA",]
	datTDDA[[i]]$siteSDW<-seq(1,dim(datTDDA[[i]])[1])		
	#now join with ids

	datMax[[i]]<-join(datMax[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datWpeak[[i]]<-join(datWpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datMin[[i]]<-join(datMin[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datSpeak[[i]]<-join(datSpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datZero[[i]]<-join(datZero[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datNthaw[[i]]<-join(datNthaw[[i]], datNIS[[i]], by="Nseq", type="left")
	datNfreeze[[i]]<-join(datNfreeze[[i]], datNIS[[i]], by="Nseq", type="left")
	datTaverageS[[i]] <- join(datTaverageS[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datThawN[[i]] <- join(datThawN[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datTDDS[[i]] <- join(datTDDS[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datFDDS[[i]] <- join(datFDDS[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datTDDA[[i]] <- join(datTDDA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datFDDA[[i]] <- join(datFDDA[[i]],AirSDW[[i]], by="siteSDW", type="left")	
	
	#pull out reps
	datAreps[[i]] <- datC[[i]][datC[[i]]$parms1=="TempA.rep",]
	datSreps[[i]] <- datC[[i]][datC[[i]]$parms1=="TempS.rep",]
	
	
	
}
	

	datTmin<-ldply(datMin,data.frame)
	datTmax<-ldply(datMax,data.frame)
	datWpeak<-ldply(datWpeak,data.frame)
	datSpeak<-ldply(datSpeak,data.frame)
	datZero<-ldply(datZero, data.frame)
	datNfreeze<-ldply(datNfreeze, data.frame)
	datNthaw<-ldply(datNthaw, data.frame)
	datAverageS <- ldply(datTaverageS, data.frame)
	datSreps <- ldply(datSreps)
	datAreps <- ldply(datAreps)
	datThawN <- ldply(datThawN,data.frame)
	datTDDS <- ldply(datTDDS,data.frame)
	datFDDS <- ldply(datFDDS,data.frame)
	datTDDA <- ldply(datTDDA,data.frame)
	datFDDA <- ldply(datFDDA,data.frame)
#subset first to only look at air parms
datMaxA<-list()
datMinA<-list()
datWpeakA<-list()
datSpeakA<-list()
datTaverageA <- list()

for(i in 1:dim(mrunF)[1]){	
	#max
	datMaxA[[i]]<-datC[[i]][datC[[i]]$parms1=="TmaxA",]
	datMaxA[[i]]$siteSDW<-seq(1,dim(datMaxA[[i]])[1])
	#min
	datMinA[[i]]<-datC[[i]][datC[[i]]$parms1=="TminA",]
	datMinA[[i]]$siteSDW<-seq(1,dim(datMinA[[i]])[1])
	#peak timing
	datWpeakA[[i]]<-datC[[i]][datC[[i]]$parms1=="peakWA",]
	datWpeakA[[i]]$siteSDW<-seq(1,dim(datWpeakA[[i]])[1])
	datSpeakA[[i]]<-datC[[i]][datC[[i]]$parms1=="peakSA",]	
	datSpeakA[[i]]$siteSDW<-seq(1,dim(datSpeakA[[i]])[1])
	#average temperature
	datTaverageA[[i]]<-datC[[i]][datC[[i]]$parms1=="TaverageA",]
	datTaverageA[[i]]$siteSDW <- seq(1,dim(datTaverageA[[i]])[1])	
	
	
	datMaxA[[i]]<-join(datMaxA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datWpeakA[[i]]<-join(datWpeakA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datMinA[[i]]<-join(datMinA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datSpeakA[[i]]<-join(datSpeakA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datTaverageA[[i]] <- join(datTaverageA[[i]],AirSDW[[i]], by="siteSDW", type="left")
}
	datTminA<-ldply(datMinA,data.frame)
	datTmaxA<-ldply(datMaxA,data.frame)
	datWpeakA<-ldply(datWpeakA,data.frame)
	datSpeakA<-ldply(datSpeakA,data.frame)
	datAverageA <- ldply(datTaverageA, data.frame)
	#now pull out mu
	
for(i in 1:dim(mrunF)[1]){
	datCSM[[i]]<-datC[[i]][datC[[i]]$parms1=="muS",]
	datCAM[[i]]<-datC[[i]][datC[[i]]$parms1=="muA",]
	datCSM[[i]]$depth<-datSM$depth[datSM$siteid==mrunF$siteid[i]]
	datCAM[[i]]$depth<-datAM$height[datAM$siteid==mrunF$siteid[i]]	
	datCSM[[i]]$ID<-as.numeric(gsub(dexps2,"", row.names(datCSM[[i]] )))
	datCAM[[i]]$ID<-as.numeric(gsub(dexps2,"", row.names(datCAM[[i]] )))
	
}	

###########################################
#pull out the credible intervals and means
#and put into dataframes 
###########################################
freezeN<-data.frame(datNfreeze[,1:2], pc2.5=datNfreeze[,5],pc97.5=datNfreeze[,9],
					datNfreeze[,12:14],depth=datNfreeze[,17],parm=datNfreeze$parms1)


thawN<-data.frame(datNthaw[,1:2], pc2.5=datNthaw[,5],pc97.5=datNthaw[,9],
					datNthaw[,12:14],depth=datNthaw[,17],parm=datNthaw$parms1)
					
Nfactor<-rbind(freezeN,thawN)

zeroC<-data.frame(datZero[,1:2], pc2.5=datZero[,5],pc97.5=datZero[,9],
					datZero[,12:14],parm=datZero$parms1)

					
Speak<-data.frame(datSpeak[,1:2], pc2.5=datSpeak[,5],pc97.5=datSpeak[,9],
					datSpeak[,12:14],parm=datSpeak$parms1)			
					
Wpeak<-data.frame(datWpeak[,1:2], pc2.5=datWpeak[,5],pc97.5=datWpeak[,9],
					datWpeak[,12:14],parm=datWpeak$parms1)							


Tmin<-data.frame(datTmin[,1:2], pc2.5=datTmin[,5],pc97.5=datTmin[,9],
					datTmin[,12:14],parm=datTmin$parms1)					

Tmax<-data.frame(datTmax[,1:2], pc2.5=datTmax[,5],pc97.5=datTmax[,9],
				datTmax[,12:14],parm=datTmax$parms1)
Tave <- data.frame(datAverageS[,1:2], pc2.5=datAverageS[,5],pc97.5=datAverageS[,9],
				datAverageS[,12:14],parm=datAverageS$parms1)

SoilParm<-rbind(zeroC,Speak,Wpeak,Tmin,Tmax, Tave)				

SpeakA<-data.frame(datSpeakA[,1:2], pc2.5=datSpeakA[,5],pc97.5=datSpeakA[,9],
					datSpeakA[,12:14],parm=datSpeakA$parms1)			
					
WpeakA<-data.frame(datWpeakA[,1:2], pc2.5=datWpeakA[,5],pc97.5=datWpeakA[,9],
					datWpeakA[,12:14],parm=datWpeakA$parms1)							


TminA<-data.frame(datTminA[,1:2], pc2.5=datTminA[,5],pc97.5=datTminA[,9],
					datTminA[,12:14],parm=datTminA$parms1)					

TmaxA<-data.frame(datTmaxA[,1:2], pc2.5=datTmaxA[,5],pc97.5=datTmaxA[,9],
				datTmaxA[,12:14],parm=datTmaxA$parms1)		

TaveA <-data.frame(datAverageA[,1:2], pc2.5=datAverageA[,5],pc97.5=datAverageA[,9],
				datAverageA[,12:14],parm=datAverageA$parms1)
				
AirParm<-rbind(SpeakA, WpeakA,TminA,TmaxA, TaveA)

ThawParm <- data.frame(datThawN[,1:2], pc2.5=datThawN[,5],pc97.5=datThawN[,9],
				datThawN[,12:14],parm=datThawN$parms1)

FDDS <-		data.frame(datFDDS[,1:2], pc2.5=datFDDS[,5],pc97.5=datFDDS[,9],
				datFDDS[,12:14],parm=datFDDS$parms1)

FDDA <-		data.frame(datFDDA[,1:2], pc2.5=datFDDA[,5],pc97.5=datFDDA[,9],
				datFDDA[,12:14],parm=datFDDA$parms1)				
TDDS <-		data.frame(datTDDS[,1:2], pc2.5=datTDDS[,5],pc97.5=datTDDS[,9],
				datTDDS[,12:14],parm=datTDDS$parms1)

TDDA <-		data.frame(datTDDA[,1:2], pc2.5=datTDDA[,5],pc97.5=datTDDA[,9],
				datTDDA[,12:14],parm=datTDDA$parms1)

DDsoil <- rbind(FDDS,TDDS)				
DDair <- rbind(FDDA,TDDA)	

#join rep id with replicate data

AirRepID <- cbind(datARdf, datAreps)
SoilRepID <- cbind(datSRdf, datSreps)

rm(list=setdiff(ls(), c("AirParm", "SoilParm","Nfactor", "AirRepID","SoilRepID", "datCSM", "datCAM","datAM", "datSM",
						"datAT","datST","datNI","datAI","datSI","ThawParm","DDsoil","DDair")))