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
### dataframes are Nfactor, SoilParm, AirParm          ###
##########################################################
##########################################################

#libraries
library(plyr)

############################
##Model run specifications##
############################

#specify the number of runs
Nruns <- 2

#set up directories for all model output
WDR <- c("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\output_u7m11r1",
        "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\output_u7m11r2"
)


#indicate starting siteID of each run 
startdim <- c(1,53)
#last siteID of each run
enddim <- c(52,104)

############################
##Read in data            ##
############################

#working directory for data
#always refer to first model run
dataWD <- WDR[1]

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

datAveIS <- read.csv(paste0(dataWD,"\\IDaveoutS.csv"))
datAveIA <- read.csv(paste0(dataWD,"\\IDaveoutA.csv"))
#get a list of the sites that should be expected
siteall <- data.frame(siteid=unique(datSI$siteid))
siteall$siteUI <- seq(1,dim(siteall)[1])


#now organize rep IDS, need to pull correct ones for the site
#in the run it is actually used in


#calculate the run lengh
rleng<-enddim-(startdim-1)
#set up a site run id
siterun <- rep(seq(1,Nruns),times=rleng)

datSRALL<-list()
datARALL<-list()

for(i in 1:Nruns){
	datSRALL[[i]]<-read.csv(paste0(WDR[i],"\\SoilrepIDS.csv"))
	datARALL[[i]]<-read.csv(paste0(WDR[i],"\\AirrepIDS.csv"))
}

#get the correct IDS for each site generated in the site run
datSR<-list()
datAR<-list()
for(i in 1:dim(siteall)[1]){
	datSR[[i]]<-datSRALL[[siterun[i]]][datSRALL[[siterun[i]]]$siteid==siteall$siteid[i],]
	datAR[[i]]<-datARALL[[siterun[i]]][datARALL[[siterun[i]]]$siteid==siteall$siteid[i],]
}

datSRdf<-ldply(datSR, data.frame)
datARdf<-ldply(datAR, data.frame)

#expression to remove bracket and vector number from parm names
dexps<-"\\[*[[:digit:]]*\\]"

datM<-list()
datQ<-list()
datC<-list()
for(i in 1:dim(siteall)[1]){
	datM[[i]]<-read.csv(paste0(WDR[siterun[i]],"\\site",siteall$siteid[i],"Temp_mod_stats.csv"))
	datQ[[i]]<-read.csv(paste0(WDR[siterun[i]],"\\site",siteall$siteid[i],"Temp_mod_quant.csv"))
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

for(i in 1:dim(siteall)[1]){

	AirSDW[[i]]<-datAI[datAI$siteid==siteall$siteid[i],]
	AirSDW[[i]]$siteSDW<-seq(1,dim(AirSDW[[i]])[1])
	
	SoilSDW[[i]]<-datSI[datSI$siteid==siteall$siteid[i],]
	SoilSDW[[i]]$siteSDW<-seq(1,dim(SoilSDW[[i]])[1])
	
	AirTA[[i]]<-datAT[datAT$siteid==siteall$siteid[i],]
	SoilTA[[i]]<-datST[datST$siteid==siteall$siteid[i],]
	
	#pull out N factor IDs by site
	datNIS[[i]]<-datNI[datNI$siteid==siteall$siteid[i],]
	#ave IDs
	datAvSID[[i]] <- datAveIS[datAveIS$siteid==siteall$siteid[i],]
	datAvAID[[i]] <- datAveIA[datAveIA$siteid==siteall$siteid[i],]
	
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
datTaveA <- list()
datTaveS <- list()
datSTav<- list()
datATav<- list()

datCSM<-list()
datCAM<-list()

#now pull out id number
dexps2<-"\\D"

#subset first to only look at soil parms
for(i in 1:dim(siteall)[1]){

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


	
	
	#now join with ids

	datMax[[i]]<-join(datMax[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datWpeak[[i]]<-join(datWpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datMin[[i]]<-join(datMin[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datSpeak[[i]]<-join(datSpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datZero[[i]]<-join(datZero[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datNthaw[[i]]<-join(datNthaw[[i]], datNIS[[i]], by="Nseq", type="left")
	datNfreeze[[i]]<-join(datNfreeze[[i]], datNIS[[i]], by="Nseq", type="left")
	datATav[[i]] <- join(datTaveA[[i]], datAvAID[[i]], by="Tave1IDA", type="left")
	datSTav[[i]] <- join(datTaveS[[i]], datAvSID[[i]], by="Tave1ID", type="left")
	
	}
	

	datTmin<-ldply(datMin,data.frame)
	datTmax<-ldply(datMax,data.frame)
	datWpeak<-ldply(datWpeak,data.frame)
	datSpeak<-ldply(datSpeak,data.frame)
	datZero<-ldply(datZero, data.frame)
	datNfreeze<-ldply(datNfreeze, data.frame)

#subset first to only look at air parms
datMaxA<-list()
datMinA<-list()
datWpeakA<-list()
datSpeakA<-list()

for(i in 1:dim(siteall)[1]){	
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
	
	datMaxA[[i]]<-join(datMaxA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datWpeakA[[i]]<-join(datWpeakA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datMinA[[i]]<-join(datMinA[[i]],AirSDW[[i]], by="siteSDW", type="left")
	datSpeakA[[i]]<-join(datSpeakA[[i]],AirSDW[[i]], by="siteSDW", type="left")
}
	datTminA<-ldply(datMinA,data.frame)
	datTmaxA<-ldply(datMaxA,data.frame)
	datWpeakA<-ldply(datWpeakA,data.frame)
	datSpeakA<-ldply(datSpeakA,data.frame)
	#now pull out mu
	
for(i in 1:dim(siteall)[1]){
	datCSM[[i]]<-datC[[i]][datC[[i]]$parms1=="muS",]
	datCAM[[i]]<-datC[[i]][datC[[i]]$parms1=="muA",]
	datCSM[[i]]$depth<-datSM$depth[datSM$siteid==siteall$siteid[i]]
	datCAM[[i]]$depth<-datAM$height[datAM$siteid==siteall$siteid[i]]	
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

Tave<- data.frame(datTaS[,1:2],pc2.5=datTaS[,5],pc97.5=datTaS[,9],
				datTaS[,12:14],parm=datTaS$parms1)				

SoilParm<-rbind(zeroC,Speak,Wpeak,Tmin,Tmax, Tave)				

SpeakA<-data.frame(datSpeakA[,1:2], pc2.5=datSpeakA[,5],pc97.5=datSpeakA[,9],
					datSpeakA[,12:14],parm=datSpeakA$parms1)			
					
WpeakA<-data.frame(datWpeakA[,1:2], pc2.5=datWpeakA[,5],pc97.5=datWpeakA[,9],
					datWpeakA[,12:14],parm=datWpeakA$parms1)							


TminA<-data.frame(datTminA[,1:2], pc2.5=datTminA[,5],pc97.5=datTminA[,9],
					datTminA[,12:14],parm=datTminA$parms1)					

TmaxA<-data.frame(datTmaxA[,1:2], pc2.5=datTmaxA[,5],pc97.5=datTmaxA[,9],
				datTmaxA[,12:14],parm=datTmaxA$parms1)		

TaveA <- data.frame(datTaA[,1:2],pc2.5=datTaA[,5],pc97.5=datTaA[,9],
				datTaA[,12:14],parm=datTaA$parms1)					
AirParm<-rbind(SpeakA, WpeakA,TminA,TmaxA, TaveA)

