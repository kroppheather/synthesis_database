##########################################################
########Temperature parameter extraction script###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script extracts output from the temperature   ###
### model output obtained from the Deep_Temp scripts.  ###
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
WDR <- c("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\deep\\run1\\sitesp1",
        "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\deep\\run1\\sitesp2")

##Note only goes 1-18, and 21-37 since model run didn't finish in time for poster


#indicate starting siteID of each run 
startdim <- c(1,21)
#last siteID of each run
enddim <- c(17,37)
#deal with missing sites
siterunAc <- data.frame(siteUIT=c(seq(startdim[1],enddim[1]),seq(startdim[2],enddim[2])))


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



#get a list of the sites that should be expected
siteall <- data.frame(siteid=unique(datSI$siteid))
siteall$siteUIT <- seq(1,dim(siteall)[1])

#join site run to data frame to remove sites
siteall <- join(siteall, siterunAc, by="siteUIT", type="inner")
#create new seq
siteall$siteUI <- seq(1, dim(siteall)[1])

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

for(i in 1:dim(siteall)[1]){

	AirSDW[[i]]<-datAI[datAI$siteid==siteall$siteid[i],]
	AirSDW[[i]]$siteSDW<-seq(1,dim(AirSDW[[i]])[1])
	
	SoilSDW[[i]]<-datSI[datSI$siteid==siteall$siteid[i],]
	SoilSDW[[i]]$siteSDW<-seq(1,dim(SoilSDW[[i]])[1])
	
	AirTA[[i]]<-datAT[datAT$siteid==siteall$siteid[i],]
	SoilTA[[i]]<-datST[datST$siteid==siteall$siteid[i],]
	
	#pull out N factor IDs by site
	datNIS[[i]]<-datNI[datNI$siteid==siteall$siteid[i],]
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

datCSM<-list()
datCAM<-list()

#now pull out id number
dexps2<-"\\D"

#subset first to only look at soil parms
for(i in 1:dim(siteall)[1]){
	#beginning of year
	datT1p1[[i]]<-datC[[i]][datC[[i]]$parms1=="T.aveS1",]
	datT1p1[[i]]$Tave1ID<-seq(1,dim(datT1p1[[i]])[1])
	#end of year temp
	datT1p2[[i]]<-datC[[i]][datC[[i]]$parms1=="T.aveS1",]
	datT1p2[[i]]$Tave2D<-seq(1,dim(datT1p2[[i]])[1])
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
	datT1t1[[i]]<-join(datT1p1[[i]],SoilTA[[i]], by="Tave1ID", type="inner")
	datT1t2[[i]]<-join(datT1p2[[i]],SoilTA[[i]], by="Tave2D", type="inner")
	datMax[[i]]<-join(datMax[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datWpeak[[i]]<-join(datWpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datMin[[i]]<-join(datMin[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datSpeak[[i]]<-join(datSpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datZero[[i]]<-join(datZero[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datNthaw[[i]]<-join(datNthaw[[i]], datNIS[[i]], by="Nseq", type="left")
	datNfreeze[[i]]<-join(datNfreeze[[i]], datNIS[[i]], by="Nseq", type="left")
	}
	
	datTave1<-ldply(datT1t1,data.frame)
	datTave2<-ldply(datT1t2,data.frame)
	datTmin<-ldply(datMin,data.frame)
	datTmax<-ldply(datMax,data.frame)
	datWpeak<-ldply(datWpeak,data.frame)
	datSpeak<-ldply(datSpeak,data.frame)
	datZero<-ldply(datZero, data.frame)
	datNfreeze<-ldply(datNfreeze, data.frame)
	datNthaw<-ldply(datNthaw, data.frame)

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

SoilParm<-rbind(zeroC,Speak,Wpeak,Tmin,Tmax)				

SpeakA<-data.frame(datSpeakA[,1:2], pc2.5=datSpeakA[,5],pc97.5=datSpeakA[,9],
					datSpeakA[,12:14],parm=datSpeakA$parms1)			
					
WpeakA<-data.frame(datWpeakA[,1:2], pc2.5=datWpeakA[,5],pc97.5=datWpeakA[,9],
					datWpeakA[,12:14],parm=datWpeakA$parms1)							


TminA<-data.frame(datTminA[,1:2], pc2.5=datTminA[,5],pc97.5=datTminA[,9],
					datTminA[,12:14],parm=datTminA$parms1)					

TmaxA<-data.frame(datTmaxA[,1:2], pc2.5=datTmaxA[,5],pc97.5=datTmaxA[,9],
				datTmaxA[,12:14],parm=datTmaxA$parms1)		

AirParm<-rbind(SpeakA, WpeakA,TminA,TmaxA)