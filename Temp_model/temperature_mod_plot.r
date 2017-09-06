

library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\output_u7m11r1")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")

datNI<-read.csv("ncomboIDS.csv")

datAT<-read.csv("AirTaveIDS_SD.csv")
datST<-read.csv("SoilTaveIDS_SD.csv")

datAM<-read.csv("Tair_model.csv")
datSM<-read.csv("Tsoil_model.csv")

datSM$decdateA<-datSM$decdate-1991
datAM$decdateA<-datAM$decdate-1991

#set up directories for all model output
WDR<-c("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\output_u7m11r1",
"c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\output_u7m11r2")

Nruns <- 2

#indicate start and last dims of site runs
startdim<-c(1,53)
enddim<-c(52,104)



#get a list of the sites that should be expected
siteall<-data.frame(siteid=unique(datSI$siteid))
siteall$siteUI<-seq(1,dim(siteall)[1])



#now organize rep IDS, need to pull correct ones for the site
#in the run it is actually used in

#read in model output from first part
#calculate the run lengh
rleng<-enddim-(startdim-1)
#set up a site run id
siterun <- rep(seq(1,Nruns),times=rleng)

datSRALL<-list()
datARALL<-list()

for(i in 1:length(startdim)){
	datSRALL[[i]]<-read.csv(paste0(WDR[i],"\\SoilrepIDS.csv"))
	datARALL[[i]]<-read.csv(paste0(WDR[i],"\\AirrepIDS.csv"))
}

#get the correct IDS for each site
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
	datM[[i]]<-read.csv(paste0(WDR[siterun[i]],"//site",siteall$siteid[i],"Temp_mod_stats.csv"))
	datQ[[i]]<-read.csv(paste0(WDR[siterun[i]],"//site",siteall$siteid[i],"Temp_mod_quant.csv"))
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
	
###########################################################
###########################################################
####plot the means

#now need to set up plotting for each site
#get unique site list from soil params
sitesS<-data.frame(siteid=unique(datSI$siteid))
sitesS$siteun<-seq(1,dim(sitesS)[1])
depthP<-list()
wyearP<-list()
depthPA<-list()
wyearPA<-list()
#set up depths and colors for depths in each site
colP<-c(terrain.colors(7),heat.colors(10),topo.colors(10))
for(i in 1:dim(siteall)[1]){
	depthP[[i]]<-unique(datSI$depth[datSI$siteid==sitesS$siteid[i]])
	wyearP[[i]]<-unique(datSI$wyear[datSI$siteid==sitesS$siteid[i]])
	depthPA[[i]]<-unique(datAI$height[datAI$siteid==sitesS$siteid[i]])
	wyearPA[[i]]<-unique(datAI$wyear[datAI$siteid==sitesS$siteid[i]])	
	
}

#plot the soil
#create predicted points
datSM$decdateA[datSM$siteid==1&datSM$depth==depthP[[1]][1]]

for(n in 1:dim(siteall)[1]){
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\plots\\soil\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]),max(datSM$decdateA[datSM$siteid==i])),
								ylim=c(min(na.omit(datSM$T[datSM$siteid==i])),max(na.omit(datSM$T[datSM$siteid==i]))),
								xlab="Water Year since 1991", ylab="Temperature", cex.axis=2, cex.lab=3)
		
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col=colP[j])
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datCSM[[n]]$Mean[datCSM[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)

	}

	legend(min(na.omit(datSM$decdateA[datSM$siteid==i]))+.001,max(na.omit(datSM$T[datSM$siteid==i]))-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=3)
			
	text(min(datSM$decdateA[datSM$siteid==i])+2,max(datSM$T[datSM$siteid==i])-2, paste("siteid=", sitesS$siteid[n]), cex=3)
	dev.off()
}
#now plot air


for(n in 1:dim(siteall)[1]){
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\plots\\air\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(min(datAM$decdateA[datAM$siteid==i]),max(datAM$decdateA[datAM$siteid==i])),
								ylim=c(min(na.omit(datAM$A[datAM$siteid==i])),max(na.omit(datAM$A[datAM$siteid==i]))),
								xlab="Water Year since 1991", ylab="Temperature", cex.axis=2, cex.lab=3)
		
		
	for(j in 1:length(depthPA[[n]])){
		points(datAM$decdateA[datAM$siteid==i&datAM$height==depthPA[[n]][j]],
				datAM$A[datAM$siteid==i&datAM$height==depthPA[[n]][j]],
				pch=19, col=colP[j])
		points(datAM$decdateA[datAM$siteid==i&datAM$height==depthPA[[n]][j]],
				datCAM[[n]]$Mean[datCAM[[n]]$depth==depthPA[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)

	}

	legend(min(na.omit(datAM$decdateA[datAM$siteid==i]))+.001,max(na.omit(datAM$A[datAM$siteid==i]))-.25, paste("height=", depthPA[[n]]),
			col=colP[1:length(depthPA[[n]])],pch=19, bty="n", cex=3)
			
	text(min(datAM$decdateA[datAM$siteid==i])+2,max(na.omit(datAM$A[datAM$siteid==i]))-2, paste("siteid=", sitesS$siteid[n]), cex=3)
	dev.off()
}

#################################################
#############now look at replicated data

#need to pull out matching temp data

#now subset the temperatures based on ids
datobsS<-list()
datobsA<-list()
IDsubS<-list()
IDsubA<-list()
datobsS1<-list()
datobsA1<-list()
datCSR<-list()
datCAR<-list()

for(i in 1:dim(siteall)[1]){
	#pull out data and ids for each site
	datobsS[[i]]<-datSM$T[datSM$siteid==siteall$siteid[i]]
	datobsA[[i]]<-datAM$A[datAM$siteid==siteall$siteid[i]]
	IDsubS[[i]]<-datSR[[i]]$repID
	IDsubA[[i]]<-datAR[[i]]$repID
	#now subset each site
	datobsS1[[i]]<-data.frame(Tobs=datobsS[[i]][IDsubS[[i]]])
	datobsS1[[i]]$siteid<-rep(siteall$siteid[i],length(datobsS1[[i]]))
	datobsA1[[i]]<-data.frame(Aobs=datobsA[[i]][IDsubA[[i]]])
	datobsA1[[i]]$siteid<-rep(siteall$siteid[i],length(datobsA1[[i]]))
	
	datCSR[[i]]<-datC[[i]][datC[[i]]$parms1=="TempS.rep",]
	datCAR[[i]]<-datC[[i]][datC[[i]]$parms1=="TempA.rep",]		
}	

p1soilO<-ldply(datobsS1, data.frame)
p1airO<-ldply(datobsA1, data.frame)

p1CSR<-ldply(datCSR, data.frame)
p1CAR<-ldply(datCAR, data.frame)


########make plots of model fit

par(mfrow=c(1,2))
plot(p1soilO$Tobs, p1CSR$Mean, pch=19, ylim=c(-40,30), xlim=c(-40,30), 
	ylab="Soil Temperature Predicted (C)", xlab="Soil Temperature Obs")
abline(0,1, col="red", lwd=2)
fitsoil<-lm(p1CSR$Mean~p1soilO$Tobs)
summary(fitsoil)
abline(fitsoil, lwd=2, col="cornflowerblue", lty=2)
text(-20,25, paste("y=",round(fitsoil$coefficients[1],2), "+",round(fitsoil$coefficients[2],2),"Tobs"), cex=1.5)
text(-20,20, paste("R2=", round(summary(fitsoil)$r.squared,3)), cex=1.5)
plot(p1airO$Aobs, p1CAR$Mean, pch=19, ylim=c(-50,30), xlim=c(-50,30), 
	ylab="Air Temperature Predicted (C)", xlab="Air Temperature Obs")
abline(0,1, col="red", lwd=2)
fitair<-lm(p1CAR$Mean~p1airO$Aobs)
summary(fitair)
abline(fitair, lwd=2, col="cornflowerblue", lty=2)
text(-20,25, paste("y=",round(fitair$coefficients[1],2), "+",round(fitair$coefficients[2],2),"Tobs"), cex=1.5)
text(-20,20, paste("R2=", round(summary(fitair)$r.squared,3)), cex=1.5)
#########################################################



##################################################################
##################################################################
# make a plot of a few sites for an example plot that looks nicer
#than the rough plots above to check for 

#start by looking at site 79
	n<-sitesS$siteun[sitesS$siteid==79]

	i<-79
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\u7m11\\nice_plots\\soil\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]+1991),max(datSM$decdateA[datSM$siteid==i]+1991)),
								ylim=c(min(na.omit(datSM$T[datSM$siteid==i])),15),axes=FALSE,
								xlab=" ", ylab=" " )
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]]+1991,
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col="tomato3", cex=2)
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]]+1991,
				datCSM[[n]]$Mean[datCSM[[n]]$depth==depthP[[n]][j]],
				col="gray20",type="l",lwd=7,lty=1)

	}
	mtext("Water year", side=1, line=5, cex=4)
	mtext("Temperature (C) ", side=2, line=6, cex=4)
	axis(1, seq(2011.5,2015.5, by=.5), rep(" ", 9),cex.axis=3,lwd.ticks=2)
	axis(2, seq(-30,20, by=5), cex.axis=3, las=2,lwd.ticks=2)
	mtext("2012", side=1, line=2, cex=3, at=2012)
	mtext("2013", side=1, line=2, cex=3, at=2013)
	mtext("2014", side=1, line=2, cex=3, at=2014)
	mtext("2015", side=1, line=2, cex=3, at=2015)
	legend(2013.5,-18, c("observed daily average temperature","model mean daily average temperature"),
				pch=c(19, NA), lty=c(NA,1), lwd=c(NA,6), col=c("tomato3", "gray20"), cex=2.5, bty="n")
			
	dev.off()

