

library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod9\\output_u7m9")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")

datAIS<-read.csv("AirIDS_SD.csv")
datSIS<-read.csv("SoilIDS_SD.csv")

datAM<-read.csv("Tair_model.csv")
datSM<-read.csv("Tsoil_model.csv")

datSM$decdateA<-datSM$decdate-1991
datAM$decdateA<-datAM$decdate-1991

#get a list of the sites that should be expected
siteall<-data.frame(siteid=unique(datSI$siteid))
siteall$siteUI<-seq(1,dim(siteall)[1])


#expression to remove bracket and vector number from parm names
dexps<-"\\[*[[:digit:]]*\\]"


#read in model output
datM<-list()
datQ<-list()
datC<-list()
for(i in 1:dim(siteall)[1]){
	datM[[i]]<-read.csv(paste0("site",siteall$siteid[i],"Temp_mod_stats.csv"))
	datQ[[i]]<-read.csv(paste0("site",siteall$siteid[i],"Temp_mod_quant.csv"))
	#join means and quantiles
	datC[[i]]<-cbind(datM[[i]],datQ[[i]])
	#make parms vectors
	datC[[i]]$parms1<-gsub(dexps,"",rownames(datC[[i]]))
}

#now pull out more ID info

AirSDW<-list()
SoilSDW<-list()
AirSD<-list()
SoilSD<-list()


for(i in 1:dim(siteall)[1]){

	AirSDW[[i]]<-datAI[datAI$siteid==siteall$siteid[i],]
	AirSDW[[i]]$siteSDW<-seq(1,dim(AirSDW[[i]])[1])
	SoilSDW[[i]]<-datSI[datSI$siteid==siteall$siteid[i],]
	SoilSDW[[i]]$siteSDW<-seq(1,dim(SoilSDW[[i]])[1])	
	SoilSD[[i]]<-datSIS[datSIS$siteid==siteall$siteid[i],]
	SoilSD[[i]]$siteSD<-seq(1,dim(SoilSD[[i]])[1])
	AirSD[[i]]<-datAIS[datAIS$siteid==siteall$siteid[i],]
	AirSD[[i]]$siteSD<-seq(1,dim(AirSD[[i]])[1])	
	colnames(AirSD[[i]])[3]<-"SDS"
}


datT1<-list()
datT2<-list()
datMax<-list()
datMin<-list()
datWpeak<-list()
datSpeak<-list()

datCSM<-list()
datCAM<-list()

#now pull out id number
dexps2<-"\\D"

#subset first to only look at soil parms
for(i in 1:dim(siteall)[1]){
	datT1[[i]]<-datC[[i]][datC[[i]]$parms1=="T.aveS1",]
	datT1[[i]]$siteSDW<-seq(1,dim(datT1[[i]])[1])
	datT2[[i]]<-datC[[i]][datC[[i]]$parms1=="T.aveS2",]
	datT2[[i]]$siteSDW<-seq(1,dim(datT2[[i]])[1])
	datMax[[i]]<-datC[[i]][datC[[i]]$parms1=="TmaxS",]
	datMax[[i]]$siteSDW<-seq(1,dim(datMax[[i]])[1])
	datMin[[i]]<-datC[[i]][datC[[i]]$parms1=="TminS",]
	datMin[[i]]$siteSDW<-seq(1,dim(datMin[[i]])[1])
	datWpeak[[i]]<-datC[[i]][datC[[i]]$parms1=="peakWS",]
	datWpeak[[i]]$siteSDW<-seq(1,dim(datWpeak[[i]])[1])
	datSpeak[[i]]<-datC[[i]][datC[[i]]$parms1=="peakSS",]	
	datSpeak[[i]]$siteSDW<-seq(1,dim(datSpeak[[i]])[1])
	
	datT1[[i]]<-join(datT1[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datT2[[i]]<-join(datT2[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datMax[[i]]<-join(datMax[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datWpeak[[i]]<-join(datWpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datMin[[i]]<-join(datMin[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	datSpeak[[i]]<-join(datSpeak[[i]],SoilSDW[[i]], by="siteSDW", type="left")
	
	}
	
	datTave1<-ldply(datT1,data.frame)
	datTave2<-ldply(datT2,data.frame)
	datTmin<-ldply(datMin,data.frame)
	datTmax<-ldply(datMax,data.frame)
	datWpeak<-ldply(datWpeak,data.frame)
	datSpeak<-ldply(datSpeak,data.frame)
	

	#now pull out mu
	
for(i in 1:dim(siteall)[1]){
	datCSM[[i]]<-datC[[i]][datC[[i]]$parms1=="muS",]
	datCAM[[i]]<-datC[[i]][datC[[i]]$parms1=="muA",]
	datCSM[[i]]$depth<-datSM$depth[datSM$siteid==siteall$siteid[i]]
	datCAM[[i]]$depth<-datAM$height[datAM$siteid==siteall$siteid[i]]	
	datCSM[[i]]$ID<-as.numeric(gsub(dexps2,"", row.names(datCSM[[i]] )))
	datCAM[[i]]$ID<-as.numeric(gsub(dexps2,"", row.names(datCAM[[i]] )))
	
}	
	
for(i in 1:dim(siteall)[1]){	
	#now join with ID info
	for(j in 1:dim(datCS[[i]])[1]){
		if(length(psplit[[i]][[j]])>1){
			pEnd[[i]][j]<-psplit[[i]][[j]]	
		}else{pEnd[i]<-"NA"}
	}
	
}





#datRS$ID<-gsub(dexps2,"",rownames(datRS))

#datRR<-data.frame(M=datRS$Mean, pc2.5=datRS$X2.5.,pc97.5=datRS$X97.5., param=datRS$parms1,ID=datRS$ID)

#pull out names
#pnames<-rownames(datCS)
#need to split because there are numbers in param names

#pull out vector number
#pEnd<-character(0)


	

#}

#get vector number only and make numeric
#parmCN<-ifelse(pEnd=="NA", NA, gsub(dexps2,"", pEnd ))

#datCS$parms2<-c(as.numeric(parmCN))

#datC<-data.frame(M=datCS[,1],pc2.5=datCS[,5],pc97.5=datCS[,9],param=as.character(datCS[,10]),ID=datCS[,11])


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
for(i in 1:dim(sitesS)[1]){
	depthP[[i]]<-unique(datSI$depth[datSI$siteid==sitesS$siteid[i]])
	wyearP[[i]]<-unique(datSI$wyear[datSI$siteid==sitesS$siteid[i]])
	depthPA[[i]]<-unique(datAI$height[datAI$siteid==sitesS$siteid[i]])
	wyearPA[[i]]<-unique(datAI$wyear[datAI$siteid==sitesS$siteid[i]])	
	
}

#plot the soil
#create predicted points
datSM$decdateA[datSM$siteid==1&datSM$depth==depthP[[1]][1]]



for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod9\\plots\\soil\\site",i,".jpg"),
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


for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod9\\plots\\air\\site",i,".jpg"),
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
