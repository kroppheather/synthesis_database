library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod6\\output_u7m6")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")
datM<-read.csv("Temp_mod6_stats.csv")
datQ<-read.csv("Temp_mod6_quant.csv")

datAIS<-read.csv("AirIDS_SD.csv")
datSIS<-read.csv("SoilIDS_SD.csv")

datAM<-read.csv("Tair_model.csv")
datSM<-read.csv("Tsoil_model.csv")

datSF<-read.csv("startF.csv")

datSM$decdateA<-datSM$decdate-1991
datAM$decdateA<-datAM$decdate-1991

#read in temperature data used in model

#now join means with quantiles
datC<-cbind(datM,datQ)
#make a param vector
dexps<-"\\[*[[:digit:]]*\\]"
datC$parms1<-gsub(dexps,"",rownames(datC))

#subset first to only look at soil parms
datCS<-datC[datC$parms1=="T.aveS1"|datC$parms1=="T.aveS2"|datC$parms1=="TmaxS"|datC$parms1=="TminS"|datC$parms1=="startS"|datC$parms1=="aZero"|datC$parms1=="bZero",]

#pull out zeroC value
zeroMeanP<-datC[datC$parms1=="zeroC",]
zeroMean<-zeroMeanP$Mean
zeroSD<-zeroMeanP$SD


datRS<-datC[datC$parms1=="TempS.rep",]


#now add id number
dexps2<-"\\D"

datRS$ID<-gsub(dexps2,"",rownames(datRS))

datRR<-data.frame(M=datRS$Mean, pc2.5=datRS$X2.5.,pc97.5=datRS$X97.5., param=datRS$parms1,ID=datRS$ID)

#pull out names
pnames<-rownames(datCS)
#need to split because there are numbers in param names
psplit<-strsplit(pnames, "\\[")
#pull out vector number
pEnd<-character(0)
for(i in 1:dim(datCS)[1]){
	if(length(psplit[[i]])>1){
		pEnd[i]<-psplit[[i]][2]
	}else{pEnd[i]<-"NA"}

}

#get vector number only and make numeric
parmCN<-ifelse(pEnd=="NA", NA, gsub(dexps2,"", pEnd ))

datCS$parms2<-c(as.numeric(parmCN))

datC<-data.frame(M=datCS[,1],pc2.5=datCS[,5],pc97.5=datCS[,9],param=as.character(datCS[,10]),ID=datCS[,11])



#now pull out params needed for sine model
sTa1<-datC[datC$param=="T.aveS1",]
sTa2<-datC[datC$param=="T.aveS2",]
sTmin<-datC[datC$param=="TminS",]
sTmax<-datC[datC$param=="TmaxS",]
sTstart<-datC[datC$param=="startS",]
sAzero<-datC[datC$param=="aZero",]
sBzero<-datC[datC$param=="bZero",]
colnames(sTa1)[5]<-"SDWS"
colnames(sTa2)[5]<-"SDWS"
colnames(sTmin)[5]<-"SDWS"
colnames(sTmax)[5]<-"SDWS"
colnames(sTstart)[5]<-"SDS"
colnames(sAzero)[5]<-"SDWS"
colnames(sBzero)[5]<-"SDWS"

#now join with IDS
sTa1<-join(sTa1, datSI, by="SDWS", type="left")
sTa2<-join(sTa2, datSI, by="SDWS", type="left")
sTmin<-join(sTmin, datSI, by="SDWS", type="left")
sTmax<-join(sTmax, datSI, by="SDWS", type="left")
sTstart<-join(sTstart, datSIS, by="SDS", type="left")
sAzero<-join(sAzero, datSI, by="SDWS", type="left")
sBzero<-join(sBzero, datSI, by="SDWS", type="left")

#now make plots by site to see how this compares
#set up sine function
zeroF<-function(a,b,tdiff){
	a*exp(-b*tdiff)


}



Tsine1<-function(Tave1,Tmin,Tyear,Tstart){
		
		ifelse(((Tyear-Tstart)-floor(Tyear))<.25,
			Tave1-((Tave1-Tmin)*sin(2*3.14159265*(Tyear-Tstart))),NA)
		}
Tsine2<-function(Tmin,Tmax,Tyear,Tstart){
			ifelse(((Tyear-Tstart)-floor(Tyear))>=.25&((Tyear-Tstart)-floor(Tyear))<.75,
					(Tmin+((Tmax-Tmin)/2))-(((Tmax-Tmin)/2)*sin(2*3.14159265*(Tyear-Tstart))),NA)
					
			}		
Tsine3<-function(Tave2,Tmax,Tyear,Tstart){				
		ifelse(((Tyear-Tstart)-floor(Tyear))>=.75,Tave2-((Tmax-Tave2)*sin(2*3.14159265*(Tyear-Tstart))),NA)
					
		}
#now need to set up plotting for each site
#get unique site list from soil params
sitesS<-data.frame(siteid=unique(datSI$siteid))
sitesS$siteun<-seq(1,dim(sitesS)[1])
depthP<-list()
wyearP<-list()
#set up depths and colors for depths in each site
colP<-c(terrain.colors(7),heat.colors(10),topo.colors(10))
for(i in 1:dim(sitesS)[1]){
	depthP[[i]]<-unique(datSI$depth[datSI$siteid==sitesS$siteid[i]])
	wyearP[[i]]<-unique(datSI$wyear[datSI$siteid==sitesS$siteid[i]])
}

#pull out data for temperature diff
datDID<-ifelse(datSF$x==-1,NA,datSM$indexI-1)
datSM$datTdev<-ifelse(is.na(datDID),-999,abs(datSM$T[datDID]))


#set up list of x variables for sine function
xP<-list()
xPwyear<-list()
DFtest<-list()
DFP1<-list()
DFP2<-list()
DFP3<-list()
DFP4<-list()
DFP5<-list()
DFtest3<-list()
DFtest4<-list()
DFtest5<-list()
DFtest6<-list()
DFtest7<-list()
Ysine1<-list()
Ysine2<-list()
Ysine3<-list()
Ztest1<-list()
Ztest2<-list()
ZP1<-list()
DDx<-list()
DDx1<-list()
DDx2<-list()
DDx3<-list()
DDx4<-list()
DDx5<-list()
DDx6<-list()
DDsine1<-list()
DDsine2<-list()
DDsine3<-list()
DDsineC<-list()
DDmix<-list()
DDfilter<-list()
DDfilter2<-list()
DDprob<-list()


for(i in 1:dim(sitesS)[1]){
	xP[[i]]<-seq(floor(min(datSM$decdateA[datSM$siteid==sitesS$siteid[i]])),
				ceiling(max(datSM$decdateA[datSM$siteid==sitesS$siteid[i]])),
				by=.01)
	#zero is 1991 jan 1 here			
	xPwyear[[i]]<-floor(xP[[i]])+1991	
	
	#below wyear is treated as a factor so 1991 is 1 in the factor list
	DFtest[[i]]<-data.frame(x=rep(xP[[i]],length(depthP[[i]])),wyear=rep(xPwyear[[i]],length(depthP[[i]])),
							depth=rep(depthP[[i]], each=length(xP[[i]])))
					
	DFP1[[i]]<-data.frame(MT=sTa1$M[sTa1$siteid==sitesS$siteid[i]],
		depth=sTa1$depth[sTa1$siteid==sitesS$siteid[i]],
				wyear=as.numeric(sTa1$wyear[sTa1$siteid==sitesS$siteid[i]]))
				
	DFtest3[[i]]<-join(DFtest[[i]],DFP1[[i]], by=c("wyear", "depth"), type="left")
					
	DFP2[[i]]<-data.frame(MT2=sTa2$M[sTa2$siteid==sitesS$siteid[i]],
		depth=sTa2$depth[sTa2$siteid==sitesS$siteid[i]],
				wyear=as.numeric(sTa2$wyear[sTa2$siteid==sitesS$siteid[i]]))

					
	DFP3[[i]]<-data.frame(MS=sTstart$M[sTstart$siteid==sitesS$siteid[i]],
		depth=sTstart$depth[sTstart$siteid==sitesS$siteid[i]])	

	DFtest4[[i]]<-join(DFtest3[[i]],DFP2[[i]], by=c("wyear", "depth"), type="left")
	DFtest5[[i]]<-join(DFtest4[[i]],DFP3[[i]], by=c( "depth"), type="left")
	
	DFP4[[i]]<-data.frame(MMA=sTmax$M[sTmax$siteid==sitesS$siteid[i]],
		depth=sTmax$depth[sTmax$siteid==sitesS$siteid[i]],
				wyear=as.numeric(sTmax$wyear[sTmax$siteid==sitesS$siteid[i]]))	
				
	DFP5[[i]]<-data.frame(MMI=sTmin$M[sTmin$siteid==sitesS$siteid[i]],
		depth=sTmin$depth[sTmin$siteid==sitesS$siteid[i]],
				wyear=as.numeric(sTmin$wyear[sTmin$siteid==sitesS$siteid[i]]))		

	DFtest6[[i]]<-join(DFtest5[[i]],DFP4[[i]], by=c("wyear", "depth"), type="left")
	DFtest7[[i]]<-join(DFtest6[[i]],DFP5[[i]], by=c("wyear", "depth"), type="left")				
	DFtest7[[i]]$Toff<-(DFtest7[[i]]$x-DFtest7[[i]]$MS)
	DFtest7[[i]]$Tyr<-floor(DFtest7[[i]]$x)
	#Tave1,Tave2,Tmin,Tmax,Tyear,Tstart
	Ysine1[[i]]<-Tsine1(DFtest7[[i]]$MT,DFtest7[[i]]$MMI,DFtest7[[i]]$x,DFtest7[[i]]$MS)
	Ysine2[[i]]<-Tsine2(DFtest7[[i]]$MMI,DFtest7[[i]]$MMA,DFtest7[[i]]$x,DFtest7[[i]]$MS)
	Ysine3[[i]]<-Tsine3(DFtest7[[i]]$MT2,DFtest7[[i]]$MMA,DFtest7[[i]]$x,DFtest7[[i]]$MS)
	
	#now pull out the temperature difference for each site
	
	
	#now look at zero mixing function
	Ztest1[[i]]<-data.frame(AZ=sAzero$M[sAzero$siteid==sitesS$siteid[i]],
		depth=sAzero$depth[sAzero$siteid==sitesS$siteid[i]],
				wyear=as.numeric(sAzero$wyear[sAzero$siteid==sitesS$siteid[i]]))
	Ztest2[[i]]<-data.frame(BZ=sBzero$M[sBzero$siteid==sitesS$siteid[i]],
		depth=sBzero$depth[sBzero$siteid==sitesS$siteid[i]],
				wyear=as.numeric(sBzero$wyear[sBzero$siteid==sitesS$siteid[i]]))
	
	ZP1[[i]]<-join(Ztest1[[i]],Ztest2[[i]], by=c("wyear", "depth"), type="left")
	
	DDx[[i]]<-datSM[datSM$siteid==sitesS$siteid[i],]
	
	DDx1[[i]]<-join(DFP1[[i]],DDx[[i]], by=c("wyear", "depth"), type="right")
	DDx2[[i]]<-join(DFP2[[i]],DDx1[[i]], by=c("wyear", "depth"), type="right")
	DDx3[[i]]<-join(DFP3[[i]],DDx2[[i]], by=c( "depth"), type="right")
	DDx4[[i]]<-join(DFP4[[i]],DDx3[[i]], by=c("wyear", "depth"), type="right")
	DDx5[[i]]<-join(DFP5[[i]],DDx4[[i]], by=c("wyear", "depth"), type="right")
	DDx6[[i]]<-join(ZP1[[i]],DDx5[[i]], by=c("wyear", "depth"), type="right")
	
	#DDsine1[[i]]<-ifelse(is.na(Tsine1(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS)),0,Tsine1(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS))
	#DDsine2[[i]]<-ifelse(is.na(Tsine2(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS)),0,Tsine2(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS))
	#DDsine3[[i]]<-ifelse(is.na(Tsine3(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS)),0,Tsine3(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS))
	#DDsineC[[i]]<-DDsine1[[i]]+DDsine2[[i]]+DDsine3[[i]]
	

	DDmix[[i]]<-zeroF(DDx6[[i]]$AZ,DDx6[[i]]$BZ,DDx6[[i]]$datTdev)
	DDfilter[[i]]<-ifelse(DDmix[[i]]==Inf|DDx6[[i]]$datTdev==-999,0, DDmix[[i]])
	DDfilter2[[i]]<-ifelse(is.na(DDfilter[[i]]),0,DDfilter[[i]])

		DDsine1[[i]]<-Tsine1(DDx6[[i]]$MT,DDx6[[i]]$MMI,DDx6[[i]]$decdateA,DDx6[[i]]$MS)
		DDsine2[[i]]<-Tsine2(DDx6[[i]]$MMI,DDx6[[i]]$MMA,DDx6[[i]]$decdateA,DDx6[[i]]$MS)
		DDsine3[[i]]<-Tsine3(DDx6[[i]]$MT2,DDx6[[i]]$MMA,DDx6[[i]]$decdateA,DDx6[[i]]$MS)
}

DDFilterDF<-ldply(DDfilter2,data.frame)
colnames(DDFilterDF)<-"Prob"

Xest<-numeric()
for(i in 1:dim(DDFilterDF)[1]){
	Xest[i]<-rbinom(1,1,DDFilterDF$Prob[i])

}
		
#now combine back together
Xlist<-list()
ZeroMix1<-list()
ZeroMix2<-list()
ZeroMix3<-list()
for(i in 1:dim(sitesS)[1]){
	Xlist[[i]]<-Xest[datSM$siteid==sitesS$siteid[i]]
	ZeroMix1[[i]]<-(Xlist[[i]]*zeroMean)+((1-Xlist[[i]])*DDsine1[[i]])
	ZeroMix2[[i]]<-(Xlist[[i]]*zeroMean)+((1-Xlist[[i]])*DDsine2[[i]])
	ZeroMix3[[i]]<-(Xlist[[i]]*zeroMean)+((1-Xlist[[i]])*DDsine3[[i]])
}
				


		



#plot the soil
#create predicted points

test<-list()
for(n in 1:dim(sitesS)[1]){
	i<-sitesS$siteid[n]
test[[n]]<-datSS$M[datSS$siteid==i&datSS$depth==depthP[[n]][j]&datSS$wyear==xPwyear[[n]]&datSS$param=="T.aveS"]

}


for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod6\\plots\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]),max(datSM$decdateA[datSM$siteid==i])),
								ylim=c(min(na.omit(datSM$T[datSM$siteid==i])),max(na.omit(datSM$T[datSM$siteid==i]))),
								xlab="Water Year since 1991", ylab="Temperature", cex.axis=2, cex.lab=3)
		
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col=colP[j])
		points(xP[[n]],Ysine1[[n]][DFtest7[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
		points(xP[[n]],Ysine2[[n]][DFtest7[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
				
		points(xP[[n]],Ysine3[[n]][DFtest7[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
	}

	legend(min(na.omit(datSM$decdateA[datSM$siteid==i]))+.001,max(na.omit(datSM$T[datSM$siteid==i]))-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=3)
			
	text(min(datSM$decdateA[datSM$siteid==i])+2,max(datSM$T[datSM$siteid==i])-2, paste("siteid=", sitesS$siteid[n]), cex=3)
	dev.off()
}

################now make plots to see what mixing function looks like
Tdiff<-seq(0,45, by=.1)


for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod6\\plots\\tdev\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(0,45),
								ylim=c(0,1),
								xlab="Temperature Deviance from zero", ylab="Probability", cex.axis=2, cex.lab=3)
								
	for(j in 1:length(ZP1[[n]]$AZ)){
			points(Tdiff,zeroF(ZP1[[n]]$AZ[j],
								ZP1[[n]]$BZ[j],Tdiff),
				col=colP[j],type="l",lwd=2,lty=1)
			
		}
	

	legend(25,1, paste("depth=", ZP1[[n]]$depth, "year=",ZP1[[n]]$wyear),
			col=colP[1:length(ZP1[[n]]$AZ)],pch=19, bty="n", cex=2)
			
	text(20,.95, paste("siteid=", sitesS$siteid[n]), cex=3)
	dev.off()
}


#now check the temp predictions

#read in ID
datSRC<-read.csv("SoilrepID.csv")
#pull out t values
datTR<-datSM$T[datSRC$x]


plot(datTR, datRR$M, pch=19, ylim=c(-40,25), xlim=c(-40,25))
fitS<-lm( datRR$M~datTR)
summary(fitS)
abline(fitS, lty=3, lwd=2)
abline(0,1,col="red", lwd=2)




#########################################################################
##########try plotting both

for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod6\\plots\\mixing\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]),max(datSM$decdateA[datSM$siteid==i])),
								ylim=c(min(na.omit(datSM$T[datSM$siteid==i])),max(na.omit(datSM$T[datSM$siteid==i]))),
								xlab="Water Year since 1991", ylab="Temperature", cex.axis=2, cex.lab=3)
		
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col=colP[j])
		points(DDx6[[n]]$decdateA[DDx6[[n]]$depth==depthP[[n]][j]],ZeroMix1[[n]][DDx6[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
		points(DDx6[[n]]$decdateA[DDx6[[n]]$depth==depthP[[n]][j]],ZeroMix2[[n]][DDx6[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
			points(DDx6[[n]]$decdateA[DDx6[[n]]$depth==depthP[[n]][j]],ZeroMix3[[n]][DDx6[[n]]$depth==depthP[[n]][j]],
				col=colP[j],type="l",lwd=2,lty=1)		


	}

	legend(min(na.omit(datSM$decdateA[datSM$siteid==i]))+.001,max(na.omit(datSM$T[datSM$siteid==i]))-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=3)
			
	text(min(datSM$decdateA[datSM$siteid==i])+2,max(datSM$T[datSM$siteid==i])-2, paste("siteid=", sitesS$siteid[n]), cex=3)
	dev.off()
}



	par(mai=c(2,2,2,2))
	plot(c(0,1),c(0,1),type="n",xlim=c(min(na.omit(datSM$decdateA[datSM$siteid==2])),max(na.omit(datSM$decdateA[datSM$siteid==2]))),
								ylim=c(min(na.omit(datSM$T[datSM$siteid==2])),max(na.omit(datSM$T[datSM$siteid==2]))),
								xlab="Water Year since 1991", ylab="Temperature", cex.axis=2, cex.lab=3)
		
		
	for(j in 1:length(depthP[[2]])){
		points(datSM$decdateA[datSM$siteid==2&datSM$depth==depthP[[1]][j]],
				datSM$T[datSM$siteid==2&datSM$depth==depthP[[1]][j]],
				pch=19, col=colP[j])
		}
	for(j in 1:length(depthP[[2]])){		
				
			points(DDx6[[1]]$decdateA[DDx6[[1]]$depth==depthP[[1]][j]],ZeroMix1[[1]][DDx6[[1]]$depth==depthP[[1]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
		points(DDx6[[1]]$decdateA[DDx6[[1]]$depth==depthP[[1]][j]],ZeroMix2[[1]][DDx6[[1]]$depth==depthP[[1]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
			points(DDx6[[1]]$decdateA[DDx6[[1]]$depth==depthP[[1]][j]],ZeroMix3[[1]][DDx6[[1]]$depth==depthP[[1]][j]],
				col=colP[j],type="l",lwd=2,lty=1)
	}

	legend(min(na.omit(datSM$decdateA[datSM$siteid==i]))+.001,max(na.omit(datSM$T[datSM$siteid==i]))-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=3)
			
	text(min(datSM$decdateA[datSM$siteid==i])+2,max(datSM$T[datSM$siteid==i])-2, paste("siteid=", sitesS$siteid[n]), cex=3)

}