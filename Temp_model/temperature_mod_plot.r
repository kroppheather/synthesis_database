library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod2\\output_u7m2")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")
datM<-read.csv("Temp_mod7_stats.csv")
datQ<-read.csv("Temp_mod7_quant.csv")

datAIS<-read.csv("AirIDS_SD.csv")
datSIS<-read.csv("SoilIDS_SD.csv")

datAM<-read.csv("Tair_model.csv")
datSM<-read.csv("Tsoil_model.csv")

datSM$decdateA<-datSM$decdate-1991
datAM$decdateA<-datAM$decdate-1991

#read in temperature data used in model

#now join means with quantiles
datC<-cbind(datM,datQ)
#make a param vector
dexps<-"\\[*[[:digit:]]*\\]"
datC$parms1<-gsub(dexps,"",rownames(datC))

#subset first to only look at soil parms
datCS<-datC[datC$parms1=="T.aveS1"|datC$parms1=="T.aveS2"|datC$parms1=="TmaxS"|datC$parms1=="TminS"|datC$parms1=="startS",]

#now add id number
dexps2<-"\\D"
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
colnames(sTa1)[5]<-"SDWS"
colnames(sTa2)[5]<-"SDWS"
colnames(sTmin)[5]<-"SDWS"
colnames(sTmax)[5]<-"SDWS"
colnames(sTstart)[5]<-"SDS"


#now join with IDS
sTa1<-join(sTa1, datSI, by="SDWS", type="left")
sTa2<-join(sTa2, datSI, by="SDWS", type="left")
sTmin<-join(sTmin, datSI, by="SDWS", type="left")
sTmax<-join(sTmax, datSI, by="SDWS", type="left")
sTstart<-join(sTstart, datSIS, by="SDS", type="left")

#now make plots by site to see how this compares
#set up sine function
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
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod2\\plots\\site",i,".jpg"),
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

######################################################################################################################
##########below is old code. Need to fix for new index


#now look at how the amplitude compares for sites with the more than three depths

#depth count
depthn<-aggregate(datSS$M[datSS$param=="AmpS"],by=list(datSS$siteid[datSS$param=="AmpS"]), FUN="length")
dim(depthn[depthn$x>2,])
dim(depthn[depthn$x>1,])

#subset data to plot by 
depthpn<-depthn[depthn$x>1,]

#now subset datSS Amplitude by sites to use

ssA<-datSS[datSS$param=="AmpS",]
colnames(depthpn)<-c("siteid", "number")
ssA<-join(depthpn,ssA, by="siteid", type="inner")
#get unique site list
site.ssA<-data.frame(siteid=unique(ssA$siteid))
site.ssA$ind<-seq(1,dim(site.ssA)[1])

#make plots comparing amplitude across depth for all sites

for(n in 1:dim(site.ssA)[1]){
	i<-site.ssA$siteid[n]
	#save plot to file
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod1\\output_u7\\plots\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	#make blank plot
	plot(c(0,1),c(0,1), type="n", xlim=c(min(ssA$pc2.5[ssA$siteid==i])-1,max(ssA$pc97.5[ssA$siteid==i])+1),
			ylim=c(max(ssA$depth[ssA$siteid==i])+5,min(ssA$depth[ssA$siteid==i]-5)), xaxs="i",yaxs="i",
			xlab="Amplitude",ylab="Depth")
	#add amplitude and error
	points(ssA$M[ssA$siteid==i], ssA$depth[ssA$siteid==i], pch=19, cex=2, col="deepskyblue3")
	arrows(ssA$pc2.5[ssA$siteid==i],ssA$depth[ssA$siteid==i],ssA$pc97.5[ssA$siteid==i],ssA$depth[ssA$siteid==i],code=0)
	#add site lable
	text(min(ssA$pc2.5[ssA$siteid==i])-.1,min(ssA$depth[ssA$siteid==i])-4,paste("siteid=", i), cex=2)
	dev.off()
}
ssS<-datSS[datSS$param=="startS",]
colnames(depthpn)<-c("siteid", "depthn")
ssS<-join(depthpn,ssS, by="siteid", type="inner")
#get unique site list
site.ssS<-data.frame(siteid=unique(ssS$siteid))
site.ssS$ind<-seq(1,dim(site.ssS)[1])

#make plots comparing startS across depth for all sites
	fit<-lm(ssS$M[ssS$siteid==1]~ssS$depth[ssS$siteid==1])
	abline(fit)
	fit$coefficients[1]
	summary(fit)$r.squared
for(n in 1:dim(site.ssS)[1]){
	i<-site.ssS$siteid[n]
	#save plot to file
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\T_mod2_out\\startdep\\site",i,".jpg"),
			width=1500,height=1000, units="px")
	#make blank plot
	plot(c(0,1),c(0,1), type="n", ylim=c(min(ssS$pc2.5[ssS$siteid==i])-.2,max(ssS$pc97.5[ssS$siteid==i])+.2),
			xlim=c(min(ssS$depth[ssS$siteid==i]-1),max(ssS$depth[ssS$siteid==i])+1), xaxs="i",yaxs="i",
			xlab="Depth",ylab="Start")
	#add amplitude and error
	points( ssS$depth[ssS$siteid==i],ssS$M[ssS$siteid==i], pch=19, cex=2, col="deepskyblue3")
	arrows(ssS$depth[ssS$siteid==i],ssS$pc2.5[ssS$siteid==i],ssS$depth[ssS$siteid==i],ssS$pc97.5[ssS$siteid==i],code=0)
	#add site lable
	text(min(ssS$depth[ssS$siteid==i])+4,max(ssS$pc97.5[ssS$siteid==i]),paste("siteid=", i), cex=2)
	fit<-lm(ssS$M[ssS$siteid==i]~ssS$depth[ssS$siteid==i])
	abline(fit)
	text(min(ssS$depth[ssS$siteid==i])+10,max(ssS$pc97.5[ssS$siteid==i])+.1, paste("rsquared =", round(summary(fit)$r.squared,2)), cex=1.5)
	text(min(ssS$depth[ssS$siteid==i])+15,max(ssS$pc97.5[ssS$siteid==i])+.17, paste("start =", round(fit$coefficients[1],5),"+", round(fit$coefficients[2],5),"*depth"), cex=1.5)
	
	dev.off()
}



#only look at air vs soil
datSA<-datS[datS$param=="AmpA"|datS$param=="T.aveA"|datS$param=="startA",]
datSS<-datS[datS$param=="AmpS"|datS$param=="T.aveS"|datS$param=="startS",]	
#plot the soil
for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\T_mod2_out\\site",i,".jpg"),
			width=1500,height=1000, units="px")

	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]),max(datSM$decdateA[datSM$siteid==i])),
								ylim=c(min(datSM$T[datSM$siteid==i]),max(datSM$T[datSM$siteid==i])),
								xlab="Time", ylab="Temperature")
		
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col=colP[j])
		points(xP[[n]],Tsine(datSS$M[datSS$siteid==i&datSS$depth==depthP[[n]][j]&datSS$param=="T.aveS"],
						datSS$M[datSS$siteid==i&datSS$depth==depthP[[n]][j]&datSS$param=="AmpS"],xP[[n]],
						datSS$M[datSS$siteid==i&datSS$depth==depthP[[n]][j]&datSS$param=="startS"]),
				col=colP[j],type="l",lwd=2,lty=1)
	}

	legend(min(datSM$decdateA[datSM$siteid==i])+.001,max(datSM$T[datSM$siteid==i])-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=1.5)
			
	text(min(datSM$decdateA[datSM$siteid==i])+.5,max(datSM$T[datSM$siteid==i])-.5, paste("siteid=", sitesS$siteid[i]), cex=2)
	dev.off()
}


#read in some of the site and vegetation data

#site vegetation class
datVClass<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\vege_class.csv")
#site mapped soil C
datSoilC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\soilc_30cm.csv")


#match up soil and vege measurements
colnames(datVClass)<-c("siteid","class")
colnames(datSoilC)<-c("siteid", "soilC30")
datSS1<-join(datSS,datVClass,by="siteid", type="left" )
datSS2<-join(datSS1,datSoilC,by="siteid", type="left" )
#set up nls function to fit for each soil
#subset so that it is only b
datSSb<-datSS2[datSS2$param=="AmpS",]
AmpF<-function(A0,b,depth){
	A0*exp(-b*depth)
}

#make an amplitude list
AmpList<-list()
AmpDF<-list()
for(i in 1:dim(sitesS)[1]){
	AmpList[[i]]<-datSSb$M[datSSb$siteid==sitesS$siteid[i]]
	AmpDF[[i]]<-data.frame(Amp=AmpList[[i]],depth=depthP[[i]])
}

N.A<-list()
	A0est<-rep(NA,dim(sitesS)[1])
	A0sig<-rep(NA,dim(sitesS)[1])
	best<-rep(NA,dim(sitesS)[1])
	bsig<-rep(NA,dim(sitesS)[1])

test<-nls.control(maxiter=50000)
for(n in 1:dim(sitesS)[1]){	
	i=sitesS$siteid[n]
	if(dim(AmpDF[[n]])[1]>3){
	
	N.A[[n]]<-nls(Amp~AmpF(A0,b,depth),control=test, data=AmpDF[[n]], start=list(A0=15,b=.007))
	A0est[n]<-summary(N.A[[n]])$coefficients[1,1]
	A0sig[n]<-summary(N.A[[n]])$coefficients[1,4]
	best[n]<-summary(N.A[[n]])$coefficients[2,1]
	bsig[n]<-summary(N.A[[n]])$coefficients[2,4]
	}else{N.A[[n]]<-"null"}
}


#make a dataframe
resultsB<-data.frame(A0=A0est,P.A0=A0sig,B=best,P.B=bsig, siteid=sitesS$siteid)

plot(resultsB$siteid,resultsB$B)

#join start with B

fitS<-list()	
for(n in 1:dim(sitesS)[1]){
	i=sitesS$siteid[n]

	fitS[[i]]<-lm(ssS$M[ssS$siteid==i]~ssS$depth[ssS$siteid==i])

}

#now join with vegeclass
#1	Herb barren
#2	gramminoid tundra
#3	tussock tundra
#4	shrub tundra
#5	sedge/gramminoid wetland	
#6	needle leaf deciduous boreal
#7	evergreen conifer borea
#8	mixed conifer/deciduoud boreal

Bresult1<-join(resultsB,datVClass, by="siteid", type="left")
par(mfrow=c(1,2))
plot(Bresult1$B~as.factor(Bresult1$class))
plot(Bresult1$A0~as.factor(Bresult1$class))
#now join with datC

Bresult2<-join(resultsB,datSoilC, by="siteid", type="left")
plot(Bresult2$soilC30,Bresult2$B,pch=19)

plot(Bresult1$B~Bresult1$class)
#see if b matches up with bd
datBD<-read.csv("mapped_BD.csv")

Bresult3<-join(resultsB,datBD, by="siteid", type="left")
plot(Bresult3$BD,Bresult3$B,pch=19)

datTC<-read.csv("soil_therm.csv")

Bresult4<-join(resultsB,datTC, by="siteid", type="left")
plot(Bresult4$TC,Bresult3$B,pch=19)

datSF<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\siteinfo.csv")
colnames(datSF)[1]<-"siteid"
Bresult5<-join(resultsB,datSF, by="siteid", type="left")
Bresult5$vegezfix<-ifelse(Bresult5$vege_z=="boreal","boreal",
				ifelse(Bresult5$vege_z=="tundra", "tundra", 
					ifelse(Bresult5$vege_z=="boreal forest", "boreal",
						ifelse(is.na(Bresult5$vege_z),"tundra",NA))))
					
plot(as.factor(Bresult5$vegezfix),Bresult5$B)

#plot b vs start

plot(datSS$M[datSS$param=="AmpS"], datSS$M[datSS$param=="startS"])

plot( datSS$M[datSS$param=="startS"],datSS$M[datSS$param=="AmpS"])

plot( datSS$depth[datSS$param=="AmpS"],datSS$M[datSS$param=="AmpS"], xlim=c(0,300), pch=19)

for(i in 1:dim(sitesS)[1]){
	if(is.na(resultsB$A0[i])==FALSE){
		points(seq(0,300, by=.1), AmpF(resultsB$A0[i],resultsB$B[i],seq(0,300, by=.1)), type="l", col="red")
	
	}


}
