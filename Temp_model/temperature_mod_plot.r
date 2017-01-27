library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")
datM<-read.csv("Temp_mod2_stats.csv")
datQ<-read.csv("Temp_mod2_quant.csv")


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
datC<-rbind(datC[1:932,],datC[935:1400,])

#now add id number
dexps2<-"\\D"
pnames<-rownames(datC)
parms2<-gsub(dexps2,"",pnames)
datC$parms2<-c(as.numeric(parms2))

datC<-data.frame(M=datC[,1],pc2.5=datC[,5],pc97.5=datC[,9],param=as.character(datC[,10]),ID=datC[,11])

#now combine back with siteid info

#set up ids for all parm
IDSJ<-data.frame(depth=c(datAI$depth,datSI$depth,datAI$depth,datSI$depth,datAI$depth,datSI$depth),
				siteid=c(datAI$siteid,datSI$siteid,datAI$siteid,datSI$siteid,datAI$siteid,datSI$siteid),
				ID=c(datAI$siteD,datSI$siteD,datAI$siteD,datSI$siteD,datAI$siteD,datSI$siteD),
				param=c(rep("AmpA",dim(datAI)[1]),rep("AmpS",dim(datSI)[1]),rep("T.aveA",dim(datAI)[1]),rep("T.aveS",dim(datSI)[1]),
				rep("startA",dim(datAI)[1]),rep("startS",dim(datSI)[1])))
				
#now join to the table 
datS<-join(datC,IDSJ,by=c("param","ID"),type="left")


#now make plots by site to see how this compares
#set up sine function
Tsine<-function(Tave,Amp,Tyear,Tstart){

		Tave+ Amp*sin(-2*3.14159265*(Tyear-Tstart))
	}

#now need to set up plotting for each site
#get unique site list from soil params
sitesS<-data.frame(siteid=unique(datSI$siteid))
sitesS$siteun<-seq(1,dim(sitesS)[1])
depthP<-list()
#set up depths and colors for depths in each site
colP<-c(terrain.colors(7),heat.colors(10),topo.colors(10))
for(i in 1:dim(sitesS)[1]){
	depthP[[i]]<-datSI$depth[datSI$siteid==sitesS$siteid[i]]

}
#set up list of x variables for sine function
xP<-list()
for(i in 1:dim(sitesS)[1]){
	xP[[i]]<-seq(floor(min(datSM$decdateA[datSM$siteid==sitesS$siteid[i]])),
				ceiling(max(datSM$decdateA[datSM$siteid==sitesS$siteid[i]])),
				by=.01)
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


#now look at how the amplitude compares for sites with the more than three depths

#depth count
depthn<-aggregate(datSS$M[datSS$param=="AmpS"],by=list(datSS$siteid[datSS$param=="AmpS"]), FUN="length")
dim(depthn[depthn$x>2,])
dim(depthn[depthn$x>1,])

#subset data to plot by 
depthpn<-depthn[depthn$x>1,]

#now subset datSS Amplitude by sites to use

ssA<-datSS[datSS$param=="AmpS",]

ssA<-join(depthpn,ssA, by="siteid", type="inner")
#get unique site list
site.ssA<-data.frame(siteid=unique(ssA$siteid))
site.ssA$ind<-seq(1,dim(site.ssA)[1])

#make plots comparing amplitude across depth for all sites

for(n in 1:dim(site.ssA)[1]){
	i<-site.ssA$siteid[n]
	#save plot to file
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\T_mod2_out\\depAmp\\site",i,".jpg"),
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

