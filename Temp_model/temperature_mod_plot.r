library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")
datM<-read.csv("Temp_moddpt_stats.csv")
datQ<-read.csv("Temp_moddpt_quant.csv")


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
#now add id number
dexps2<-"\\D"

datC<-datC[datC$parms1!="sig.muA"|datC$parms1!="sig.muS",]
pnames<-rownames(datC)
parms2<-gsub(dexps2,"",pnames)
datC$parms2<-c(as.numeric(parms2))

datC<-data.frame(M=datC[,1],pc2.5=datC[,5],pc97.5=datC[,9],param=as.character(datC[,10]),ID=datC[,11])

colnames(datAI)<-c("siteid", "ID")

datC2<-join(datC,datAI, by="ID", type="left")

#now make plots by site to see how this compares
#set up sine function
Tsine<-function(Tave,Amp,Tyear){

		Tave+ Amp*sin(-2*3.14159265*Tyear)
	}

Tsine2<-function(Tave,Amp,Tyear,startD,depthF,b){

		Tave+ (Amp*exp(-b*depthF))*sin(-2*3.14159265*(Tyear-startD)+(b*depthF))
	}

#now need to set up plotting for each site
#get unique site list from soil params

#get unique depth for each site

depthdF<-unique(data.frame(depth=datSM$depth,depthF=datSM$depthF, siteM=datSM$siteM,siteid=datSM$siteid))

depthP<-list()
depthPF<-list()
#set up depths and colors for depths in each site
colP<-c(terrain.colors(7),heat.colors(10),topo.colors(10))
for(i in 1:dim(datSI)[1]){
	depthP[[i]]<-depthdF$depth[depthdF$siteid==datSI$siteid[i]]
	depthPF[[i]]<-depthdF$depthF[depthdF$siteid==datSI$siteid[i]]
}
#set up list of x variables for sine function
xP<-list()
for(i in 1:dim(datSI)[1]){
	xP[[i]]<-seq(floor(min(datSM$decdateA[datSM$siteid==datSI$siteid[i]])),
				ceiling(max(datSM$decdateA[datSM$siteid==datSI$siteid[i]])),
				by=.01)
}
		
#need to join siteID to the datC


datSS<-datC2[datC2$param=="AmpS"|datC2$param=="T.aveS"|datC2$param=="b"|datC2$param=="startS",]	
#now joint so that s
colnames(datSS)[5]<-"siteM"
datSS<-join(datSS,datSI,by="siteM",type="left")
#plot the soil
for(n in 1:dim(datSI)[1]){	
	i<-datSI$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\T_modpt_out\\site",i,".jpg"),
			width=1500,height=1000, units="px")

	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]),max(datSM$decdateA[datSM$siteid==i])),
								ylim=c(min(datSM$T[datSM$siteid==i]),max(datSM$T[datSM$siteid==i])),
								xlab="Time", ylab="Temperature")
		
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col=colP[j])
		points(xP[[n]],Tsine2(datSS$M[datSS$siteid==i&datSS$param=="T.aveS"],
						datSS$M[datSS$siteid==i&datSS$param=="AmpS"],xP[[n]],
						datSS$M[datSS$siteid==i&datSS$param=="startS"],
					depthPF[[n]][j],datSS$M[datSS$siteid==i&datSS$param=="b"]),
				col=colP[j],type="l",lwd=2,lty=1)
	}

	legend(min(datSM$decdateA[datSM$siteid==i])+.001,max(datSM$T[datSM$siteid==i])-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=1.5)
			
	text(min(datSM$decdateA[datSM$siteid==i])+.5,max(datSM$T[datSM$siteid==i])-.5, paste("siteid=", i), cex=2)
	dev.off()
}
		
