library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

datAI<-read.csv("AirIDS.csv")
datSI<-read.csv("SoilIDS.csv")
datM<-read.csv("Temp_mod_stats.csv")
datQ<-read.csv("Temp_mod_quant.csv")


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
pnames<-rownames(datC[1:936,])
parms2<-gsub(dexps2,"",pnames)
datC$parms2<-c(as.numeric(parms2),NA,NA)

datC<-data.frame(M=datC[,1],pc2.5=datC[,5],pc97.5=datC[,9],param=as.character(datC[,10]),ID=datC[,11])
datC<-datC[1:936,]
#now combine back with siteid info

#set up ids for all parm
IDSJ<-data.frame(depth=c(datAI$depth,datSI$depth,datAI$depth,datSI$depth),siteid=c(datAI$siteid,datSI$siteid,datAI$siteid,datSI$siteid),
				ID=c(datAI$siteD,datSI$siteD,datAI$siteD,datSI$siteD),
				param=c(rep("AmpA",dim(datAI)[1]),rep("AmpS",dim(datSI)[1]),rep("T.aveA",dim(datAI)[1]),rep("T.aveS",dim(datSI)[1])))
				
#now join to the table 
datS<-join(datC,IDSJ,by=c("param","ID"),type="left")


#now make plots by site to see how this compares
#set up sine function
Tsine<-function(Tave,Amp,Tyear){

		Tave+ Amp*sin(-2*3.14159265*Tyear)
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
datSA<-datS[datS$param=="AmpA"|datS$param=="T.aveA",]
datSS<-datS[datS$param=="AmpS"|datS$param=="T.aveS",]	
#plot the soil
for(n in 1:dim(sitesS)[1]){	
	i<-sitesS$siteid[n]
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\T_mod_out\\site",i,".jpg"),
			width=1500,height=1000, units="px")

	plot(c(0,1),c(0,1),type="n",xlim=c(min(datSM$decdateA[datSM$siteid==i]),max(datSM$decdateA[datSM$siteid==i])),
								ylim=c(min(datSM$T[datSM$siteid==i]),max(datSM$T[datSM$siteid==i])),
								xlab="Time", ylab="Temperature")
		
		
	for(j in 1:length(depthP[[n]])){
		points(datSM$decdateA[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				datSM$T[datSM$siteid==i&datSM$depth==depthP[[n]][j]],
				pch=19, col=colP[j])
		points(xP[[n]],Tsine(datSS$M[datSS$siteid==i&datSS$depth==depthP[[n]][j]&datSS$param=="T.aveS"],
						datSS$M[datSS$siteid==i&datSS$depth==depthP[[n]][j]&datSS$param=="AmpS"],xP[[n]]),
				col=colP[j],type="l",lwd=2,lty=1)
	}

	legend(min(datSM$decdateA[datSM$siteid==i])+.001,max(datSM$T[datSM$siteid==i])-.25, paste("depth=", depthP[[n]]),
			col=colP[1:length(depthP[[n]])],pch=19, bty="n", cex=1.5)
			
	text(min(datSM$decdateA[datSM$siteid==i])+.5,max(datSM$T[datSM$siteid==i])-.5, paste("siteid=", sitesS$siteid[i]), cex=2)
	dev.off()
}