##########################################################
########Deep soil measurement sites            ###########
########Heather Kropp started September 2017   ###########
##########################################################
##########################################################
### This script uses vegetation classes and biomes     ###
###  to look at patterns in deep soil                  ###
###  temperature and coupling with air                 ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
##########################################################
##########################################################

#libraries loaded in source script: plyr
library(wesanderson)

##########################################
##first grab soil temperature parameters##
##########################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\deep_soil_parms_extract.r")

#read in vegetation class data
#note sites 199-222 not confirmed yet

datVC <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\vege_class\\new_class.csv")

#read in site info

datSite <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\siteinfo.csv",
						na.strings=c("NaN", "NA"))
						

#make sub table
newC <- data.frame(siteid=datVC$siteid, Vclass=datVC$new.class)

#make a subset site info table
Sitesub <- data.frame(siteid=datSite$site_id, biome=datSite$vege_z)

newC <- join(newC, Sitesub, by="siteid", type="left")

#join vegetation class to each parameter table
NfactorV <- join(Nfactor, newC, by="siteid", type="left")
SoilParmV <- join(SoilParm, newC, by="siteid", type="left")

#check to see how many observations in each vege class
ParmsCountVC <- aggregate(SoilParmV$Mean[SoilParmV$parm=="TmaxS"],
						by=list(SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]), FUN="length")
#see how many sites are unique
SiteCountLVC <-unique(data.frame(siteid=SoilParmV$siteid[SoilParmV$parm=="TmaxS"],
								Vclass=SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]))
#count how many sites in each class
SiteCountVC <- aggregate(SiteCountLVC$siteid, by=list(SiteCountLVC$Vclass), FUN="length")

#check the biome
#check to see how many observations in each vege class
ParmsCountB <- aggregate(SoilParmV$Mean[SoilParmV$parm=="TmaxS"],
						by=list(SoilParmV$biome[SoilParmV$parm=="TmaxS"]), FUN="length")
#see how many sites are unique
SiteCountLB <-unique(data.frame(siteid=SoilParmV$siteid[SoilParmV$parm=="TmaxS"],
								Vclass=SoilParmV$biome[SoilParmV$parm=="TmaxS"]))
#count how many sites in each class
SiteCountB <- aggregate(SiteCountLB$siteid, by=list(SiteCountLB$Vclass), FUN="length")

#make seperate data frames fore each soil parameter

datNF<-NfactorV [NfactorV $parm=="Fn",]
datNT<-NfactorV [NfactorV $parm=="Tn",]
datTmax<-SoilParmV[SoilParmV$parm=="TmaxS",]
datTmin<-SoilParmV[SoilParmV$parm=="TminS",]
datDZ<-SoilParmV[SoilParmV$parm=="DayZero",]
datPS<-SoilParmV[SoilParmV$parm=="peakSS",]
datPW<-SoilParmV[SoilParmV$parm=="peakWS",]

#now seperate out air to match
datTmaxA<-AirParm[AirParm$parm=="TmaxA",]
colnames(datTmaxA)[1:4]<-paste0(colnames(datTmaxA)[1:4],"A")
datTminA<-AirParm[AirParm$parm=="TminA",]
colnames(datTminA)[1:4]<-paste0(colnames(datTminA)[1:4],"A")
datPSA<-AirParm[AirParm$parm=="peakSA",]
colnames(datPSA)[1:4]<-paste0(colnames(datPSA)[1:4],"A")
datPWA<-AirParm[AirParm$parm=="peakWA",]
colnames(datPWA)[1:4]<-paste0(colnames(datPWA)[1:4],"A")

#now combine air measure with matching parm
datNF<-join(datNF, datTminA, by=c("siteid","height","wyear"), type="left")
datNT<-join(datNT, datTmaxA, by=c("siteid","height","wyear"), type="left")

datTmax<-join(datTmax, datTmaxA, by=c("siteid","wyear"), type="left")
datTmin<-join(datTmin, datTminA, by=c("siteid","wyear"), type="left")

datPS<-join(datPS,datPSA,by=c("siteid","wyear"), type="left")
datPW<-join(datPW,datPWA,by=c("siteid","wyear"), type="left")

datDZ<-join(datDZ,datTminA, by=c("siteid","wyear"), type="left")

datAll<-list(datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ)

#pull out a unique table of depth, site, biome ect

IDsTable <- unique(data.frame(siteid=datNF$siteid, wyear=datNF$wyear, depth=datNF$depth,
						Vclass=datNF$Vclass, biome=datNF$biome))
						
IDsTable$newB <- ifelse(IDsTable$Vclass<=3 |IDsTable$Vclass==6, "gtundra",
					ifelse(IDsTable$Vclass==4 |IDsTable$Vclass==5, "stundra","boreal"))						

#create a color table

colVCT <- unique(data.frame(Vclass=IDsTable$Vclass))
colVCT$CName <- c("mixed boreal", "evergreen boreal", "tall shrub", "gramminoid", "short shrub", "tussock", "wetland", "herb-bare")

colVCT$coli <- c("tomato3","lightsteelblue3","tomato3","lightsteelblue3","lightsteelblue3","tomato3",
					"lightgoldenrod4", "grey30")
IDsTable <- join(IDsTable, colVCT, by="Vclass", type="left")
#get unique site year to plot in each biome

BorealID <- IDsTable[IDsTable$newB=="boreal",]						
	BorealSites<- unique(data.frame(siteid=BorealID$siteid, wyear=BorealID$wyear))					
gTundraID <- IDsTable[IDsTable$newB=="gtundra",]						
gTundraSites<- unique(data.frame(siteid=gTundraID$siteid, wyear=gTundraID$wyear))	
sTundraID <- IDsTable[IDsTable$newB=="stundra",]						
sTundraSites<- unique(data.frame(siteid=sTundraID$siteid, wyear=sTundraID$wyear))	

	
##############
#start by looking at n factors across depth with each biome seperated
wb <- 35
hb <- 50

#get the maximum depth to plot
yu <- 0
yh <- 310
#datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ
xl <- c(0,0,-1,-40,.5,0,0)
xh <- c(1.7,1.7,20,0,1,.6,255)
yla<- -0
yha <- 300
yi <- -50
xla <- c(0,0,0,-35,.5,0,0)
xha <-  c(1.5,1.5,18,-5,.9,.5,200)
xi <- c(.3,.3,3,10,.1,.1,50)
xl1 <-c(.8,.8,10,-15,.7,.3,200)
yl1 <-c(250,250,250,250,250,250,250)
dataType <- c("Freezing n-factor", "Thawing n-factor", "Temperature maximum (C)", "Temperature minimum (C)",
				"Time of temperature maximum", "Time of temperature minimum", "Number of days in zero model")
dataFile<-c("Fn", "Tn", "Tmax", "Tmin", "pmax", "pmin", "Zero")				

for(k in 1:length(dataType)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\deep\\run1\\analysis_plots\\", dataFile[k],".jpg"), 
		, width=4000,height=2000)

ab <- layout (matrix(seq(1,3), ncol=3), width=rep(lcm(wb), 3), height=rep(lcm(hb), 3))
layout.show(ab)

par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), xlim= c(xl[k],xh[k]), ylim= c(yh,yu), xlab = " ", ylab = " ", axes=FALSE,
		xaxs="i", yaxs="i", type="n")
	for(i in 1:dim(BorealID)[1]){
		points(datAll[[k]]$Mean[datAll[[k]]$biome=="boreal"&datAll[[k]]$siteid==BorealID$siteid[i]&datAll[[k]]$wyear==BorealID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="boreal"&datAll[[k]]$siteid==BorealID$siteid[i]&datAll[[k]]$wyear==BorealID$wyear[i]],
		type="b",pch=19,col=BorealID$coli[i],lwd=4, lty=1,cex=6)
		arrows(datAll[[k]]$pc2.5[datAll[[k]]$biome=="boreal"&datAll[[k]]$siteid==BorealID$siteid[i]&datAll[[k]]$wyear==BorealID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="boreal"&datAll[[k]]$siteid==BorealID$siteid[i]&datAll[[k]]$wyear==BorealID$wyear[i]],
		datAll[[k]]$pc97.5[datAll[[k]]$biome=="boreal"&datAll[[k]]$siteid==BorealID$siteid[i]&datAll[[k]]$wyear==BorealID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="boreal"&datAll[[k]]$siteid==BorealID$siteid[i]&datAll[[k]]$wyear==BorealID$wyear[i]], code=0,lwd=4)
	}			
	
	box(which="plot")			
	axis(2, seq(yha, yla, by=yi),rep(" ", length(seq(yha, yla, by=yi))), cex.axis=5, las=2 )
	mtext(paste(seq(yha, yla, by=yi)), at=seq(yha, yla, by=yi), side=2, line=3,cex=5, las=2)
	axis(1, seq(xla[k],xha[k], by=xi[k]),rep(" ", length(seq(xla[k],xha[k], by=xi[k]))), cex.axis=5)
	mtext(paste(round(seq(xla[k],xha[k], by=xi[k]),2)),at=seq(xla[k],xha[k], by=xi[k]),side=1, line=5, cex=5)
	
	legend(xl1[k],yl1[k], c(paste(colVCT$CName[colVCT$Vclass==9]),paste(colVCT$CName[colVCT$Vclass==7])), 
				col=c(colVCT$coli[colVCT$Vclass==9],colVCT$coli[colVCT$Vclass==7]), pch=19, lwd=4, cex=5, bty="n")
	mtext("Boreal", side=3, cex=7, line=3)
	mtext(paste(dataType[k]), side=1, outer=TRUE, line=-8, cex=7)
	mtext("Depth (cm)", side=2, outer=TRUE, cex=7, line=-19)
par(mai=c(0,0,0,0))	
plot(c(0,1), c(0,1), xlim= c(xl[k],xh[k]), ylim= c(yh,yu), xlab = " ", ylab = " ", axes=FALSE,
		xaxs="i", yaxs="i", type="n")
	for(i in 1:dim(sTundraID)[1]){
		points(datAll[[k]]$Mean[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==sTundraID$siteid[i]&datAll[[k]]$wyear==sTundraID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==sTundraID$siteid[i]&datAll[[k]]$wyear==sTundraID$wyear[i]],
		type="b",pch=19,col=sTundraID$coli[i], cex=6, lwd=4, lty=1)
		arrows(datAll[[k]]$pc2.5[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==sTundraID$siteid[i]&datAll[[k]]$wyear==sTundraID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==sTundraID$siteid[i]&datAll[[k]]$wyear==sTundraID$wyear[i]],
		datAll[[k]]$pc97.5[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==sTundraID$siteid[i]&datAll[[k]]$wyear==sTundraID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==sTundraID$siteid[i]&datAll[[k]]$wyear==sTundraID$wyear[i]],code=0, lwd=4)
	
	}	
	box(which="plot")			

	axis(1, seq(xla[k],xha[k], by=xi[k]),rep(" ", length(seq(xla[k],xha[k], by=xi[k]))), cex.axis=5)
	mtext(paste(round(seq(xla[k],xha[k], by=xi[k]),2)),at=seq(xla[k],xha[k], by=xi[k]),side=1, line=5, cex=5)
		legend(xl1[k],yl1[k], c(paste(colVCT$CName[colVCT$Vclass==5]),paste(colVCT$CName[colVCT$Vclass==4])), 
				col=c(colVCT$coli[colVCT$Vclass==5],colVCT$coli[colVCT$Vclass==4]), pch=19, lwd=2, cex=5, bty="n")
	mtext("Shrub tundra", side=3, cex=7, line=3)
par(mai=c(0,0,0,0))	
plot(c(0,1), c(0,1), xlim= c(xl[k],xh[k]), ylim= c(yh,yu), xlab = " ", ylab = " ", axes=FALSE,
		xaxs="i", yaxs="i", type="n")
	for(i in 1:dim(gTundraID)[1]){
		points(datAll[[k]]$Mean[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==gTundraID$siteid[i]&datAll[[k]]$wyear==gTundraID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==gTundraID$siteid[i]&datAll[[k]]$wyear==gTundraID$wyear[i]],
		type="b",pch=19,col=gTundraID$coli[i], cex=6, lwd=4, lty=1)
		arrows(datAll[[k]]$pc2.5[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==gTundraID$siteid[i]&datAll[[k]]$wyear==gTundraID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==gTundraID$siteid[i]&datAll[[k]]$wyear==gTundraID$wyear[i]],
		datAll[[k]]$pc97.5[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==gTundraID$siteid[i]&datAll[[k]]$wyear==gTundraID$wyear[i]],
		datAll[[k]]$depth[datAll[[k]]$biome=="tundra"&datAll[[k]]$siteid==gTundraID$siteid[i]&datAll[[k]]$wyear==gTundraID$wyear[i]],code=0, lwd=4
		)
	}	
	box(which="plot")			
	axis(4, seq(yha, yla, by=yi),rep(" ", length(seq(yha, yla, by=yi))), cex.axis=5, las=2 )
	mtext(paste(seq(yha, yla, by=yi)), at=seq(yha, yla, by=yi), side=4, line=3,cex=5, las=2)
	axis(1, seq(xla[k],xha[k], by=xi[k]),rep(" ", length(seq(xla[k],xha[k], by=xi[k]))), cex.axis=5)
	mtext(paste(round(seq(xla[k],xha[k], by=xi[k]),2)),at=seq(xla[k],xha[k], by=xi[k]),side=1, line=5, cex=5)	
	legend(xl1[k],yl1[k], c(paste(colVCT$CName[colVCT$Vclass==1]),paste(colVCT$CName[colVCT$Vclass==2]),paste(colVCT$CName[colVCT$Vclass==3])), 
				col=c(colVCT$coli[colVCT$Vclass==1],colVCT$coli[colVCT$Vclass==2],colVCT$coli[colVCT$Vclass==3]),
				pch=19, lwd=2, cex=5, bty="n")
	mtext("Gramminoid tundra", side=3, cex=7, line=3)
dev.off()
	}	