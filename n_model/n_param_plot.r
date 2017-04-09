



###############################################################
###############################################################
################look at some specific site patterns to help 
################deal with interpretation
library(plyr)
#read in data
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod1\\output_u7")

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


#now add id number
dexps2<-"\\D"
pnames<-rownames(datC)
parms2<-gsub(dexps2,"",pnames)
datC$parms2<-c(as.numeric(parms2))

datC<-data.frame(M=datC[,1],pc2.5=datC[,5],pc97.5=datC[,9],param=as.character(datC[,10]),ID=datC[,11])

datADDF<-datC[datC$param=="FDDA",]
datADDT<-datC[datC$param=="TDDA",]
datSDDF<-datC[datC$param=="FDDS",]
datSDDT<-datC[datC$param=="TDDS",]
colnames(datADDF)[5]<-"SDWA"
colnames(datADDT)[5]<-"SDWA"
colnames(datSDDF)[5]<-"SDWS"
colnames(datSDDT)[5]<-"SDWS"


datADDF2<-join(datADDF,datAI, by="SDWA", type="left")
datADDT2<-join(datADDT,datAI, by="SDWA", type="left")
datSDDF2<-join(datSDDF,datSI, by="SDWS", type="left")
datSDDT2<-join(datSDDT,datSI, by="SDWS", type="left")

#read in vegetation id info
datVC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\vegeClass.csv")
colnames(datVC)[1]<-"siteid"

datADDF3<-join(datADDF2,datVC, by="siteid", type="left")
datADDT3<-join(datADDT2,datVC, by="siteid", type="left")
datSDDF3<-join(datSDDF2,datVC, by="siteid", type="left")
datSDDT3<-join(datSDDT2,datVC, by="siteid", type="left")

colnames(datSDDF3)<-c("M.S", "pc2.5S", "pc.97.5S", "paramS",
					"SDWS", "siteid", "depth", "wyear","classS")
colnames(datSDDT3)<-c("M.S", "pc2.5S", "pc.97.5S", "paramS",
					"SDWS", "siteid", "depth", "wyear","classS")

					
FreezeJ<-join(datSDDF3, datADDF3, by=c("siteid", "wyear"), type="left")

bareFJ<-FreezeJ[FreezeJ$classS==1,]
par(mai=c(1,1,1,1))
plot(FreezeJ$M.S[FreezeJ$classS==1],FreezeJ$M[FreezeJ$classS==1],
		xlim=c(-5000,-3000), ylim=c(-5000,-3000),pch=19,
		xlab="Freezing degree days soil", 
		ylab="Freezing degree days air", cex.axis=2, cex.lab=2,
		cex=1.5)
		
AmpA<-datC[datC$param=="AmpA",]
AmpS<-datC[datC$param=="AmpS",]
TaA<-datC[datC$param=="T.aveA",]
TaS<-datC[datC$param=="T.aveS",]
colnames(AmpA)<-c("Aa", "Aapc2.5","Aapc97.5","paramAA", "SDWA")
colnames(AmpS)<-c("Sa", "Sapc2.5","Sapc97.5","paramSA", "SDWS")
colnames(TaA)<-c("At", "Atpc2.5","Atpc97.5","paramAT", "SDWA")
colnames(TaS)<-c("st", "Stpc2.5","Stpc97.5","paramST", "SDWS")

bareFJ2<-join(bareFJ, AmpA, by="SDWA", type="left")
bareFJ3<-join(bareFJ2, AmpS, by="SDWS", type="left")	
bareFJ4<-join(bareFJ3, TaS, by="SDWS", type="left")	
bareFJ5<-join(bareFJ4, TaA, by="SDWA", type="left")	

par(mai=c(1,1,1,1))
plot(bareFJ5$Aa,bareFJ5$Sa, pch=19, xlim=c(13,20), ylim=c(13,20),
	xlab="Air Temperature Amplitude (C)", 
	ylab="Soil Temperature Amplitude (C)", cex.axis=2, cex.lab=2,
		cex=1.5 )
		arrows(bareFJ5$Aapc2.5,bareFJ5$Sa,bareFJ5$Aapc97.5,bareFJ5$Sa,
		code=0, lwd=1.5)
		arrows(bareFJ5$Aa,bareFJ5$Sapc2.5,bareFJ5$Aa,bareFJ5$Sapc97.5,
		code=0, lwd=1.5)		
		
	abline(0,1, lty=4, lwd=2)
	
par(mai=c(1,1,1,1))
plot(bareFJ5$At,bareFJ5$st, pch=19, xlim=c(-13,-6), ylim=c(-13,-6),
	xlab="Air Temperature Average (C)", 
	ylab="Soil Temperature Average (C)", cex.axis=2, cex.lab=2,
		cex=1.5 )
		arrows(bareFJ5$Atpc2.5,bareFJ5$st,bareFJ5$Atpc97.5,bareFJ5$st,
		code=0, lwd=1.5)
		arrows(bareFJ5$At,bareFJ5$Stpc2.5,bareFJ5$At,bareFJ5$Stpc97.5,
		code=0, lwd=1.5)		
	abline(0,1, lty=4, lwd=2)	
	
#####################################################################
####look at N factors vs world climate data
####


######################################
#read in data
library(plyr)
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4")

datF<-read.csv("Freezing_n_forMod.csv")
datT<-read.csv("Thawing_n_forMod.csv")
datEA<-read.csv("AVET_forMod.csv")


#read in params
datM<-read.csv("model_variaion_stats.csv")
datQ<-read.csv("model_variaion_quant.csv")
#now join means with quantiles
datC<-cbind(datM,datQ)
#make a param vector
#make a param vector
dexps<-"\\[*[[:digit:]]*\\]"
datC$parms1<-gsub(dexps,"",rownames(datC))

#now add id number
dexps2<-"\\D"
#pull out names
pnames<-rownames(datC)
#need to split because there are numbers in param names
psplit<-strsplit(pnames, "\\[")
#pull out vector number
pEnd<-character(0)
for(i in 1:dim(datC)[1]){
	if(length(psplit[[i]])>1){
		pEnd[i]<-psplit[[i]][2]
	}else{pEnd[i]<-"NA"}

}
#get vector number only and make numeric
parmCN<-ifelse(pEnd=="NA", NA, gsub(dexps2,"", pEnd ))

datC$parms2<-c(as.numeric(parmCN))

datCS<-data.frame(M=datC[,1],pc2.5=datC[,5],pc97.5=datC[,9],param=as.character(datC[,10]),ID=datC[,11])


datCT<-datCS[datCS$param=="betaT3",]
datCF<-datCS[datCS$param=="betaF3",]


wb<-40
hb<-40

covlm<-function(x,b0, xave,b3){
	b0+ (b3*(x-xave))
}

#classID code
#1= herb barren
#2 = grasstundra
#3= tussock tundra
#4= shrub tundra
#5= wetland
#6= evergreen boreal
#7= mixed boreal

#make a plot to look at more closely
jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\n_Fmin.jpg", width=9000,height=2200)	
ab<-layout(matrix(seq(1,7), ncol=7, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),
			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))
			
			
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50,-7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==1],datF$NT[datF$classID==1], 
		pch=19, cex=12, col="deepskyblue4")		
abline(h=datCS$M[datCS$param=="betaF1"][1],
		 col="black", lwd=8)
abline(v=datEA$AveMin[1],
		 col="plum3", lwd=8, lty=3)		
		
text(-45 + 10, 1.6, "Herb barren", cex=10)	
axis(1, seq(-45, -10, by=5), cex.axis=8, padj=1)
axis(2, seq(.2,1.6, by=.2),cex.axis=8,las=2)
mtext("Minimum Yearly Temperature (C)", side=1, cex=7, line=-8,outer =TRUE)
mtext("Freezing n-factor", side=2, cex=7, line=15)
	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50, -7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datF$Amin[datF$classID==2],datF$NT[datF$classID==2], 
		pch=19, cex=12, col="deepskyblue4")		
points(seq(-50,-7,by=.5), 	covlm(seq(-50,-7,by=.5),datCS$M[datCS$param=="betaF1"][2],datEA$AveMin[2],
	datCS$M[datCS$param=="betaF3"][2]),
		type="l", col="black", lwd=8)

text(-45 + 10, 1.6, "Grass tundra", cex=10)	
axis(1, seq(-45, -10, by=5), cex.axis=8, padj=1)
abline(v=datEA$AveMin[2],
		 col="plum3", lwd=8, lty=3)	
		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50, -7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
		
points(datF$Amin[datF$classID==3],datF$NT[datF$classID==3], 
		pch=19, cex=12, col="deepskyblue4")	
abline(h=datCS$M[datCS$param=="betaF1"][3],
		 col="black", lwd=8)
abline(v=datEA$AveMin[3],
		 col="plum3", lwd=8, lty=3)			
	
text(-45 + 10, 1.6, "Tussock tundra", cex=10)	
axis(1, seq(-45, -10, by=5), cex.axis=8, padj=1)	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50, -7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datF$Amin[datF$classID==4],datF$NT[datF$classID==4], 
		pch=19, cex=12, col="deepskyblue4")		
points(seq(-50,-7,by=.5), 	covlm(seq(-50,-7,by=.5),datCS$M[datCS$param=="betaF1"][4],datEA$AveMin[4],
	datCS$M[datCS$param=="betaF3"][4]),
		type="l", col="black", lwd=8)
		
abline(v=datEA$AveMin[4],
		 col="plum3", lwd=8, lty=3)	
text(-45 + 10, 1.6, "Shrub tunda", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)			
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50, -7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==5],datF$NT[datF$classID==5], 
		pch=19, cex=12, col="deepskyblue4")	
abline(h=datCS$M[datCS$param=="betaF1"][5],
		 col="black", lwd=8)
abline(v=datEA$AveMin[5],
		 col="plum3", lwd=8, lty=3)	
		
text(-45 + 10, 1.6, "wetland tund", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))

		
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50, -7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==6],datF$NT[datF$classID==6], 
		pch=19, cex=12, col="deepskyblue4")
abline(h=datCS$M[datCS$param=="betaF1"][6],
		 col="black", lwd=8)
abline(v=datEA$AveMin[6],
		 col="plum3", lwd=8, lty=3)	
		
text(-45 + 10, 1.6, "evergreen boreal", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-50, -7), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==7],datF$NT[datF$classID==7], 
		pch=19, cex=12, col="deepskyblue4")	

abline(h=datCS$M[datCS$param=="betaF1"][7],
		 col="black", lwd=8)
abline(v=datEA$AveMin[7],
		 col="plum3", lwd=8, lty=3)			

text(-45 + 10, 1.6, "mixed boreal", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)			
		
box(which="plot")

dev.off()

########################################################################
########################################################################


#make a plot to look at more closely
jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\n_Tmax.jpg", width=9000,height=2200)	
ab<-layout(matrix(seq(1,7), ncol=7, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),
			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))
			
			
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==1],datT$NT[datT$classID==1], 
		pch=19, cex=12, col="seagreen4")		
points(seq(10,26,by=.5), 	covlm(seq(10,26,by=.5),datCS$M[datCS$param=="betaT1"][1],datEA$AveMax[1],
	datCS$M[datCS$param=="betaT3"][1]),
		type="l", col="black", lwd=8)
abline(v=datEA$AveMax[1],
		 col="plum3", lwd=8, lty=3)		
		
text( 20, 1.6, "Herb barren", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)
axis(2, seq(.2,1.6, by=.2),cex.axis=8,las=2)
mtext("Maximum Yearly Temperature (C)", side=1, cex=7, line=-8,outer =TRUE)
mtext("Thawing n-factor", side=2, cex=7, line=15)
	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datT$Amax[datT$classID==2],datT$NT[datT$classID==2], 
		pch=19, cex=12, col="seagreen4")		
points(seq(10,26,by=.5), 	covlm(seq(10,26,by=.5),datCS$M[datCS$param=="betaT1"][2],datEA$AveMax[2],
	datCS$M[datCS$param=="betaT3"][2]),
		type="l", col="black", lwd=8)

text(20, 1.6, "Grass tundra", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)
abline(v=datEA$AveMax[2],
		 col="plum3", lwd=8, lty=3)	
		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
		
points(datT$Amax[datT$classID==3],datT$NT[datT$classID==3], 
		pch=19, cex=12, col="seagreen4")	

		
points(seq(10,26,by=.5), 	covlm(seq(10,26,by=.5),datCS$M[datCS$param=="betaT1"][3],datEA$AveMax[3],
	datCS$M[datCS$param=="betaT3"][3]),
		type="l", col="black", lwd=8)		
		
abline(v=datEA$AveMax[3],
		 col="plum3", lwd=8, lty=3)			
	
text(20, 1.6, "Tussock tundra", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datT$Amax[datT$classID==4],datT$NT[datT$classID==4], 
		pch=19, cex=12, col="seagreen4")		

points(seq(10,26,by=.5), 	covlm(seq(10,26,by=.5),datCS$M[datCS$param=="betaT1"][4],datEA$AveMax[4],
	datCS$M[datCS$param=="betaT3"][4]),
		type="l", col="black", lwd=8)		
abline(v=datEA$AveMax[4],
		 col="plum3", lwd=8, lty=3)	
text(20, 1.6, "Shrub tunda", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==5],datT$NT[datT$classID==5], 
		pch=19, cex=12, col="seagreen4")	

points(seq(10,26,by=.5), 	covlm(seq(10,26,by=.5),datCS$M[datCS$param=="betaT1"][5],datEA$AveMax[5],
	datCS$M[datCS$param=="betaT3"][5]),
		type="l", col="black", lwd=8)		
		
abline(v=datEA$AveMax[5],
		 col="plum3", lwd=8, lty=3)	
		
text(20, 1.6, "wetland tunda", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))

		
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==6],datT$NT[datT$classID==6], 
		pch=19, cex=12, col="seagreen4")
abline(h=datCS$M[datCS$param=="betaT1"][6],
		 col="black", lwd=8)
abline(v=datEA$AveMax[6],
		 col="plum3", lwd=8, lty=3)	
		
text(20, 1.6, "evergreen boreal", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(10,26), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==7],datT$NT[datT$classID==7], 
		pch=19, cex=12, col="seagreen4")	

abline(h=datCS$M[datCS$param=="betaT1"][7],
		 col="black", lwd=8)
abline(v=datEA$AveMax[7],
		 col="plum3", lwd=8, lty=3)			

text(20, 1.6, "mixed boreal", cex=10)	
axis(1, seq(10,26, by=5), cex.axis=8, padj=1)		
		
box(which="plot")

dev.off()

#####################################################################################
#####################################################################################


#make a plot to look at more closely
jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n3\\n_FTdepth.jpg", width=9000,height=2200)	
ab<-layout(matrix(seq(1,7), ncol=7, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),
			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))
			
			
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$depth[datF$classID==1&datF$orgID==1],datF$NT[datF$classID==1&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$depth[datF$classID==1&datF$orgID==2],datF$NT[datF$classID==1&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Herb barren", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)	
axis(2, seq(0,1.6, by=.2),cex.axis=8,las=2)
mtext("Depth (cm)", side=1, cex=7, line=-8,outer =TRUE)
mtext("Freezing n-factor", side=2, cex=7, line=15)
legend(TminH-20, .4,c("orgnaic","mineral"),col=c("seagreen4","saddlebrown"), pch=19,
bty="n", cex=10 )	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datF$depth[datF$classID==2&datF$orgID==1],datF$NT[datF$classID==2&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$depth[datF$classID==2&datF$orgID==2],datF$NT[datF$classID==2&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Grass tund", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)	

		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
		
points(datF$depth[datF$classID==3&datF$orgID==1],datF$NT[datF$classID==3&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$depth[datF$classID==3&datF$orgID==2],datF$NT[datF$classID==3&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Tussock", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datF$depth[datF$classID==4&datF$orgID==1],datF$NT[datF$classID==4&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$depth[datF$classID==4&datF$orgID==2],datF$NT[datF$classID==4&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Shrub tund", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)				
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$depth[datF$classID==5&datF$orgID==1],datF$NT[datF$classID==5&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text(TminL + 10, 1.6, "wetland tund", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)	
box(which="plot")
#
par(mai=c(0,0,0,0))

		
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$depth[datF$classID==6&datF$orgID==1],datF$NT[datF$classID==6&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text(TminL + 10, 1.6, "evergreen boreal", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$depth[datF$classID==7&datF$orgID==1],datF$NT[datF$classID==7&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$depth[datF$classID==7&datF$orgID==2],datF$NT[datF$classID==7&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "mixed boreal", cex=10)	
axis(1, seq(0, 20, by=5), cex.axis=8, padj=1)				
		
box(which="plot")

dev.off()


#make a plot to look at more closely
jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n3\\n_TTdepth.jpg", width=9000,height=2200)	
ab<-layout(matrix(seq(1,7), ncol=7, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),
			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))
			
			
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$depth[datT$classID==1&datT$orgID==1],datT$NT[datT$classID==1&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$depth[datT$classID==1&datT$orgID==2],datT$NT[datT$classID==1&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(10, 1.6, "Herb barren", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)
axis(2, seq(0,1.6, by=.2),cex.axis=8,las=2)
mtext("Maximum Yearly Temperature (C)", side=1, cex=7, line=-8,outer =TRUE)
mtext("Thawing n-factor", side=2, cex=7, line=15)
legend(15, .4,c("orgnaic","mineral"),col=c("seagreen4","saddlebrown"), pch=19,
bty="n", cex=10 )	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datT$depth[datT$classID==2&datT$orgID==1],datT$NT[datT$classID==2&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$depth[datT$classID==2&datT$orgID==2],datT$NT[datT$classID==2&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text( 10, 1.6, "Grass tund", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)

		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
		
points(datT$depth[datT$classID==3&datT$orgID==1],datT$NT[datT$classID==3&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$depth[datT$classID==3&datT$orgID==2],datT$NT[datT$classID==3&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text( 10, 1.6, "Tussock", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datT$depth[datT$classID==4&datT$orgID==1],datT$NT[datT$classID==4&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$depth[datT$classID==4&datT$orgID==2],datT$NT[datT$classID==4&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text( 10, 1.6, "Shrub tund", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)			
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$depth[datT$classID==5&datT$orgID==1],datT$NT[datT$classID==5&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text( 10, 1.6, "wetland tund", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))

		
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$depth[datT$classID==6&datT$orgID==1],datT$NT[datT$classID==6&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text( 10, 1.6, "evergreen boreal", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(-.50, 21), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$depth[datT$classID==7&datT$orgID==1],datT$NT[datT$classID==7&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$depth[datT$classID==7&datT$orgID==2],datT$NT[datT$classID==7&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(10, 1.6, "mixed boreal", cex=10)	
axis(1, seq(0, 25, by=5), cex.axis=8, padj=1)		
		
box(which="plot")

dev.off()