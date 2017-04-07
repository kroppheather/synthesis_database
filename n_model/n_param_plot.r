setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n2")

#read in output files 
#read in stats
datS<-read.csv("model_variaion_stats.csv")
#read in quantiles
datQ<-read.csv("model_variaion_quant.csv")

#combine into single data frame
datC<-cbind(datS,datQ)

#read in n factor data
datT<-read.csv("Thawing_n_forMod.csv")
datF<-read.csv("Freezing_n_forMod.csv")

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
#now pull out rep parms to look at goodness of fit plots
nTrep<-datCS[datCS$param=="nT.rep",]
nFrep<-datCS[datCS$param=="nF.rep",]

##################################################
###################################################
#goodness of fit
par(mai=c(2,2,1,1))
plot(datT$NT,nTrep$M, pch=19, xlim=c(0,1.6), ylim=c(0,1.6),
	xlab="Observed N thaw", ylab="Predicted N thaw", cex.axis=1.5, cex.lab=2)
	
fitT<-lm(nTrep$M~datT$NT)	
summary(fitT)
abline(0,1, lty=2, lwd=2, col="red")
abline(fitT, lwd=2)
legend(.1,1.6, c("1:1 line", "model fit"), lwd=2, lty=c(2,1), col=c("red","black"), bty="n", cex=1.5)

text(1,1.5, paste0("y=",round(summary(fitT)$coefficients[1,1],2),"+",round(summary(fitT)$coefficients[2,1],2),"x"), cex=1.75)
text(1,1.40, expression("R"^{2}~"= 0.69"), cex=1.75)


par(mai=c(2,2,1,1))
plot(datF$NT,nFrep$M, pch=19, xlim=c(0,1.6), ylim=c(0,1.6),
	xlab="Observed N freeze", ylab="Predicted N freeze", cex.axis=1.5, cex.lab=2)
	
fitF<-lm(nFrep$M~datF$NT)	
summary(fitF)
abline(0,1, lty=2, lwd=2, col="red")
abline(fitF, lwd=2)
legend(.1,1.6, c("1:1 line", "model fit"), lwd=2, lty=c(2,1), col=c("red","black"), bty="n", cex=1.5)

text(1,1.5, paste0("y=",round(summary(fitF)$coefficients[1,1],2),"+",round(summary(fitF)$coefficients[2,1],2),"x"), cex=1.75)
text(1,1.40, expression("R"^{2}~"= 0.53"), cex=1.75)

#########################################################
#########################################################
#now look at patterns in data
datBT<-datCS[datCS$param=="betaT1star"|datCS$param=="betaT2"|datCS$param=="betaT3",]
wp<-11
hp<-11
bl<-layout(matrix(seq(1,4), byrow=TRUE, ncol=2), width=c(lcm(wp),lcm(wp),lcm(wp)),
			height=c(lcm(hp),lcm(hp),lcm(hp)))
			
layout.show(bl)	

#plot the response to EVI
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,.55), ylim=c(0,1.7), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$EVI[datT$orgID==1],datT$NT[datT$orgID==1],pch=19)
box(which="plot")
#depth
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,23), ylim=c(0,1.7), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$depth[datT$orgID==1],datT$NT[datT$orgID==1],pch=19)
box(which="plot")
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==1],datBT$M[datBT$param=="betaT3"&datBT$ID==1],
	lwd=2)
	


#plot the response to EVI
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,.55), ylim=c(0,1.7), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$EVI[datT$orgID==2],datT$NT[datT$orgID==2],pch=19)
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==2],datBT$M[datBT$param=="betaT2"&datBT$ID==2],
	lwd=2)
box(which="plot")
#depth
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,23), ylim=c(0,1.7), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$depth[datT$orgID==2],datT$NT[datT$orgID==2],pch=19)
box(which="plot")
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==2],datBT$M[datBT$param=="betaT3"&datBT$ID==2],
	lwd=2)
	
	
#plot paramters for comparision
datBF<-datCS[datCS$param=="betaF1star"|datCS$param=="betaF2"|datCS$param=="betaF3",]
datBT<-datCS[datCS$param=="betaT1star"|datCS$param=="betaT2"|datCS$param=="betaT3",]
wp<-25
hp<-20

	
x.cor1<-c(1,1,3,3)
x.cor2<-c(4,4,6,6)
jpeg(paste0(getwd(),"//reg_param.jpg"), width=2500,height=2000)	

bl<-layout(matrix(seq(1,6), byrow=FALSE, ncol=2), width=c(lcm(wp),lcm(wp),lcm(wp),lcm(wp),lcm(wp),lcm(wp)),
			height=c(lcm(hp),lcm(hp),lcm(hp),lcm(hp),lcm(hp),lcm(hp)))
layout.show(bl)	
#intercept

par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), ylim=c(0,2), xlim=c(0,7), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")

polygon(x.cor1,c(0,datBT$M[datBT$ID==1&datBT$param=="betaT1star"],datBT$M[datBT$ID==1&datBT$param=="betaT1star"],
				0),
			col="seagreen4")

polygon(x.cor2,c(0,datBT$M[datBT$ID==2&datBT$param=="betaT1star"],datBT$M[datBT$ID==2&datBT$param=="betaT1star"],
				0),
			col="mediumseagreen")			
			
arrows(c(2,5), datBT$pc2.5[datBT$param=="betaT1star"],	c(2,5), datBT$pc97.5[datBT$param=="betaT1star"],code=0, lwd=3)		
axis(2, seq(0,2, by=.25), cex.axis=5,las=2)
mtext(expression("Intercept ("~italic("n"[thaw])~")"), side=2, cex=5, line=12)
box(which="plot")
legend(1,2,c("organic","mineral"), fill=c("seagreen4", "mediumseagreen"), bty="n", cex=6)

#depth

par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), ylim=c(-.05,0.01), xlim=c(0,7), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")
box(which="plot")

polygon(x.cor1,c(0,datBT$M[datBT$ID==1&datBT$param=="betaT3"],datBT$M[datBT$ID==1&datBT$param=="betaT3"],
				0),
			col="seagreen4")

polygon(x.cor2,c(0,datBT$M[datBT$ID==2&datBT$param=="betaT3"],datBT$M[datBT$ID==2&datBT$param=="betaT3"],
				0),
			col="mediumseagreen")			
abline(h=0, lwd=2)			
arrows(c(2,5), datBT$pc2.5[datBT$param=="betaT3"],	c(2,5), datBT$pc97.5[datBT$param=="betaT3"],code=0, lwd=3)		
axis(2, seq(-.05,0.00, by=.01), cex.axis=5,las=2)
mtext(expression("Depth Effect (cm"^-1~")"), side=2, cex=5, line=12)


#EVI
par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), ylim=c(-2.5,.1), xlim=c(0,7), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")
box(which="plot")


polygon(x.cor1,c(0,datBT$M[datBT$ID==1&datBT$param=="betaT2"],datBT$M[datBT$ID==1&datBT$param=="betaT2"],
				0),
			col="seagreen4")

polygon(x.cor2,c(0,datBT$M[datBT$ID==2&datBT$param=="betaT2"],datBT$M[datBT$ID==2&datBT$param=="betaT2"],
				0),
			col= "mediumseagreen")			
abline(h=0, lwd=2)			
arrows(c(2,5), datBT$pc2.5[datBT$param=="betaT2"],	c(2,5), datBT$pc97.5[datBT$param=="betaT2"],code=0, lwd=3)		
axis(2, seq(-2.5,-.25, by=.25), cex.axis=5,las=2)
mtext(expression("EVI Effect (-) "), side=2, cex=5, line=12)
axis(1, c(2,5), c("organic","mineral"), cex.axis=5, padj=1, lwd.ticks=3)
#intercept freeze
par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), ylim=c(0,2), xlim=c(0,7), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")

polygon(x.cor1,c(0,datBF$M[datBF$ID==1&datBF$param=="betaF1star"],datBF$M[datBF$ID==1&datBF$param=="betaF1star"],
				0),
			col="deepskyblue4")

polygon(x.cor2,c(0,datBF$M[datBF$ID==2&datBF$param=="betaF1star"],datBF$M[datBF$ID==2&datBF$param=="betaF1star"],
				0),
			col="lightskyblue3")			
			
arrows(c(2,5), datBF$pc2.5[datBF$param=="betaF1star"],	c(2,5), datBF$pc97.5[datBF$param=="betaF1star"],code=0, lwd=3)		
axis(4, seq(0,2, by=.25), cex.axis=5,las=2)
mtext(expression("Intercept ("~italic("n"[freeze])~")"), side=4, cex=5, line=17)
box(which="plot")
legend(1,2,c("organic","mineral"), fill=c("deepskyblue4", "lightskyblue3"), bty="n", cex=6)

#depth
par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), ylim=c(-.02,.02), xlim=c(0,7), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")
box(which="plot")

polygon(x.cor1,c(0,datBF$M[datBF$ID==1&datBF$param=="betaF3"],datBF$M[datBF$ID==1&datBF$param=="betaF3"],
				0),
			col="deepskyblue4")

polygon(x.cor2,c(0,datBF$M[datBF$ID==2&datBF$param=="betaF3"],datBF$M[datBF$ID==2&datBF$param=="betaF3"],
				0),
			col= "lightskyblue3")			
			
arrows(c(2,5), datBF$pc2.5[datBF$param=="betaF3"],	c(2,5), datBF$pc97.5[datBF$param=="betaF3"],code=0, lwd=3)		
axis(4, seq(-.01,.01, by=.01), cex.axis=5,las=2)
mtext(expression("Depth Effect (cm"^-1~")"), side=4, cex=5, line=17)
abline(h=0, lwd=2)


#EVI
par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), ylim=c(-1.3,.25), xlim=c(0,7), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")
box(which="plot")


polygon(x.cor1,c(0,datBF$M[datBF$ID==1&datBF$param=="betaF2"],datBF$M[datBF$ID==1&datBF$param=="betaF2"],
				0),
			col="deepskyblue4")

polygon(x.cor2,c(0,datBF$M[datBF$ID==2&datBF$param=="betaF2"],datBF$M[datBF$ID==2&datBF$param=="betaF2"],
				0),
			col="lightskyblue3")			
abline(h=0, lwd=2)
arrows(c(2,5), datBF$pc2.5[datBF$param=="betaF2"],	c(2,5), datBF$pc97.5[datBF$param=="betaF2"],code=0, lwd=3)		
axis(4, c(-1.2,-1,-.8,-.6,-.4,-.2,0,.2), cex.axis=5,las=2)
mtext(expression("EVI Effect (-) "), side=4, cex=5, line=17)
axis(1, c(2,5), c("organic","mineral"), cex.axis=5, padj=1, lwd.ticks=3)
dev.off()


#now plot vegetation effects

datEF<-datCS[datCS$param=="epsF.star",]
datET<-datCS[datCS$param=="epsT.star",]

	
wp<-82
hp<-25

xcor1<-c(1,4,7,10,13,16,19)

jpeg(paste0(getwd(),"//reg_re.jpg"), width=4000,height=2200)		
	
	bl<-layout(matrix(seq(1,2), ncol=1), width=c(lcm(wp),lcm(wp)),
			height=c(lcm(hp),lcm(hp)))
	layout.show(bl)	
	
	par(mai=c(0,0,0,0))
	plot(c(0,1), c(0,1), ylim=c(-.35,.35), xlim=c(0,22), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")
	box(which="plot")	
	for(i in 1:7){
	polygon(c(xcor1[i],xcor1[i],xcor1[i]+2,xcor1[i]+2), c(0,datET$M[i],datET$M[i],0), col=
				"mediumseagreen")
		}
	arrows(xcor1+1,datET$pc2.5,xcor1+1,datET$pc97.5,code=0 , lwd=3)
	axis(2, c(-.3,-.2,-.1,0,.1,.2,.3), cex.axis=3, las=2)

	abline(h=0)
	
	mtext(expression("Random Effect ("~italic("n"[thaw])~")"), side=2, line=12, cex=4)

	
	#freeze
	par(mai=c(0,0,0,0))
	plot(c(0,1), c(0,1), ylim=c(-.4,.75), xlim=c(0,22), xaxs="i",yaxs="i", axes=FALSE,xlab=" ", ylab=" ", type="n")
	box(which="plot")

	for(i in 1:7){
	polygon(c(xcor1[i],xcor1[i],xcor1[i]+2,xcor1[i]+2), c(0,datEF$M[i],datEF$M[i],0), col=
				"deepskyblue4")
		}
	arrows(xcor1+1,datEF$pc2.5,xcor1+1,datEF$pc97.5,code=0 , lwd=3)
	axis(2, c(-.4,-.3,-.2,-.1,0,.1,.2,.3,.4,.5,.6,.7), cex.axis=3, las=2)
	axis(1, xcor1+1, c("herb barren", "grass tundra", "tussock tundra", "shrub tundra", "wetland", "evergreen boreal", "mixed boreal"),
		cex.axis=3.25, padj=1, lwd.ticks=3)
	abline(h=0)
	
	mtext(expression("Random Effect ("~italic("n"[freeze])~")"), side=2, line=12, cex=4)
	mtext("Vegetation community", side=1, line=10, cex=4)
	
dev.off()	


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


library(plyr)
#read in world climate data
datWC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\world_clim.csv")
colnames(datWC)[1]<-"siteid"	
#join world clim to the n tables
datTC<-join(datT,datWC, by="siteid", type="left")	
datFC<-join(datF,datWC, by="siteid", type="left")	

plot(datTC$prec[datTC$depth<10],datTC$NT[datTC$depth<10] )
plot(datFC$prec[datTC$depth<10],datFC$NT[datTC$depth<10] )
plot(datTC$tavg[datTC$depth<10],datTC$NT[datTC$depth<10] )
plot(datFC$tavg[datTC$depth<10],datFC$NT[datTC$depth<10] )

plot(datTC$tmin[datTC$depth<10],datTC$NT[datTC$depth<10] )
plot(datFC$tmin[datTC$depth<10],datFC$NT[datTC$depth<10] )
plot(datTC$tmax[datTC$depth<10],datTC$NT[datTC$depth<10] )
plot(datFC$tmax[datTC$depth<10],datFC$NT[datTC$depth<10] )


######################################
#read in data

setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n3")

datF<-read.csv("Freezing_n_forMod.csv")
datT<-read.csv("Thawing_n_forMod.csv")
datID<-read.csv("vegeorgID_forMod.csv")


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
colnames(datCT)[5]<- "vegeorgID.T"
library(plyr)
datCTI<-join(datCT, datID, by="vegeorgID.T", type="left")

datCF<-datCS[datCS$param=="betaF3",]
colnames(datCF)[5]<- "vegeorgID.T"
datCFI<-join(datCF, datID, by="vegeorgID.T", type="left")
write.table(datCTI, "datCTI.csv", sep=",", row.names=FALSE)
write.table(datCFI, "datCTI.csv", sep=",", row.names=FALSE)

wb<-40
hb<-40

TminH<- -5
TminL<- -50
TmaxH<- 25
TmaxL<-10

#classID code
#1= herb barren
#2 = grasstundra
#3= tussock tundra
#4= shrub tundra
#5= wetland
#6= evergreen boreal
#7= mixed boreal

#make a plot to look at more closely
jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n3\\n_FT.jpg", width=9000,height=2200)	
ab<-layout(matrix(seq(1,7), ncol=7, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),
			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))
			
			
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==1&datF$orgID==1],datF$NT[datF$classID==1&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$Amin[datF$classID==1&datF$orgID==2],datF$NT[datF$classID==1&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Herb barren", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)
axis(2, seq(0,1.6, by=.2),cex.axis=8,las=2)
mtext("Minimum Yearly Temperature (C)", side=1, cex=7, line=-8,outer =TRUE)
mtext("Freezing n-factor", side=2, cex=7, line=15)
legend(TminH-20, .4,c("orgnaic","mineral"),col=c("seagreen4","saddlebrown"), pch=19,
bty="n", cex=10 )	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datF$Amin[datF$classID==2&datF$orgID==1],datF$NT[datF$classID==2&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$Amin[datF$classID==2&datF$orgID==2],datF$NT[datF$classID==2&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Grass tund", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)

		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
		
points(datF$Amin[datF$classID==3&datF$orgID==1],datF$NT[datF$classID==3&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$Amin[datF$classID==3&datF$orgID==2],datF$NT[datF$classID==3&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Tussock", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datF$Amin[datF$classID==4&datF$orgID==1],datF$NT[datF$classID==4&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$Amin[datF$classID==4&datF$orgID==2],datF$NT[datF$classID==4&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "Shrub tund", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)			
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==5&datF$orgID==1],datF$NT[datF$classID==5&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text(TminL + 10, 1.6, "wetland tund", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))

		
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==6&datF$orgID==1],datF$NT[datF$classID==6&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text(TminL + 10, 1.6, "evergreen boreal", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TminL, TminH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datF$Amin[datF$classID==7&datF$orgID==1],datF$NT[datF$classID==7&datF$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datF$Amin[datF$classID==7&datF$orgID==2],datF$NT[datF$classID==7&datF$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TminL + 10, 1.6, "mixed boreal", cex=10)	
axis(1, seq(-45, -5, by=10), cex.axis=8, padj=1)			
		
box(which="plot")

dev.off()



#make a plot to look at more closely
jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n3\\n_TT.jpg", width=9000,height=2200)	
ab<-layout(matrix(seq(1,7), ncol=7, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),
			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))
			
			
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==1&datT$orgID==1],datT$NT[datT$classID==1&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$Amax[datT$classID==1&datT$orgID==2],datT$NT[datT$classID==1&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TmaxL + 10, 1.6, "Herb barren", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)
axis(2, seq(0,1.6, by=.2),cex.axis=8,las=2)
mtext("Maximum Yearly Temperature (C)", side=1, cex=7, line=-8,outer =TRUE)
mtext("Thawing n-factor", side=2, cex=7, line=15)
legend(TmaxH-10, .4,c("orgnaic","mineral"),col=c("seagreen4","saddlebrown"), pch=19,
bty="n", cex=10 )	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datT$Amax[datT$classID==2&datT$orgID==1],datT$NT[datT$classID==2&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$Amax[datT$classID==2&datT$orgID==2],datT$NT[datT$classID==2&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TmaxL + 10, 1.6, "Grass tund", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)

		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
		
points(datT$Amax[datT$classID==3&datT$orgID==1],datT$NT[datT$classID==3&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$Amax[datT$classID==3&datT$orgID==2],datT$NT[datT$classID==3&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TmaxL + 10, 1.6, "Tussock", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)		
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
points(datT$Amax[datT$classID==4&datT$orgID==1],datT$NT[datT$classID==4&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$Amax[datT$classID==4&datT$orgID==2],datT$NT[datT$classID==4&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TmaxL + 10, 1.6, "Shrub tund", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)			
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==5&datT$orgID==1],datT$NT[datT$classID==5&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text(TmaxL + 10, 1.6, "wetland tund", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)
box(which="plot")
#
par(mai=c(0,0,0,0))

		
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==6&datT$orgID==1],datT$NT[datT$classID==6&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
text(TmaxL + 10, 1.6, "evergreen boreal", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)	
box(which="plot")
#
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", axes=FALSE, xlim=c(TmaxL, TmaxH), ylim=c(0,1.75), xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
points(datT$Amax[datT$classID==7&datT$orgID==1],datT$NT[datT$classID==7&datT$orgID==1], 
		pch=19, cex=12, col="seagreen4")		
points(datT$Amax[datT$classID==7&datT$orgID==2],datT$NT[datT$classID==7&datT$orgID==2], 
		pch=19, cex=12, col="saddlebrown")	
text(TmaxL + 10, 1.6, "mixed boreal", cex=10)	
axis(1, seq(15, 25, by=5), cex.axis=8, padj=1)		
		
box(which="plot")

dev.off()



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