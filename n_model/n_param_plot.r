setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n1")

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
text(1,1.40, expression("R"^{2}~"= 0.61"), cex=1.75)


par(mai=c(2,2,1,1))
plot(datF$NT,nFrep$M, pch=19, xlim=c(0,1.6), ylim=c(0,1.6),
	xlab="Observed N freeze", ylab="Predicted N freeze", cex.axis=1.5, cex.lab=2)
	
fitF<-lm(nFrep$M~datF$NT)	
summary(fitF)
abline(0,1, lty=2, lwd=2, col="red")
abline(fitF, lwd=2)
legend(.1,1.6, c("1:1 line", "model fit"), lwd=2, lty=c(2,1), col=c("red","black"), bty="n", cex=1.5)

text(1,1.5, paste0("y=",round(summary(fitF)$coefficients[1,1],2),"+",round(summary(fitF)$coefficients[2,1],2),"x"), cex=1.75)
text(1,1.40, expression("R"^{2}~"= 0.36"), cex=1.75)

#########################################################
#########################################################
#now look at patterns in data
datBT<-datCS[datCS$param=="betaT1star"|datCS$param=="betaT2"|datCS$param=="betaT3"|datCS$param=="betaT4",]
wp<-11
hp<-11
bl<-layout(matrix(seq(1,6), byrow=TRUE, ncol=3), width=c(lcm(wp),lcm(wp),lcm(wp),lcm(wp),lcm(wp)),
			height=c(lcm(hp),lcm(hp),lcm(hp),lcm(hp),lcm(hp)))
			
layout.show(bl)	

#plot the response to EVI
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,.7), ylim=c(0,1.6), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$EVI[datT$biomeID==1],datT$NT[datT$biomeID==1],pch=19)
box(which="plot")
#depth
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,23), ylim=c(0,1.6), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$depth[datT$biomeID==1],datT$NT[datT$biomeID==1],pch=19)
box(which="plot")
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==1],datBT$M[datBT$param=="betaT3"&datBT$ID==1],
	lwd=2)
	
#OLT
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,365), ylim=c(0,1.6), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$OLT[datT$biomeID==1],datT$NT[datT$biomeID==1],pch=19)
box(which="plot")
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==1],datBT$M[datBT$param=="betaT4"&datBT$ID==1],
	lwd=2)	

#plot the response to EVI
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,.7), ylim=c(0,1.6), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$EVI[datT$biomeID==2],datT$NT[datT$biomeID==2],pch=19)
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==2],datBT$M[datBT$param=="betaT2"&datBT$ID==2],
	lwd=2)
box(which="plot")
#depth
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(0,23), ylim=c(0,1.6), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$depth[datT$biomeID==2],datT$NT[datT$biomeID==2],pch=19)
box(which="plot")
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==2],datBT$M[datBT$param=="betaT3"&datBT$ID==2],
	lwd=2)
	
#OLT
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(-.10,365), ylim=c(0,1.6), xlab=" ", ylab=" ", xaxs="i", yaxs="i", axes=FALSE,
	type="n")
points(datT$OLT[datT$biomeID==2],datT$NT[datT$biomeID==2],pch=19)
box(which="plot")
abline(datBT$M[datBT$param=="betaT1star"&datBT$ID==2],datBT$M[datBT$param=="betaT4"&datBT$ID==2],
	lwd=2)		