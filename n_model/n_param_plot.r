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