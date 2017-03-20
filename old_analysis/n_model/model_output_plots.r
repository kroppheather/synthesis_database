########################################################
#####this output plots model output form the ###########
##### N factor model                         ###########
########################################################
#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

#read in stats
datS<-read.csv("model_variaion2_stats.csv")
#read in quantiles
datQ<-read.csv("model_variaion2_quant.csv")
#combine into single data frame
datA<-cbind(datS,datQ)

#now grab N factor data for comparision
datSN<-read.csv("organized_sN_for_model.csv")

#read in winter N factor data
datWN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\organized_wN_for_model.csv")


#get the information to go along with each year and site

#now work on structuring the data to match up with the model
#start by getting the indexes together for the data
#set up a table to get unique site
Ssite<-unique(data.frame(siteid=datSN$siteid, lat=datSN$lat, lon=datSN$lon,
						loc=datSN$loc))
Ssite$siteIDm<-seq(1,length(Ssite$siteid))

#now create unique site table for winter n factor
Wsite<-unique(data.frame(siteid=datWN$siteid, lat=datWN$lat, lon=datWN$lon,
						loc=datWN$loc))

Wsite$siteIDm<-seq(1,length(Wsite$siteid))		

#need an index that varies by N data for site and year
#get unique years in the dataset
Syear<-data.frame(year=unique(datSN$year))
Syear$year<-sort.int(Syear$year)
Syear$yearID<-seq(1,length(Syear$year))

#get winter year year
Wyear<-data.frame(wyear=unique(datWN$wyear))
Wyear$wyear<-sort.int(Wyear$wyear)
Wyear$yearID<-seq(1,length(Wyear$wyear))

#start by looking at the model results to see how the site and year 
# result in variation in the n factor

#create a row names column with out the vector labels

dexps<-"\\[*[[:digit:]]*\\]"
datA$parms<-gsub(dexps,"",rownames(datA))

#plot the random effect for year
par(mai=c(2,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,26.5),ylim=c(-.12,.1), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Syear)[1]){
	polygon(c(Syear$yearID[i]-.5,Syear$yearID[i]-.5,Syear$yearID[i]+.5,Syear$yearID[i]+.5), 
			c(0,datA$Mean[datA$parms=="eps.star"][i],datA$Mean[datA$parms=="eps.star"][i],0),
			col="grey60")
}
arrows(Syear$yearID,datA$X2.5.[datA$parms=="eps.star"],Syear$yearID,datA$X97.5.[datA$parms=="eps.star"],code=0)
axis(1, Syear$yearID, Syear$year, cex.axis=1.25)
axis(2, seq(-.1,.1,by=.02),las=2,cex.axis=1.25)
mtext("Year", side=1,cex=2,line=3)
mtext("Random Effect", side=2,cex=2,line=5)

#now for winter
par(mai=c(2,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,26.5),ylim=c(-.2,.2), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Wyear)[1]){
	polygon(c(Wyear$yearID[i]-.5,Wyear$yearID[i]-.5,Wyear$yearID[i]+.5,Wyear$yearID[i]+.5), 
			c(0,datA$Mean[datA$parms=="epsW.star"][i],datA$Mean[datA$parms=="epsW.star"][i],0),
			col="grey60")
}
arrows(Wyear$yearID,datA$X2.5.[datA$parms=="epsW.star"],Wyear$yearID,datA$X97.5.[datA$parms=="epsW.star"],code=0)
axis(1, Wyear$yearID, Wyear$wyear, cex.axis=1.25)
axis(2, seq(-.2,.2,by=.02),las=2,cex.axis=1.25)
mtext("Year", side=1,cex=2,line=3)
mtext("Random Effect", side=2,cex=2,line=5)

#make a plot for the site random effect
par(mai=c(3,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,73.5),ylim=c(-1,1), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Ssite)[1]){
	polygon(c(Ssite$siteIDm[i]-.5,Ssite$siteIDm[i]-.5,Ssite$siteIDm[i]+.5,Ssite$siteIDm[i]+.5), 
			c(0,datA$Mean[datA$parms=="alpha.star"][i],datA$Mean[datA$parms=="alpha.star"][i],0),
			col="grey60")
}
arrows(Ssite$siteIDm,datA$X2.5.[datA$parms=="alpha.star"],Ssite$siteIDm,datA$X97.5.[datA$parms=="alpha.star"],code=0)

axis(1, seq(1,dim(Ssite)[1]), Ssite$loc,las=2, cex.axis=1.25)
axis(2, seq(-15,1,by=.5),las=2,cex.axis=1.25)
mtext("Random Effect", side=2,cex=2,line=5)
#winter
par(mai=c(3,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,dim(Wsite)[1]),ylim=c(-.5,1), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Wsite)[1]){
	polygon(c(Wsite$siteIDm[i]-.5,Wsite$siteIDm[i]-.5,Wsite$siteIDm[i]+.5,Wsite$siteIDm[i]+.5), 
			c(0,datA$Mean[datA$parms=="alphaW.star"][i],datA$Mean[datA$parms=="alphaW.star"][i],0),
			col="grey60")
}
arrows(Wsite$siteIDm,datA$X2.5.[datA$parms=="alphaW.star"],Wsite$siteIDm,datA$X97.5.[datA$parms=="alphaW.star"],code=0)

axis(1, seq(1,dim(Wsite)[1]), Wsite$loc,las=2, cex.axis=1.25)
axis(2, seq(-2,3,by=.5),las=2,cex.axis=1.25)
mtext("Random Effect", side=2,cex=2,line=5)


#now see how random effect varies with latitude
par(mai=c(3,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(55,85),ylim=c(-1,1), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")

points(Ssite$lat,datA$Mean[datA$parms=="alpha.star"],pch=19,col="grey60")

arrows(Ssite$lat,datA$X2.5.[datA$parms=="alpha.star"],Ssite$lat,datA$X97.5.[datA$parms=="alpha.star"],code=0)

axis(1, seq(55,85, by=5),  cex.axis=1.25)
axis(2, seq(-2,3.5,by=.5),las=2,cex.axis=1.25)
mtext("Random Effect", side=2,cex=2,line=5)

##############################
####Now see how the variation
####is predicted just by
#### distance and random effects

plot(datSN$n, datA$Mean[datA$parms=="mu.nS"], xlim=c(0,1.6),ylim=c(0,1.6), pch=19) 
abline(0,1)

fitRE<-lm(datA$Mean[datA$parms=="mu.nS"]~datSN$n)
summary(fitRE)
abline(fitRE, lty=2)

plot(datWN$n, datA$Mean[datA$parms=="mu.nW"], xlim=c(0,1.6),ylim=c(0,1.6), pch=19) 
abline(0,1)

fitREW<-lm(datA$Mean[datA$parms=="mu.nW"]~datWN$n)
summary(fitREW)
abline(fitREW, lty=2)



#get soil depths for each site

soilD<-unique(data.frame(depth=datSN$depth, siteid=datSN$siteid))
airD<-unique(data.frame(height=datSN$height, siteid=datSN$siteid))


#########################################################################################
#########################################################################################
###############T diff model output ######################################################
#########################################################################################
library(plyr)
#read in data 
datQ<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_Tdiff_quant.csv")
datM<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_Tdiff_stats.csv")

datD<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\datTdiff_model.csv")
#get back the original site id and name
datS<-unique(data.frame(siteid=datD$siteid, model.site=datD$siteidm,sitename=datD$loc))


#combine quantiles with means
datA<-cbind(datM,datQ)
dexps<-"\\[*[[:digit:]]*\\]"
datA$parms<-gsub(dexps,"",rownames(datA))

#start by looking at goodness of fit
datr<-datA[datA$parms=="Tdiff.rep",]

plot(datD$TdiffA,datr$Mean, xlim=c(-30,30),ylim=c(-30,30))
abline(0,1, lwd=2, col="red")
fit1<-lm(datr$Mean~datD$TdiffA)
summary(fit1)
abline(fit1, lwd=2, lty=2)


#now look at parameters
datint<-datA[datA$parms=="beta1star",]
dats1<-datA[datA$parms=="beta2",]
dats2<-datA[datA$parms=="beta3",]

parmall<-data.frame(b1=datint$Mean,b1l=datint$X2.5.,b1h=datint$X97.5.,b2=dats1$Mean,b2l=dats1$X2.5.,b2h=dats1$X97.5.,
						b3=dats2$Mean,b3l=dats2$X2.5.,b3h=dats2$X97.5.,model.site=seq(1,dim(datint)[1]))

parmS<-join(parmall,datS, by="model.site", type="inner")						
	

	
#combine paramer table 
xp<-seq(1,dim(parmS)[1])
par(mai=c(3,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,length(xp)+1), ylim=c(-15,5),xlab=" ", ylab=" ", axes=FALSE,xaxs="i", yaxs="i")
for( i in 1:dim(parmS)[1]){
	polygon(c(xp[i]-.5,xp[i]-.5,xp[i]+.5,xp[i]+.5), c(0,parmS$b1[i],parmS$b1[i],0),col="grey65")
}
arrows(xp,parmS$b1l,xp,parmS$b1h, code=0)
axis(1,xp,parmS$sitename, las=2)
axis(2,seq(-15,5,by=5), las=2)
#now look at slope 1
xp<-seq(1,dim(parmS)[1])
par(mai=c(3,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,length(xp)+1), ylim=c(-25,20),xlab=" ", ylab=" ", axes=FALSE,xaxs="i", yaxs="i")
for( i in 1:dim(parmS)[1]){
	polygon(c(xp[i]-.5,xp[i]-.5,xp[i]+.5,xp[i]+.5), c(0,parmS$b2[i],parmS$b2[i],0),col="grey65")
}
arrows(xp,parmS$b2l,xp,parmS$b2h, code=0)
axis(1,xp,parmS$sitename, las=2)
axis(2,seq(-25,20,by=5), las=2)

#now look at slope 2
xp<-seq(1,dim(parmS)[1])
par(mai=c(3,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,length(xp)+1), ylim=c(-25,20),xlab=" ", ylab=" ", axes=FALSE,xaxs="i", yaxs="i")
for( i in 1:dim(parmS)[1]){
	polygon(c(xp[i]-.5,xp[i]-.5,xp[i]+.5,xp[i]+.5), c(0,parmS$b3[i],parmS$b3[i],0),col="grey65")
}
arrows(xp,parmS$b3l,xp,parmS$b3h, code=0)
axis(1,xp,parmS$sitename, las=2)
axis(2,seq(-25,20,by=5), las=2)
#now look at year random effect
#plot the random effect for year
Syear<-data.frame(year=sort.int(unique(datD$year)))
Syear$yearID<-seq(1,27)

par(mai=c(2,2,1,1))
plot(c(0,1),c(0,1), type="n", xlim=c(0,26.5),ylim=c(-1.0,1.6), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Syear)[1]){
	polygon(c(Syear$yearID[i]-.5,Syear$yearID[i]-.5,Syear$yearID[i]+.5,Syear$yearID[i]+.5), 
			c(0,datA$Mean[datA$parms=="eps.star"][i],datA$Mean[datA$parms=="eps.star"][i],0),
			col="grey60")
}
arrows(Syear$yearID,datA$X2.5.[datA$parms=="eps.star"],Syear$yearID,datA$X97.5.[datA$parms=="eps.star"],code=0)
axis(1, Syear$yearID, Syear$year, cex.axis=1.25)
axis(2, seq(-2,2,by=.1),las=2,cex.axis=1.25)
mtext("Year", side=1,cex=2,line=3)
mtext("Random Effect", side=2,cex=2,line=5)


#############################################################
####show the T diff function with data for several sites ####
#############################################################

#parmS
#subset eps 
epsS<-data.frame(eps=datA$Mean[datA$parms=="eps.star"],epsl=datA$X2.5.[datA$parms=="eps.star"],epsh=datA$X97.5.[datA$parms=="eps.star"])

Ftd<-function(b1,b2,b3,eps,Tlength){
	ifelse(0.5-Tlength>=0,b1+(b2*(Tlength*Tlength))+eps,b1+(b3*(Tlength*Tlength))+eps)
}

#now look at data to plot a few of the sites and years
#look at siteid: 193, 100,73, 60,62,5
sitesub<-c(5,60,62,73,100,193)
parmsite<-c(4,53,54,65,75,153)
colortype<-c("red","sky","gold", "green","purple", "orange")
datDS<-list()
yearS<-list()
epsDS<-list()
colall<-list()
for(i in 1:length(sitesub)){
	datDS[[i]]<-datD[datD$siteid==sitesub[i],]
	yearS[[i]]<-unique(datDS[[i]]$yearid)
	epsDS[[i]]<-epsS[yearS[[i]],]
	colall[[i]]<-colors()[grep(colortype[[i]],colors())]
	colall[[i]]<-colall[[i]][1:length(yearS[[i]])]
}
parmSS<-parmS[parmsite,]

xseq<-seq(0,1,by=.01)


#now make the T diff plot
plot(c(0,1),c(0,1), type="n", ylim=c(-25,10), xlim=c(0,1), xaxs="i",yaxs="i",axes=FALSE,xlab=" ",ylab=" ")
for(i in 1:length(sitesub)){
	for(j in 1:length(yearS[[i]])){
	points(datDS[[i]]$dayX[datDS[[i]]$yearid==yearS[[i]][j]],datDS[[i]]$TdiffA[datDS[[i]]$yearid==yearS[[i]][j]], col=colall[[i]][j], pch=19)
	points(xseq,Ftd(parmSS$b1[i],parmSS$b2[i],parmSS$b3[i],epsDS[[i]]$eps[j],xseq), col=colall[[i]][j], type="l",lwd=2)
	}
}

points(xseq,Ftd(.2,-100,25,0,xseq), type="l",lwd=2)