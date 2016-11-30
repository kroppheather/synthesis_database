########################################################
#####this output plots model output form the ###########
##### N factor model                         ###########
########################################################
#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

#read in stats
datS<-read.csv("model_variaion1_stats.csv")
#read in quantiles
datQ<-read.csv("model_variaion1_quant.csv")
#combine into single data frame
datA<-cbind(datS,datQ)

#now grab N factor data for comparision
datSN<-read.csv("SummerNvege.csv")

#read in winter N factor data
datWN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\WinterNvege.csv")

#Apply data filter used in the model
#This needs to be fixed, but
#until that can happen need filter out
datSN<-datSN[datSN$T<3000,]

datSN<-datSN[datSN$depth<=10,]
datWN<-datWN[datWN$depth<=10,]
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

plot(datSN$n, datA$Mean[datA$parms=="mu.nS"], xlim=c(-.5,1.6),ylim=c(-.5,1.6), pch=19) 
abline(0,1)

fitRE<-lm(datA$Mean[datA$parms=="mu.nS"]~datSN$n)
summary(fitRE)
abline(fitRE, lty=2)

#get soil depths for each site

soilD<-unique(data.frame(depth=datSN$depth, siteid=datSN$siteid))
airD<-unique(data.frame(height=datSN$height, siteid=datSN$siteid))