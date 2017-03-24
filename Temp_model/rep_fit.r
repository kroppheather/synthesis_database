library(plyr)
#setwd
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod1\\output_u7")
#model results
datm<-read.csv("Temp_mod7_stats.csv")
dats<-read.csv("Temp_mod7_quant.csv")
#index for randomly selected rep monitor
datsid<-read.csv("SoilrepID.csv")
dataid<-read.csv("AirrepID.csv")
#real data
datS<-read.csv("Tsoil_model.csv")
datA<-read.csv("Tair_model.csv")


#read in ids for parameters
datSDWS<-read.csv("SoilIDS.csv")
datSDWA<-read.csv("AirIDS.csv")

#read om ids for just site and depth parameters
datSDS<-read.csv("SoilIDS_SD.csv")
datSDA<-read.csv("AirIDS_SD.csv")

#now add the id for n factor
datComb<-read.csv("NfactorIDS.csv")

###now get parameter names and id number

#now join means with quantiles
datC<-cbind(datm,dats)
#make a param vector
dexps<-"\\[*[[:digit:]]*\\]"
datC$parms1<-gsub(dexps,"",rownames(datC))


#now add id number
dexps2<-"\\D"
pnames<-rownames(datC)
parms2<-gsub(dexps2,"",pnames)
datC$parms2<-c(as.numeric(parms2))

datC<-data.frame(M=datC[,1],pc2.5=datC[,5],pc97.5=datC[,9],param=as.character(datC[,10]),ID=datC[,11])

datCFN<-datC[datC$param=="Fn",]
datCTN<-datC[datC$param=="Tn",]


datC<-datC[datC$param!="sig.muS",]
datC<-datC[datC$param!="sig.muA",]

datC<-datC[datC$param!="sig.muS",]
datC<-datC[datC$param!="sig.muA",]

datCT<-datC[datC$param!="Fn",]

datCT<-datCT[datCT$param!="Tn",]
datCT<-datCT[datCT$param!="TempA",]
datCT<-datCT[datCT$param!="TempS",]
datCT<-datCT[datCT$param!="TempA.rep",]
datCT<-datCT[datCT$param!="TempS.rep",]

#now combine back with id info
#set up ids for each individual type of id 
IDSJ<-data.frame(depth=c(datSDWA$height,datSDWS$depth,datSDWA$height,datSDWS$depth,datSDWA$height,datSDWS$depth,datSDWA$height,datSDWS$depth,datSDA$height,datSDS$depth),
				siteid=c(datSDWA$siteid,datSDWS$siteid,datSDWA$siteid,datSDWS$siteid,datSDWA$siteid,datSDWS$siteid,datSDWA$siteid,datSDWS$siteid,datSDA$siteid,datSDS$siteid),
				ID=c(datSDWA$SDWA,datSDWS$SDWS,datSDWA$SDWA,datSDWS$SDWS,datSDWA$SDWA,datSDWS$SDWS,datSDWA$SDWA,datSDWS$SDWS,datSDA$SDA,datSDS$SDS),
				wyear=c(datSDWA$wyear,datSDWS$wyear,datSDWA$wyear,datSDWS$wyear,datSDWA$wyear,datSDWS$wyear,datSDWA$wyear,datSDWS$wyear,rep(NA,dim(datSDA)[1]),rep("NA",dim(datSDS)[1])),
				param=c(rep("AmpA",dim(datSDWA)[1]),rep("AmpS",dim(datSDWS)[1]),
				rep("FDDA",dim(datSDWA)[1]),rep("FDDS",dim(datSDWS)[1]),
				rep("T.aveA",dim(datSDWA)[1]),rep("T.aveS",dim(datSDWS)[1]),
				rep("TDDA",dim(datSDWA)[1]),rep("TDDS",dim(datSDWS)[1]),
				rep("startA",dim(datSDA)[1]),rep("startS",dim(datSDS)[1])))


datST<-join(datCT,IDSJ,by=c("param","ID"),type="left")


#now pull out data for goodness of fit

datRS<-datC[datC$param=="TempS.rep",]
datRA<-datC[datC$param=="TempA.rep",]
#now pull out matching soil measurements
STobs<-datS$T[datsid$x]
ATobs<-datA$A[dataid$x]

#now make a plot


#somehow decdate is influencing site 189 in the rep data
par(mfrow=c(1,2))
plot(STobs[datRS$M<100],datRS$M[datRS$M<100], pch=19, xlim=c(-35,30), ylim=c(-35,30))
soilfit<-lm(datRS$M[datRS$M<100]~STobs[datRS$M<100])
summary(soilfit)
abline(soilfit, col="tomato4", lwd=3)
abline(0,1, col="red", lwd=3, lty=2)
text(-15,25,"y = -0.20 + 0.87* Tobs", cex=1.5)
text(-15,20,"R2= 0.863", cex=1.5)
plot(ATobs,datRA$M, pch=19, xlim=c(-45,30), ylim=c(-45,30))
airfit<-lm(datRA$M~ATobs)
summary(airfit)
abline(airfit, col="tomato4", lwd=3)
abline(0,1, col="red", lwd=3, lty=2)
text(-25,25,"y = -1.04 + 0.82* Tobs", cex=1.5)
text(-25,20,"R2= 0.81", cex=1.5)


###now output n factors for model
colnames(datComb)[7]<-"ID"
Fr.n<-join(datCFN, datComb, by="ID", type="left")
Th.n<-join(datCTN, datComb, by="ID", type="left")


write.table(Fr.n, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\FreezeN_out.csv", sep=",", row.names=FALSE)
write.table(Th.n, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\ThawN_out.csv", sep=",", row.names=FALSE)