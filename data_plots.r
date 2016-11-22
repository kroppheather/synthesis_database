#################################################
#####Plot initial data to get a  ################
##### sense for models           ################
#####Read in Vegetation N factor ################
#################################################
#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")
datS<-read.csv("SummerNvege.csv")
datW<-read.csv("WinterNvege.csv")

#first check for any N factor outliers
datS[which(datS$n>2),]

#Greenland site has two N factors above 100, and suggests some 
#non number data is present. Need to just filter out for now
datSu<-datS[datS$n<2,]

#plot organic vs n
par(mfrow=c(1,2))
plot(datSu$olt, datSu$n, pch=19, xlab="Organic Layer thickness (cm)",
		ylab="Summer N factor")
plot(datW$olt, datW$n, pch=19, xlab="Organic Layer thickness (cm)",
		ylab="Winter N factor")
		
#plot shrub cover
par(mfrow=c(1,2))
plot(datSu$shrub.pc, datSu$n, pch=19, xlab="Shrub % cover",
		ylab="Summer N factor")
		
plot(datW$shrub.pc, datW$n, pch=19, xlab="Shrub % cover",
		ylab="Winter N factor")
		
#plot graminoid cover
par(mfrow=c(1,2))
plot(datSu$gram.pc, datSu$n, pch=19, xlab="Graminoid % cover",
		ylab="Summer N factor")
		
plot(datW$gram.pc, datW$n, pch=19, xlab="Graminoid % cover",
		ylab="Winter N factor")
		
#plot moss cover
par(mfrow=c(1,2))
plot(datSu$moss.pc, datSu$n, pch=19, xlab="Moss % cover",
		ylab="Summer N factor")
		
plot(datW$moss.pc, datW$n, pch=19, xlab="Moss % cover",
		ylab="Winter N factor")	

#plot ground cover
par(mfrow=c(1,2))
plot(datSu$ground.pc, datSu$n, pch=19, xlab="ground % cover",
		ylab="Summer N factor")
		
plot(datW$ground.pc, datW$n, pch=19, xlab="ground % cover",
		ylab="Winter N factor")		
				
#figure out how distance is a factor
SummD<-datSu$height+(datSu$depth/100)
WintD<-datW$height+(datW$depth/100)		

par(mfrow=c(1,2))
plot(SummD, datSu$n, pch=19, xlab="Sensor Dist",
		ylab="Summer N factor")
plot(WintD, datW$n, pch=19, xlab="Sensor Dist",
		ylab="Winter N factor")
		

