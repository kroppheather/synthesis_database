##########################################################
########Vegetation classification analysis     ###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in soil              ###
###  to analyze patterns in                            ###
### air and shallow soil temperature coupling          ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### AirRepID,SoilRepID, datCSM(list), datCAM (list)    ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
##########################################################
##########################################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_5\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_5\\vegeID.csv")
#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow_soil\\comp"

#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(soilParm$vegeclass)
AirParm <- join(AirParm, datV, by=c("siteid"), type="left")

#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$parm)

SoilL <- list()

for(i in 1:length(parmV)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmV[i],]

}

#plot all soil parms for correlation
compI <- data.frame(c1 = c(1,1,1,1,1,2,2,2,2,3,3,3,4,4,5),
					c2 = c(2,3,4,5,6,3,4,5,6,4,5,6,5,6,6))
fit <- list()
#doing a lot of comparisions, lower p value with bonfernoi cor
bc <- 0.05/(dim(compI)[1]-1)

for(i in 1:dim(compI)[1]){
	fit[[i]] <- lm(SoilL[[compI$c2[i]]]$Mean~SoilL[[compI$c1[i]]]$Mean)

	jpeg(paste0(plotDI,"\\",parmV[compI$c1[i]],"_vs_",parmV[compI$c2[i]],".jpg"),  width=1000, height=1000, units="px", quality=100)
		par(mai=c(2,2,2,2))
		plot(SoilL[[compI$c1[i]]]$Mean,SoilL[[compI$c2[i]]]$Mean, xlab= paste(parmV[compI$c1[i]]),ylab=paste(parmV[compI$c2[i]]), cex=2, pch=19)
		if(summary(fit[[i]])$coefficients[2,4]<= bc){
			abline(fit[[i]], lwd=2, col="red")
		}else{
			abline(h=summary(fit[[i]])$coefficients[1,1], lwd=2, col="red", lty=3)
		}
		
		mtext(paste("R2 =",round(summary(fit[[i]])$r.squared,3)), side=3, line=3, cex=2) 
		mtext(paste(parmV[compI$c2[i]],"=",round(summary(fit[[i]])$coefficients[1,1],3), " + ", round(summary(fit[[i]])$coefficients[2,1],3),"x",parmV[compI$c1[i]]), side=3, line=1, cex=2)
		
		
	dev.off()

}

wd <- 22
hd <-22

fitV <- list()
for(i in 1:dim(compI)[1]){
	jpeg(paste0(plotDI,"\\vege_comp\\",parmV[compI$c1[i]],"_vs_",parmV[compI$c2[i]],".jpg"),  width=2000, height=2000, units="px", quality=100)
	layout(matrix(seq(1,9), ncol=3, byrow=TRUE), width=rep(lcm(wd), 9), height=rep(lcm(hd),9))
	for(j in 1:dim(datVI)[1]){
		fitV <- lm(SoilL[[compI$c2[i]]]$Mean[SoilL[[compI$c2[i]]]$vegeclass==j]~SoilL[[compI$c1[i]]]$Mean[SoilL[[compI$c1[i]]]$vegeclass==j])
		par(mai=c(1,1,1,1))
		plot(SoilL[[compI$c1[i]]]$Mean[SoilL[[compI$c1[i]]]$vegeclass==j],
			SoilL[[compI$c2[i]]]$Mean[SoilL[[compI$c2[i]]]$vegeclass==j],
			xlab= paste(parmV[compI$c1[i]]),ylab=paste(parmV[compI$c2[i]]), cex=3,cex.axis=3,cex.lab=3, pch=19)
	
		if(summary(fitV)$coefficients[2,4]<= bc){
			abline(fitV, lwd=2, col="red")
		}else{
			abline(h=summary(fitV)$coefficients[1,1], lwd=2, col="red", lty=3)
		}
		mtext(paste("p=",round(summary(fitV)$coefficients[2,4],3)), side=3, line=6, cex=2) 
		mtext(paste("R2 =",round(summary(fitV)$r.squared,3)), side=3, line=3, cex=2) 
		mtext(paste(parmV[compI$c2[i]],"=",round(summary(fitV)$coefficients[1,1],3), " + ", round(summary(fitV)$coefficients[2,1],3),"x",parmV[compI$c1[i]]), side=3, line=1, cex=2)
		mtext(paste(datVI$vegename[j]), side=1, line=5, cex=2)
	}

	dev.off()
}	


AirL <- list()
#see how air temp measures correlate
for(i in 1:length(parmA)){
	AirL[[i]] <- AirParm[AirParm$parm==parmA[i],]

}

#plot all soil parms for correlation
compA <- data.frame(c1 = c(1,1,1,1,2,2,2,3,3,4),
					c2 = c(2,3,4,5,3,4,5,4,5,5))
					
					
fitA <- list()
#doing a lot of comparisions, lower p value with bonfernoi cor
bcA <- 0.05/(dim(compA)[1]-1)

for(i in 1:dim(compA)[1]){
	fitA[[i]] <- lm(AirL[[compA$c2[i]]]$Mean~AirL[[compA$c1[i]]]$Mean)

	jpeg(paste0(plotDI,"\\Air_comp\\",parmA[compA$c1[i]],"_vs_",parmA[compA$c2[i]],".jpg"),  width=1000, height=1000, units="px", quality=100)
		par(mai=c(2,2,2,2))
		plot(AirL[[compA$c1[i]]]$Mean,AirL[[compA$c2[i]]]$Mean, xlab= paste(parmA[compA$c1[i]]),ylab=paste(parmA[compA$c2[i]]), cex=2, pch=19)
		if(summary(fitA[[i]])$coefficients[2,4]<= bcA){
			abline(fitA[[i]], lwd=2, col="red")
		}else{
			abline(h=summary(fitA[[i]])$coefficients[1,1], lwd=2, col="red", lty=3)
		}
		
		mtext(paste("R2 =",round(summary(fitA[[i]])$r.squared,3)), side=3, line=3, cex=2) 
		mtext(paste(parmA[compA$c2[i]],"=",round(summary(fitA[[i]])$coefficients[1,1],3), " + ", round(summary(fitA[[i]])$coefficients[2,1],3),"x",parmA[compA$c1[i]]), side=3, line=1, cex=2)
		
		
	dev.off()

}					

#

#####