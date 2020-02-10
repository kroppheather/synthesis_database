##########################################################
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in                   ###
### air and shallow soil temperature coupling          ###
### focus only on key soil variables: min, max, and    ###
### the timing of the minimum                          ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### AirRepID,SoilRepID, datCSM(list), datCAM (list)    ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
### ThawParm: # of days above or at zero               ###
##########################################################
##########################################################


#######################################
#####read in data                 ##### 
#######################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")



#get unique sites
sitesAll <- unique(SoilRepID$siteid)
soilL <- list()
soilRepIDL <- list()
#subset data into list
for(i in 1:length(sitesAll)){
	soilL[[i]] <- datSM[datSM$siteid==sitesAll[i],]
	soilRepIDL[[i]] <- SoilRepID[SoilRepID$siteid==sitesAll[i],]
	soilRepIDL[[i]]$MeasT <- soilL[[i]]$T[soilRepIDL[[i]]$repID]
}

repCompS <- ldply(soilRepIDL,data.frame)

plot(repCompS$MeasT,repCompS$Mean)

fitS <- lm(repCompS$Mean~repCompS$MeasT)

summary(fitS)
abline(0,1,col="red")
abline(fitS,col="cornflowerblue")


airL <- list()
airRepIDL <- list()
#subset data into list
for(i in 1:length(sitesAll)){
	airL[[i]] <- datAM[datAM$siteid==sitesAll[i],]
	airRepIDL[[i]] <- AirRepID[AirRepID$siteid==sitesAll[i],]
	airRepIDL[[i]]$MeasT <- airL[[i]]$A[airRepIDL[[i]]$repID]
}

repCompA <- ldply(airRepIDL,data.frame)

plot(repCompA$MeasT,repCompA$Mean)

fitA <- lm(repCompA$Mean~repCompA$MeasT)

summary(fitA)
abline(0,1,col="red")
abline(fitA,col="cornflowerblue")



#make panel

plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\figures"	

hd <- 30
wd <- 30
#soil range
tl1 <- -35
th1 <- 29
#air range
tl2 <- -47
th2 <- 28

lws <- 4
sseq <- seq(-35,25, by=10)
aseq <- seq(-50,20, by=10)

png(paste0(plotDI,"\\supp_temperature_fit.png"), width=2000,height=1000,
			units="px")
	layout(matrix(c(1,2),ncol=2,byrow=TRUE), width=c(lcm(wd),lcm(wd)),height=lcm(hd))
	par(mai=c(1,1,1,1))
		
			plot(c(0,1),c(0,1), ylim=c(tl1,th1), xlim=c(tl1,th1),
				xlab="Observed soil temperature (C) ", 
				ylab="Predicted soil temperature (C) ",xaxs="i",yaxs="i",axes=FALSE, cex.lab=2.5)
			points(repCompS$MeasT,repCompS$Mean, pch=19, col=rgb(.5,.5,.5,.15), cex=1.5	)
	text(5,-25, paste("R squared =", round(summary(fitS)$r.squared,2)), cex=2)
	text(5,-30, paste("Predicted =", round(summary(fitS)$coefficients[1,1],2),
		"+",round(summary(fitS)$coefficients[2,1],2),"observed"), cex=2)
	
	abline(0,1,col="red", lwd=lws)
	abline(fitS,col="cornflowerblue", lty=3, lwd= lws)
	axis(1, sseq, cex.axis=2)
	axis(2, sseq, cex.axis=2, las=2)

	par(mai=c(1,1,1,1))
		
			plot(c(0,1),c(0,1), ylim=c(tl2,th2), xlim=c(tl2,th2),
				xlab="Observed air temperature (C) ", 
				ylab="Predicted air temperature (C) ",xaxs="i",yaxs="i",axes=FALSE, cex.lab=2.5)	
				points(repCompA$MeasT,repCompA$Mean, pch=19, col=rgb(.5,.5,.5,.15), cex=1.5	)
	abline(0,1,col="red", lwd= lws)
	abline(fitA,col="cornflowerblue",lty=3, lwd= lws)
	axis(1, aseq, cex.axis=2)
	axis(2, aseq, cex.axis=2, las=2)
	legend("topleft", c("1:1 line", "goodness of fit"), col=c("red","cornflowerblue"),
			lty=c(1,3), lwd=lws, bty="n", cex=2)
	text(5,-35, paste("R squared =", round(summary(fitA)$r.squared,2)), cex=2)
	text(5,-40, paste("Predicted =", round(summary(fitA)$coefficients[1,1],2),
		"+",round(summary(fitA)$coefficients[2,1],2),"observed"), cex=2)		
dev.off()	

