##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
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
##########################################################
##########################################################


#######################################
#####read in data                 ##### 
#######################################

                    
#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")

#######################################
#####libraries                    ##### 
#######################################
library(rjags)
library(coda)
library(mcmcplots)
library(plyr)

#######################################
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\plots\\model_all"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run7"
Nrun <- 7
#######################################
#####set up colors               ##### 
#######################################
vegeclassColors <- data.frame(vegeclass=seq(1,9),
						coli=c(rgb(77/255,77/255,77/255),
								rgb(0/255,110/255,130/255),
								rgb(160/255,130/255,180/255),
								rgb(130/255,160/255,190/255),
								rgb(250/255,120/255,80/255),
								rgb(250/255,230/255,140/255),
								rgb(50/255,80/255,10/255),
								rgb(170/255,190/255,140/255),
								rgb(240/255,240/255,50/255)))

#######################################
#####organize data                ##### 
#######################################


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)

#create unique names for air
colnames(AirParm)[1:4] <- paste0("A",colnames(AirParm)[1:4])
colnames(AirParm)[8] <- paste0("A",colnames(AirParm)[8])
#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$Aparm)

#pull out relevant parameters
#for analysis: TminS, TmaxS, peakWS
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "TaverageS") 

parmAs <- c("TminA","TmaxA", "TaverageA") 


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmVs)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmVs[i]&SoilParm$depth<=20,]
	#calculate mean
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
	#add a regression ID
	SoilL[[i]]$regID <- rep(i,dim(SoilL[[i]])[1])
}

AirL <- list()
AirMs <- numeric(0)
for(i in 1:length(parmAs)){
	AirL[[i]] <- AirParm[AirParm$Aparm==parmAs[i],]
	#calculate mean
	AirMs[i] <- round(mean(AirL[[i]]$AMean),0)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}


#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)



#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#get unique veg regression id

regvegeID <- unique(data.frame(vegeclass=ParmAll$vegeclass,regID=ParmAll$regID))
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#now join back into parmall
ParmAll <- join(ParmAll,regvegeID, by=c("vegeclass","regID"),type="left")




#get range of precip and air temps in each vegetation group for each regression
#get minimumAir
minAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="min")
maxAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="max")
meanAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="mean")




regvegeID$minAir <- minAir$x
regvegeID$maxAir <- maxAir$x
regvegeID$meanAir <- round(meanAir$x,0)



#get min and max of data for plotting
#air
regMinA <- aggregate(ParmAll$AMean,by=list(ParmAll$regID),FUN="min")
regMinA$x <- round_any(regMinA$x,5,floor)

regMaxA <- aggregate(ParmAll$AMean,by=list(ParmAll$regID),FUN="max")
regMaxA$x <- round_any(regMaxA$x,5,ceiling)



#set up in matrix for plotting
regPlotA <- matrix(rep(NA,dim(regvegeID)[1]*200),ncol=dim(regvegeID)[1])
regTempA <- numeric(0)

for(i in 1:dim(regvegeID)[1]){
	regTempA <- seq(regMinA$x[regvegeID$regID[i]],regMaxA$x[regvegeID$regID[i]],length.out=200)
	regPlotA[,i] <- regTempA

}



#######################################
#####look at model                ##### 
#######################################


#read in model results 

datM <- read.csv(paste0(modDI,"\\vege_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\vege_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))

#isolate betas

beta0 <- datC[datC$parms=="beta0",]
beta1 <- datC[datC$parms=="beta1",]
beta2 <- datC[datC$parms=="beta2",]

#add in sig test
beta1$sigID <- ifelse(beta1$X0.2.<0&beta1$X99.8.<0,1,
				ifelse(beta1$X0.2.>0&beta1$X99.8.>0,1,0))
				
beta2$sigID <- ifelse(beta2$X0.2.<0&beta2$X99.8.<0,1,
				ifelse(beta2$X0.2.>0&beta2$X99.8.>0,1,0))				


#add identifier info
beta0 <- data.frame(beta0,regvegeID )
beta1 <- data.frame(beta1,regvegeID )
beta2 <-data.frame(beta2,regvegeID )


datVI$name2 <- c("herb barren", "graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra",
					"wetland","evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")

#######################################
#####plot parameter comparison   ##### 
#######################################



wd <- 45
hd <- 40


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(-35,0,-15)
yhi <- c(10,25,15)
yls1 <- c(-1,-1,-1)
yhs1 <- c(1,.5,1)
yls2 <- c(-1,-1,-2.5)
yhs2 <- c(1.5,2,2)


xl <- -1
xh <- 27
alw <- 2
zlw <- 10
mlw <- 5
#axis labels
tlw <- 4
alx <- 4
mlx <- 7

yii <- c(5,5,1)
yi1 <- c(.1,.1,.5)
yi2 <- c(.5,.5,.5)
yi3 <- c(.1,.1,.1)
#three regressions
regName <- c("Soil min vs air min","Soil max vs air max", "Time of soil min vs time of air min")


for(i in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_parm",i,".jpg"), width=5500,height=2500,
			quality=100,units="px")
	layout(matrix(seq(1,3),ncol=3), width=rep(lcm(wd),3),height=rep(lcm(hd),3))
		#plot intercept
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==i&beta0$vegeclass==j],beta0$X75.[beta0$regID==i&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==i&beta0$vegeclass==j],beta0$X25.[beta0$regID==i&beta0$vegeclass==j]),
						col="tomato3",border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==i&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==i&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.2.[beta0$regID==i&beta0$vegeclass==j],
						xseq[j],beta0$X99.8.[beta0$regID==i&beta0$vegeclass==j],
						code=0, lwd=alw)
			}
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(datVI$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yli[i],yhi[i], by=yii[i]),rep("",length(seq(yli[i],yhi[i], by=yii[i]))), lwd.ticks=tlw)
		mtext(seq(yli[i],yhi[i], by=yii[i]),at=seq(yli[i],yhi[i], by=yii[i]),cex=alx,line=3,las=2,side=2)
		mtext("Intercept", side=3, line=5, cex=mlx)
		mtext(paste(regName[i]), side=3, outer=TRUE,line=-40,cex=12)
		box(which="plot")
		#plot slope 1
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls1[i],yhs1[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		abline(h=0,	lwd	=zlw, col="grey75",lty=3)			
			for(j in 1:9){
			if(beta1$sigID[beta1$regID==i&beta1$vegeclass==j]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta1$X25.[beta1$regID==i&beta1$vegeclass==j],beta1$X75.[beta1$regID==i&beta1$vegeclass==j],
							beta1$X75.[beta1$regID==i&beta1$vegeclass==j],beta1$X25.[beta1$regID==i&beta1$vegeclass==j]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta1$X25.[beta1$regID==i&beta1$vegeclass==j],beta1$X75.[beta1$regID==i&beta1$vegeclass==j],
							beta1$X75.[beta1$regID==i&beta1$vegeclass==j],beta1$X25.[beta1$regID==i&beta1$vegeclass==j]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,beta1$Mean[beta1$regID==i&beta1$vegeclass==j],
						xseq[j]+1,beta1$Mean[beta1$regID==i&beta1$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta1$X0.2.[beta1$regID==i&beta1$vegeclass==j],
						xseq[j],beta1$X99.8.[beta1$regID==i&beta1$vegeclass==j],
						code=0, lwd=alw)
			}			
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(datVI$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yls1[i],yhs1[i], by=yi1[i]),rep("",length(seq(yls1[i],yhs1[i], by=yi1[i]))), lwd.ticks=tlw)
		mtext(seq(yls1[i],yhs1[i], by=yi1[i]),at=seq(yls1[i],yhs1[i], by=yi1[i]),cex=alx,line=3,las=2,side=2)	
		mtext("Slope depth", side=3, line=5, cex=mlx)
		box(which="plot")	

		#plot slope 2
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls2[i],yhs2[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)	
			abline(h=0,	lwd	=zlw, col="grey75",lty=3)	
			for(j in 1:9){
			if(beta2$sigID[beta2$regID==i&beta2$vegeclass==j]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==i&beta2$vegeclass==j],beta2$X75.[beta2$regID==i&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==i&beta2$vegeclass==j],beta2$X25.[beta2$regID==i&beta2$vegeclass==j]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==i&beta2$vegeclass==j],beta2$X75.[beta2$regID==i&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==i&beta2$vegeclass==j],beta2$X25.[beta2$regID==i&beta2$vegeclass==j]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,beta2$Mean[beta2$regID==i&beta2$vegeclass==j],
						xseq[j]+1,beta2$Mean[beta2$regID==i&beta2$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta2$X0.2.[beta2$regID==i&beta2$vegeclass==j],
						xseq[j],beta2$X99.8.[beta2$regID==i&beta2$vegeclass==j],
						code=0, lwd=alw)
		}
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(datVI$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yls2[i],yhs2[i], by=yi2[i]),rep("",length(seq(yls2[i],yhs2[i], by=yi2[i]))), lwd.ticks=tlw)
		mtext(seq(yls2[i],yhs2[i], by=yi2[i]),at=seq(yls2[i],yhs2[i], by=yi2[i]),cex=alx,line=3,las=2,side=2)
		mtext("Slope air", side=3, line=5, cex=mlx)
		box(which="plot")	


		
	dev.off()		
	
}	


#######################################
#####check fit                    ##### 
#######################################
reps <- datC$Mean[datC$parms=="repSoilP"]
plotDI2 <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\figures"	


ll <- c(-35,0,-15)
hh <- c(0,25,10)
lname <- c("minimum soil temperature (C)", "maximum soil temperature (C)","mean annual soil temperature (C)")
tx1 <- c(-23,8,-7)
ty1 <- c(-1,24,9)
tx2 <- c(-23,8,-7)
ty2 <- c(-4,22,7)


hd <- 20
wd <- 20

png(paste0(plotDI2,"\\supp_vege_fit.png"), width=1800,height=700,
			units="px")
	layout(matrix(seq(1,3),ncol=3,byrow=TRUE), width=c(lcm(wd),lcm(wd),lcm(wd)),height=lcm(hd))
	
for(i in 1:3){
	par(mai=c(1,1,1,1))
	plot(ParmAll$Mean[ParmAll$regID==i], reps[ParmAll$regID==i],
		xlim=c(ll[i],hh[i]), ylim=c(ll[i],hh[i]),
			xlab=paste("Observed",lname[i]),ylab=paste("Predicted",lname[i]),
			cex.lab=2.5, cex.axis=2, cex=2, col=rgb(.5,.5,.5,.3), pch=19)
	fit <- lm(reps[ParmAll$regID==i]~ParmAll$Mean[ParmAll$regID==i])
	abline(fit, lwd=3,lty=3)
	abline(0,1,lwd=3,col="red")
	text(tx1[i],ty1[i], paste("Predicted=",round(summary(fit)$coefficients[1,1],2),
				"+",round(summary(fit)$coefficients[2,1],2),"Observed"),cex=2)
	
	text(tx2[i],ty2[i], paste("R2=", round(summary(fit)$r.squared,2)), cex=2)
	
}
dev.off()