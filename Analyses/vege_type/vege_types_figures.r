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
#world clim 2 precip in mm
datWC <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\WCprecSites.csv")
colnames(datWC)[1] <- "siteid"
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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run4"
Nrun <-4
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


#join world clim data
SoilParm <- join(SoilParm, datWC, by=c("siteid"), type="left")

#create unique names for air
colnames(AirParm)[1:4] <- paste0("A",colnames(AirParm)[1:4])
colnames(AirParm)[8] <- paste0("A",colnames(AirParm)[8])
#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$Aparm)

#pull out relevant parameters
#for analysis: TminS, TmaxS, peakWS
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "peakWS") 

parmAs <- c("TminA","TmaxA", "peakWA") 


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
	AirMs[i] <- round(mean(AirL[[i]]$AMean),3)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}

#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)

#sum up winter months Oct-Mar
#sum up summer months Aprl-Sept
SoilR$precW <- rowSums(SoilR[,11:13])+rowSums(SoilR[,20:22])
SoilR$precS <- rowSums(SoilR[,14:19])

#set up a vector for the matching timeperiod
SoilR$precR <- ifelse(SoilR$regID==2,SoilR$precS,SoilR$precW)


#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#get unique veg regression id

regvegeID <- unique(data.frame(vegeclass=ParmAll$vegeclass,regID=ParmAll$regID))
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#now join back into parmall
ParmAll <- join(ParmAll,regvegeID, by=c("vegeclass","regID"),type="left")

#precip mean
precMs <- c(100,200,100)



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
beta3 <- datC[datC$parms=="beta3",]
#add in sig test
beta1$sigID <- ifelse(beta1$X0.2.<0&beta1$X99.8.<0,1,
				ifelse(beta1$X0.2.>0&beta1$X99.8.>0,1,0))
				
beta2$sigID <- ifelse(beta2$X0.2.<0&beta2$X99.8.<0,1,
				ifelse(beta2$X0.2.>0&beta2$X99.8.>0,1,0))				
				
beta3$sigID <- ifelse(beta3$X0.2.<0&beta3$X99.8.<0,1,
				ifelse(beta3$X0.2.>0&beta3$X99.8.>0,1,0))

#add identifier info
beta0 <- data.frame(beta0,regvegeID )
beta1 <- data.frame(beta1,regvegeID )
beta2 <-data.frame(beta2,regvegeID )
beta3 <-data.frame(beta3,regvegeID )

datVI$name2 <- c("herb barren", "graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra",
					"wetland","evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")

#######################################
#####plot parameter comparison   ##### 
#######################################



wd <- 45
hd <- 40


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(-35,0,0.2)
yhi <- c(10,25,.65)
yls1 <- c(-1,-1,-.01)
yhs1 <- c(1,.5,.01)
yls2 <- c(-1,-1,-2.5)
yhs2 <- c(1.5,2,2)
yls3 <- c(-.5,-.15,-.005)
yhs3 <- c(.5,.15,.005)

xl <- -1
xh <- 27
alw <- 2
zlw <- 10
mlw <- 5
#axis labels
tlw <- 4
alx <- 4
mlx <- 7

yii <- c(5,5,.1)
yi1 <- c(.1,.1,.005)
yi2 <- c(.5,.5,.5)
yi3 <- c(.1,.1,.001)
#three regressions
regName <- c("Soil min vs air min","Soil max vs air max", "Time of soil min vs time of air min")


for(i in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_parm",i,".jpg"), width=5500,height=2500,
			quality=100,units="px")
	layout(matrix(seq(1,4),ncol=4), width=rep(lcm(wd),4),height=rep(lcm(hd),4))
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

		#plot slope 3
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls3[i],yhs3[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)	
			abline(h=0,	lwd	=zlw, col="grey75",lty=3)	
			for(j in 1:9){
			if(beta3$sigID[beta3$regID==i&beta3$vegeclass==j]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta3$X25.[beta3$regID==i&beta2$vegeclass==j],beta3$X75.[beta3$regID==i&beta3$vegeclass==j],
							beta3$X75.[beta3$regID==i&beta3$vegeclass==j],beta3$X25.[beta3$regID==i&beta3$vegeclass==j]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta3$X25.[beta3$regID==i&beta2$vegeclass==j],beta3$X75.[beta3$regID==i&beta3$vegeclass==j],
							beta3$X75.[beta3$regID==i&beta3$vegeclass==j],beta3$X25.[beta3$regID==i&beta3$vegeclass==j]),
						col="grey75",border=NA)
				}
						
				arrows(xseq[j]-1,beta3$Mean[beta0$regID==i&beta3$vegeclass==j],
						xseq[j]+1,beta3$Mean[beta0$regID==i&beta3$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta3$X0.2.[beta3$regID==i&beta3$vegeclass==j],
						xseq[j],beta3$X99.8.[beta3$regID==i&beta3$vegeclass==j],
						code=0, lwd=alw)
		}
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(datVI$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		
		axis(2,seq(yls3[i],yhs3[i], by=yi3[i]),rep("",length(seq(yls3[i],yhs3[i], by=yi3[i]))), lwd.ticks=tlw)
		mtext(seq(yls3[i],yhs3[i], by=yi3[i]),at=seq(yls3[i],yhs3[i], by=yi3[i]),cex=alx,line=3,las=2,side=2)
		mtext("Slope precipitation", side=3, line=5, cex=mlx)
		box(which="plot")	
		
	dev.off()		
	
}	


#######################################
#####check fit                    ##### 
#######################################
reps <- datC$Mean[datC$parms=="repSoilP"]

ll <- c(-35,0,0)
hh <- c(0,30,.65)
lname <- c("Temperature minimum (C)", "Temperature maximum (C)","Minimum time")
tx1 <- c(-15,15,.35)
ty1 <- c(-3,25,.6)
tx2 <- c(-15,15,.35)
ty2 <- c(-7,20,.5)
for(i in 1:3){

	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_fit",i,".jpg"), width=700,height=700,
			quality=100,units="px")
	plot(ParmAll$Mean[ParmAll$regID==i], reps[ParmAll$regID==i], xlim=c(ll[i],hh[i]), ylim=c(ll[i],hh[i]),
			xlab=lname[i],ylab=lname[i],pch=19)
	fit <- lm(reps[ParmAll$regID==i]~ParmAll$Mean[ParmAll$regID==i])
	abline(fit, lwd=2,lty=3)
	abline(0,1,lwd=2,col="red")
	text(tx1[i],ty1[i], paste("y=",round(summary(fit)$coefficients[1,1],2),
				"+",round(summary(fit)$coefficients[2,1],2),"x"), col="red",cex=2)
	
	text(tx2[i],ty2[i], paste("R2=", round(summary(fit)$r.squared,2)), col="red",cex=2)
	dev.off()
}

#######################################
#####plot just intercept          ##### 
#######################################

wd <- 55
hd <- 30


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(-35,0,0.2)
yhi <- c(10,25,.65)
yii <- c(5,5,.05)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 5
#size of x labels
axc <- 5
#line of x label
xll <- 2
#line for units
yll1 <- 20
#line for name
yll2 <- 30
#cex of axis label
mcx <- 6



jpeg(paste0(plotDI,"\\run",Nrun,"\\intercepts.jpg"), width=2500,height=4100,
			quality=100,units="px")
	layout(matrix(seq(1,3),ncol=1), width=rep(lcm(wd),3),height=rep(lcm(hd),3))
		#plot intercept
	
		par(mai=c(1,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[1],yhi[1]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==1&beta0$vegeclass==j],beta0$X75.[beta0$regID==1&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==1&beta0$vegeclass==j],beta0$X25.[beta0$regID==1&beta0$vegeclass==j]),
						col="tomato3",border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.2.[beta0$regID==1&beta0$vegeclass==j],
						xseq[j],beta0$X99.8.[beta0$regID==1&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("(T"[max],","~degree~"C)")),side=2,line=yll1,cex=mcx)
			mtext("Temperature minimum",side=2,line=yll2,cex=mcx)
			
		par(mai=c(1,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[2],yhi[2]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==2&beta0$vegeclass==j],beta0$X75.[beta0$regID==2&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==2&beta0$vegeclass==j],beta0$X25.[beta0$regID==2&beta0$vegeclass==j]),
						col="tomato3",border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.2.[beta0$regID==2&beta0$vegeclass==j],
						xseq[j],beta0$X99.8.[beta0$regID==2&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[2],yhi[2], by=yii[2]), rep(" ",length(seq(yli[2],yhi[2], by=yii[2]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[2],yhi[2], by=yii[2]),at=seq(yli[2],yhi[2], by=yii[2]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("(T"[max],","~degree~"C)")),side=2,line=yll1,cex=mcx)			
			mtext("Temperature maximum",side=2,line=yll2,cex=mcx)
			
				
		par(mai=c(1,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[3],yhi[3]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==3&beta0$vegeclass==j],beta0$X75.[beta0$regID==3&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==3&beta0$vegeclass==j],beta0$X25.[beta0$regID==3&beta0$vegeclass==j]),
						col="tomato3",border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==3&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==3&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.2.[beta0$regID==3&beta0$vegeclass==j],
						xseq[j],beta0$X99.8.[beta0$regID==3&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[3],yhi[3], by=yii[3]), rep(" ",length(seq(yli[3],yhi[3], by=yii[3]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[3],yhi[3], by=yii[3]),at=seq(yli[3],yhi[3], by=yii[3]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("(p"[min],", proportion of water year)")),side=2,line=yll1,cex=mcx)		
			mtext("Time of minimum",side=2,line=yll2,cex=mcx)			
			mtext(datVI$name2,at=xseq, side=1, line=xll,cex=axc,las=2)

				
			
dev.off()	



#######################################
#####plot just air slope          ##### 
#######################################

wd <- 55
hd <- 30


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(-1,-1,-2.5)
yhi <- c(1.5,2,2)
yii <- c(.5,.5,.5)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 5
#size of x labels
axc <- 5
#line of x label
xll <- 2
#line for units
yll1 <- 20
#line for name
yll2 <- 30
#second line for name
yll3 <- 40
#cex of axis label
mcx <- 6
#zero line
zlw <- 10



jpeg(paste0(plotDI,"\\run",Nrun,"\\air_slopes.jpg"), width=2700,height=4100,
			quality=100,units="px")
	layout(matrix(seq(1,3),ncol=1), width=rep(lcm(wd),3),height=rep(lcm(hd),3))
		#plot intercept
	
		par(mai=c(1,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[1],yhi[1]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			abline(h=0,	lwd	=zlw, col="grey75",lty=3)
			for(j in 1:9){
				if(beta2$sigID[beta2$regID==1&beta2$vegeclass==j]==1){
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==1&beta2$vegeclass==j],beta2$X75.[beta2$regID==1&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==1&beta2$vegeclass==j],beta2$X25.[beta2$regID==1&beta2$vegeclass==j]),
						col="tomato3",border=NA)
				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==1&beta2$vegeclass==j],beta2$X75.[beta2$regID==1&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==1&beta2$vegeclass==j],beta2$X25.[beta2$regID==1&beta2$vegeclass==j]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,beta2$Mean[beta2$regID==1&beta2$vegeclass==j],
						xseq[j]+1,beta2$Mean[beta2$regID==1&beta2$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta2$X0.2.[beta2$regID==1&beta2$vegeclass==j],
						xseq[j],beta2$X99.8.[beta2$regID==1&beta2$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("("~degree~"C soil"~degree~"C air"^"-1",")")),side=2,line=yll1,cex=mcx)
			mtext("Change in",side=2,line=yll3,cex=mcx)
			mtext("temperature minimum",side=2,line=yll2,cex=mcx)
		par(mai=c(1,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[2],yhi[2]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			abline(h=0,	lwd	=zlw, col="grey75",lty=3)
			for(j in 1:9){
				if(beta2$sigID[beta2$regID==2&beta2$vegeclass==j]==1){
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==2&beta2$vegeclass==j],beta2$X75.[beta2$regID==2&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==2&beta2$vegeclass==j],beta2$X25.[beta2$regID==2&beta2$vegeclass==j]),
						col="tomato3",border=NA)
				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==2&beta2$vegeclass==j],beta2$X75.[beta2$regID==2&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==2&beta2$vegeclass==j],beta2$X25.[beta2$regID==2&beta2$vegeclass==j]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,beta2$Mean[beta2$regID==2&beta2$vegeclass==j],
						xseq[j]+1,beta2$Mean[beta2$regID==2&beta2$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta2$X0.2.[beta2$regID==2&beta2$vegeclass==j],
						xseq[j],beta2$X99.8.[beta2$regID==2&beta2$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[2],yhi[2], by=yii[2]), rep(" ",length(seq(yli[2],yhi[2], by=yii[2]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[2],yhi[2], by=yii[2]),at=seq(yli[2],yhi[2], by=yii[2]), side=2, line=xll,cex=axc,las=2)	
		mtext(expression(paste("("~degree~"C soil"~degree~"C air"^"-1",")")),side=2,line=yll1,cex=mcx)
			mtext("Change in",side=2,line=yll3,cex=mcx)
			mtext("temperature maximum",side=2,line=yll2,cex=mcx)
			
				
		par(mai=c(1,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[3],yhi[3]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			abline(h=0,	lwd	=zlw, col="grey75",lty=3)
			for(j in 1:9){
				if(beta2$sigID[beta2$regID==3&beta2$vegeclass==j]==1){
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==3&beta2$vegeclass==j],beta2$X75.[beta2$regID==3&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==3&beta2$vegeclass==j],beta2$X25.[beta2$regID==3&beta2$vegeclass==j]),
						col="tomato3",border=NA)
				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==3&beta2$vegeclass==j],beta2$X75.[beta2$regID==3&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==3&beta2$vegeclass==j],beta2$X25.[beta2$regID==3&beta2$vegeclass==j]),
						col="grey75",border=NA)				
				}
				arrows(xseq[j]-1,beta2$Mean[beta2$regID==3&beta2$vegeclass==j],
						xseq[j]+1,beta2$Mean[beta2$regID==3&beta2$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta2$X0.2.[beta2$regID==3&beta2$vegeclass==j],
						xseq[j],beta2$X99.8.[beta2$regID==3&beta2$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[3],yhi[3], by=yii[3]), rep(" ",length(seq(yli[3],yhi[3], by=yii[3]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[3],yhi[3], by=yii[3]),at=seq(yli[3],yhi[3], by=yii[3]), side=2, line=xll,cex=axc,las=2)	
		mtext(expression(paste("(proportion soil proportion air"^"-1",")")),side=2,line=yll1,cex=mcx)
			mtext("Change in time of",side=2,line=yll3,cex=mcx)
			mtext("temperature minimum",side=2,line=yll2,cex=mcx)		
			mtext(datVI$name2,at=xseq, side=1, line=xll,cex=axc,las=2)

				
			
dev.off()		


			

				
#######################################
#####plot min/max intercept       ##### 
#######################################					


wd <- 80
hd <- 73


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(-35,0)
yhi <- c(10,25)
yii <- c(5,5)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 8
#size of x labels
axc <- 10
#line for label num
xll <- 2
#line for units
yll1 <- 20
#line for name
yll2 <- 33
#line for name
yll3 <- 46
#cex of axis label
mcx <- 13



png(paste0(plotDI,"\\run",Nrun,"\\intercepts_min_max2.png"), width=3000,height=4200,
			units="px")
	layout(matrix(seq(1,2),ncol=1), width=rep(lcm(wd),2),height=rep(lcm(hd),2))
		#plot intercept
	
		par(mai=c(1,6,15,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[1],yhi[1]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==1&beta0$vegeclass==j],beta0$X75.[beta0$regID==1&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==1&beta0$vegeclass==j],beta0$X25.[beta0$regID==1&beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.2.[beta0$regID==1&beta0$vegeclass==j],
						xseq[j],beta0$X99.8.[beta0$regID==1&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("(T"[min],","~degree~"C)")),side=2,line=yll1,cex=mcx)
			mtext("minimum",side=2,line=yll2,cex=mcx)
			mtext("Temperature",side=2,line=yll3,cex=mcx)
		par(mai=c(15,6,1,0),xpd=TRUE)
		
			plot(c(0,1),c(0,1), ylim=c(yli[2],yhi[2]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==2&beta0$vegeclass==j],beta0$X75.[beta0$regID==2&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==2&beta0$vegeclass==j],beta0$X25.[beta0$regID==2&beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.2.[beta0$regID==2&beta0$vegeclass==j],
						xseq[j],beta0$X99.8.[beta0$regID==2&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[2],yhi[2], by=yii[2]), rep(" ",length(seq(yli[2],yhi[2], by=yii[2]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[2],yhi[2], by=yii[2]),at=seq(yli[2],yhi[2], by=yii[2]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("(T"[max],","~degree~"C)")),side=2,line=yll1,cex=mcx)			
			mtext("maximum",side=2,line=yll2,cex=mcx)
			mtext("Temperature",side=2,line=yll3,cex=mcx)
					
			text(xseq,rep(-1,length(xseq)),datVI$name2,srt=35, adj=1,cex=axc,xpd=TRUE)

				
			
dev.off()	
	

#######################################
#####plot just air slope          ##### 
#######################################

wd <- 80
hd <- 73


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(-1,-1)
yhi <- c(1.5,2)
yii <- c(.5,.5)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 8
#size of x labels
axc <- 10
#line of x label
xll <- 2
#line for units
yll1 <- 20
#line for name
yll2 <- 33
#second line for name
yll3 <- 46
#second line for name
yll4 <- 59
#cex of axis label
mcx <- 13
#zero line
zlw <- 9



png(paste0(plotDI,"\\run",Nrun,"\\air_slopes_min_max2.png"), width=3500,height=4200,
			units="px")
	layout(matrix(seq(1,2),ncol=1), width=rep(lcm(wd),2),height=rep(lcm(hd),2))
		#plot intercept
	
		par(mai=c(1,6,15,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[1],yhi[1]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			points(c(xl,xh),c(0,0),type="l",lwd=zlw, col="grey75",lty=3)
			for(j in 1:9){
		
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==1&beta2$vegeclass==j],beta2$X75.[beta2$regID==1&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==1&beta2$vegeclass==j],beta2$X25.[beta2$regID==1&beta2$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				
				arrows(xseq[j]-1,beta2$Mean[beta2$regID==1&beta2$vegeclass==j],
						xseq[j]+1,beta2$Mean[beta2$regID==1&beta2$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta2$X0.2.[beta2$regID==1&beta2$vegeclass==j],
						xseq[j],beta2$X99.8.[beta2$regID==1&beta2$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			mtext(expression(paste("(",degree,"C soil",degree,"C air"^"-1",")")),side=2,line=yll1,cex=mcx)
			mtext("Change in",side=2,line=yll4,cex=mcx)
			mtext("temperature",side=2,line=yll3,cex=mcx)
			mtext("minimum",side=2,line=yll2,cex=mcx)
		par(mai=c(15,6,1,0),xpd=TRUE)
		
			plot(c(0,1),c(0,1), ylim=c(yli[2],yhi[2]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			points(c(xl,xh),c(1,1),type="l",lwd=zlw, col="grey75",lty=3)
			for(j in 1:9){
		
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta2$X25.[beta2$regID==2&beta2$vegeclass==j],beta2$X75.[beta2$regID==2&beta2$vegeclass==j],
							beta2$X75.[beta2$regID==2&beta2$vegeclass==j],beta2$X25.[beta2$regID==2&beta2$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
		
				arrows(xseq[j]-1,beta2$Mean[beta2$regID==2&beta2$vegeclass==j],
						xseq[j]+1,beta2$Mean[beta2$regID==2&beta2$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta2$X0.2.[beta2$regID==2&beta2$vegeclass==j],
						xseq[j],beta2$X99.8.[beta2$regID==2&beta2$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[2],yhi[2], by=yii[2]), rep(" ",length(seq(yli[2],yhi[2], by=yii[2]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[2],yhi[2], by=yii[2]),at=seq(yli[2],yhi[2], by=yii[2]), side=2, line=xll,cex=axc,las=2)	
		mtext(expression(paste("(",degree,"C soil",degree,"C air"^"-1",")")),side=2,line=yll1,cex=mcx)
			mtext("Change in",side=2,line=yll4,cex=mcx)
			mtext("temperature",side=2,line=yll3,cex=mcx)
			mtext(" maximum",side=2,line=yll2,cex=mcx)
					
			text(xseq,rep(-1.25,length(xseq)),datVI$name2,srt=35, adj=1,cex=axc,xpd=TRUE)
				
			
dev.off()		

		