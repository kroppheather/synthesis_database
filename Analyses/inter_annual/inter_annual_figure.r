##########################################################
########Vegetation interannual soil temp       ###########
########Heather Kropp started August 2018      ###########
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
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\plots\\model"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run12"
Nrun <- 12
#indicate if a model run is occuring
modRun <- 1


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



#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#get number of years for each site and depth

YearAll <- unique(data.frame(siteid=ParmAll$siteid,depth=ParmAll$depth,wyear=ParmAll$wyear))
#count the number of years for each site and depth
YearCount <- aggregate(YearAll$wyear, by=list(YearAll$depth,YearAll$siteid), FUN="length")
colnames(YearCount) <- c("depth","siteid","nYear")
#subset sites with at least 6 years
YearSub <- YearCount[YearCount$nYear >=2,]

#join vegeclass to see how many
YearSub <- join(YearSub,datV, by="siteid",type="left")
#find out number of sites in each vegeclass
SubsiteN <- unique(data.frame(siteid=YearSub$siteid,vegeclass=YearSub$vegeclass))
SubsiteN <- SubsiteN[order(SubsiteN$vegeclass),]
SubdepthN <-  unique(data.frame(depth=YearSub$depth,vegeclass=YearSub$vegeclass))
SubdepthN <- SubdepthN[order(SubdepthN$vegeclass),]

#find out how many sites in each vegeclass
siteNl <- aggregate(SubsiteN$siteid, by=list(SubsiteN$vegeclass), FUN="length")
colnames(siteNl) <- c("vegeclass","count")
#most vegeclasses only have 1-3 sites with at least 3 years of data
#need to subset vege classes to focus on ones with more sites
#subset so that there are at least 5 different sites in a vegeclass
siteNl <- siteNl[siteNl$count>=5,]
#join back into Yearsub
YearSub <- join(YearSub, siteNl, by="vegeclass", type="inner")
#make a dataframe with only info needed for joining
YearSub2 <- data.frame(depth=YearSub$depth, siteid=YearSub$siteid,vegeclass=YearSub$vegeclass)
#subset ParmAll to only focus on interannual sites
ParmAlls <- join(ParmAll, YearSub2, by=c("depth","siteid"), type="inner") 


#organize previous soil components
#organize past soil maximum focusing on past 4 years
Tmax <- SoilParm[SoilParm$parm=="TmaxS",]


#matching for 1 year into the past
Tmax1 <- Tmax
Tmax1$wyear <- Tmax1$wyear+1
colnames(Tmax1)[1:4] <- paste0(colnames(Tmax1)[1:4],"Max1")


#join to ParmAlls

ParmAlls1 <- join(ParmAlls,Tmax1, by=c("siteid","wyear","depth"),type="left")



#organize past soil maximum focusing on past 4 years
Tmin <- SoilParm[SoilParm$parm=="TminS",]
#matching same year for Tmax model
Tmin0 <- Tmin
Tmin0$wyear <- Tmin0$wyear
colnames(Tmin0)[1:4] <- paste0(colnames(Tmin0)[1:4],"MinT0")
#matching previous year 
Tmin1 <- Tmin
Tmin1$wyear <- Tmin1$wyear+1
colnames(Tmin1)[1:4] <- paste0(colnames(Tmin1)[1:4],"MinT1")


#join to Parms

#first get the temp years joined
ParmAlls2 <- join(ParmAlls1, Tmin0, by=c("siteid","wyear","depth"), type="left")
ParmAlls3 <- join(ParmAlls2, Tmin1, by=c("siteid","wyear","depth"), type="left")




ParmAlls3$MeanMin1 <- ifelse(ParmAlls3$regID==2,ParmAlls3$MeanMinT0,ParmAlls3$MeanMinT1)
ParmAlls3$SDMin1 <- ifelse(ParmAlls3$regID==2,ParmAlls3$SDMinT0,ParmAlls3$SDMinT1)
#omit any data with NA because that means there aren't enough preceding years
ParmAlls3 <- na.omit(ParmAlls3)

#make regVege table
regVegeDF <- unique(data.frame(regID=ParmAlls3$regID,vegeclass=ParmAlls3$vegeclass)) 
regVegeDF$regvegeID <- seq(1,dim(regVegeDF)[1])

#join into dataframe
ParmAlls4 <- join(ParmAlls3,regVegeDF, by=c("regID","vegeclass"), type="left")

#calculate average air temp in each regression
airTempCurrentm <- aggregate(ParmAlls4$AMean,by=list(ParmAlls4$regID),FUN="mean")

colnames(airTempCurrentm) <- c("regID","meanA")

pastMaxave <- aggregate(ParmAlls4$MeanMax1,by=list(ParmAlls4$regID),FUN="mean")
colnames(pastMaxave) <- c("regID","meanMax")

pastMinave <- aggregate(ParmAlls4$MeanMin1,by=list(ParmAlls4$regID),FUN="mean")
colnames(pastMinave) <- c("regID","meanMin")



#######################################
#####read in model results        ##### 
#######################################	



#read in model results 

datM <- read.csv(paste0(modDI,"\\inter_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\inter_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))



#subset parms

beta0 <- datC[datC$parms=="beta0",]
beta1 <- datC[datC$parms=="beta1",]
beta2 <- datC[datC$parms=="beta2",]
beta3 <- datC[datC$parms=="beta3",]
beta4 <- datC[datC$parms=="beta4",]


#add identifying info to results
beta0 <- cbind(beta0,regVegeDF)
beta1 <- cbind(beta1,regVegeDF)
beta2 <- cbind(beta2,regVegeDF)
beta3 <- cbind(beta3,regVegeDF)
beta4 <- cbind(beta4,regVegeDF)



#turn beta into a list
betaOut <- list(beta0,beta1,beta2,beta3,beta4)

#create significance ID on each dataframe
for(i in 1:5){
	betaOut[[i]]$sigP <- ifelse(betaOut[[i]]$X0.3.<0&betaOut[[i]]$X99.7.<0,1,
			ifelse(betaOut[[i]]$X0.3.>0&betaOut[[i]]$X99.7.>0,1,0))
}



#######################################
#####panel comparing regression   ##### 
#####parms                        #####
#######################################	
#make a panel for comparing regression coefficients

#######################################
#####plot parameter comparison   ##### 
#######################################

#create vegeclass df
vegeSub <- data.frame(vegeclass=unique(regVegeDF$vegeclass))
vegeSub <- join(vegeSub,datVI, by="vegeclass", type="left")
vegeSub <- vegeSub[order(vegeSub$vegeclass),]


wd <- 45
hd <- 40


#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16)

yli <- c(-20,0,0.3)
yhi <- c(0,15,.65)
yls1 <- c(-1.5,-.5,-.02)
yhs1 <- c(2,.1,.02)
yls2 <- c(-.2,-.2,-.2)
yhs2 <- c(.8,1.4,1.4)
yls3 <- c(-.4,-.2,-.015)
yhs3 <- c(1,1.2,.015)
yls4 <- c(-.2,-.2,-.01)
yhs4 <- c(1.2,.4,.01)

xl <- -1
xh <- 18
alw <- 2
zlw <- 10
mlw <- 5
#axis labels
tlw <- 4
alx <- 4
mlx <- 7

yii <- c(5,5,.1)
yi1 <- c(.5,.1,.005)
yi2 <- c(.2,.2,.2)
yi3 <- c(.2,.2,.005)
yi4 <- c(.2,.2,.002)
#three regressions
regName <- c("Soil min vs air min","Soil max vs air max", "Time of soil min vs time of air min")


for(i in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_parm",i,".jpg"), width=7500,height=2500,
			quality=100,units="px")
	layout(matrix(seq(1,5),ncol=5), width=rep(lcm(wd),5),height=rep(lcm(hd),5))
		#plot intercept
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
					for(j in 1:dim(vegeSub)[1]){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[1]]$X25.[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[1]]$X75.[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[1]]$X75.[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[1]]$X25.[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]]),
						col="tomato3",border=NA)
				arrows(xseq[j]-1,betaOut[[1]]$Mean[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j]+1,betaOut[[1]]$Mean[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],code=0,lwd=mlw)
				arrows(	xseq[j],betaOut[[1]]$X0.3.[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j],betaOut[[1]]$X99.7.[betaOut[[1]]$regID==i&betaOut[[1]]$vegeclass==vegeSub$vegeclass[j]],
						code=0, lwd=alw)
			}
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(vegeSub$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yli[i],yhi[i], by=yii[i]),rep("",length(seq(yli[i],yhi[i], by=yii[i]))), lwd.ticks=tlw)
		mtext(seq(yli[i],yhi[i], by=yii[i]),at=seq(yli[i],yhi[i], by=yii[i]),cex=alx,line=3,las=2,side=2)
		mtext("Intercept", side=3, line=5, cex=mlx)
		mtext(paste(regName[i]), side=3, outer=TRUE,line=-40,cex=12)
		box(which="plot")		
		#plot depth
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls1[i],yhs1[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		abline(h=0,	lwd	=zlw, col="grey75",lty=3)			
			for(j in 1:dim(vegeSub)[1]){
			if(betaOut[[2]]$sigP[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[2]]$X25.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[2]]$X75.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[2]]$X75.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[2]]$X25.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[2]]$X25.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[2]]$X75.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[2]]$X75.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[2]]$X25.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,betaOut[[2]]$Mean[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j]+1,betaOut[[2]]$Mean[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],code=0,lwd=mlw)
				arrows(	xseq[j],betaOut[[2]]$X0.3.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j],betaOut[[2]]$X99.7.[betaOut[[2]]$regID==i&betaOut[[2]]$vegeclass==vegeSub$vegeclass[j]],
						code=0, lwd=alw)
			}			
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(vegeSub$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yls1[i],yhs1[i], by=yi1[i]),rep("",length(seq(yls1[i],yhs1[i], by=yi1[i]))), lwd.ticks=tlw)
		mtext(seq(yls1[i],yhs1[i], by=yi1[i]),at=seq(yls1[i],yhs1[i], by=yi1[i]),cex=alx,line=3,las=2,side=2)	
		mtext("Slope depth", side=3, line=5, cex=mlx)
		box(which="plot")			
		#plot currrent air
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls2[i],yhs2[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		abline(h=0,	lwd	=zlw, col="grey75",lty=3)			
			for(j in 1:dim(vegeSub)[1]){
			if(betaOut[[3]]$sigP[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[3]]$X25.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[3]]$X75.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[3]]$X75.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[3]]$X25.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[3]]$X25.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[3]]$X75.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[3]]$X75.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[3]]$X25.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,betaOut[[3]]$Mean[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j]+1,betaOut[[3]]$Mean[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],code=0,lwd=mlw)
				arrows(	xseq[j],betaOut[[3]]$X0.3.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j],betaOut[[3]]$X99.7.[betaOut[[3]]$regID==i&betaOut[[3]]$vegeclass==vegeSub$vegeclass[j]],
						code=0, lwd=alw)
			}			
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(vegeSub$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yls2[i],yhs2[i], by=yi2[i]),rep("",length(seq(yls2[i],yhs2[i], by=yi2[i]))), lwd.ticks=tlw)
		mtext(seq(yls2[i],yhs2[i], by=yi2[i]),at=seq(yls2[i],yhs2[i], by=yi2[i]),cex=alx,line=3,las=2,side=2)	
		mtext("Slope Air", side=3, line=5, cex=mlx)
		box(which="plot")	
		#plot past max soil
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls3[i],yhs3[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		abline(h=0,	lwd	=zlw, col="grey75",lty=3)			
			for(j in 1:dim(vegeSub)[1]){
			if(betaOut[[4]]$sigP[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[4]]$X25.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[4]]$X75.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[4]]$X75.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[4]]$X25.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[4]]$X25.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[4]]$X75.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[4]]$X75.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[4]]$X25.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,betaOut[[4]]$Mean[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j]+1,betaOut[[4]]$Mean[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],code=0,lwd=mlw)
				arrows(	xseq[j],betaOut[[4]]$X0.3.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j],betaOut[[4]]$X99.7.[betaOut[[4]]$regID==i&betaOut[[4]]$vegeclass==vegeSub$vegeclass[j]],
						code=0, lwd=alw)
			}			
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(vegeSub$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yls3[i],yhs3[i], by=yi3[i]),rep("",length(seq(yls3[i],yhs3[i], by=yi3[i]))), lwd.ticks=tlw)
		mtext(seq(yls3[i],yhs3[i], by=yi3[i]),at=seq(yls3[i],yhs3[i], by=yi3[i]),cex=alx,line=3,las=2,side=2)	
		mtext("Slope Past soil max", side=3, line=5, cex=mlx)
		box(which="plot")	

		#plot past min soil
		par(mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), ylim=c(yls4[i],yhs4[i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		abline(h=0,	lwd	=zlw, col="grey75",lty=3)			
			for(j in 1:dim(vegeSub)[1]){
			if(betaOut[[5]]$sigP[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]]==1){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[5]]$X25.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[5]]$X75.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[5]]$X75.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[5]]$X25.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]]),
						col="tomato3",border=NA)
			}else{
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(betaOut[[5]]$X25.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
						betaOut[[5]]$X75.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[5]]$X75.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
							betaOut[[5]]$X25.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]]),
						col="grey75",border=NA)
				}
				arrows(xseq[j]-1,betaOut[[5]]$Mean[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j]+1,betaOut[[5]]$Mean[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],code=0,lwd=mlw)
				arrows(	xseq[j],betaOut[[5]]$X0.3.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
						xseq[j],betaOut[[5]]$X99.7.[betaOut[[5]]$regID==i&betaOut[[5]]$vegeclass==vegeSub$vegeclass[j]],
						code=0, lwd=alw)
			}			
		axis(1, xseq,rep("",length(xseq)), lwd.ticks=tlw)
		mtext(vegeSub$vegename,at=xseq,cex=alx,line=3,las=2,side=1)
		axis(2,seq(yls4[i],yhs4[i], by=yi4[i]),rep("",length(seq(yls4[i],yhs4[i], by=yi4[i]))), lwd.ticks=tlw)
		mtext(seq(yls4[i],yhs4[i], by=yi4[i]),at=seq(yls4[i],yhs4[i], by=yi4[i]),cex=alx,line=3,las=2,side=2)	
		mtext("Slope Past soil min", side=3, line=5, cex=mlx)
		box(which="plot")
	dev.off()
	
}	








#######################################
#####plot goodness of fit         ##### 
#######################################

reps <- datC[datC$parms=="repSoilP",]
reps$regID <- ParmAlls4$regID


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
	plot(ParmAlls4$Mean[ParmAlls4$regID==i], reps$Mean[reps$regID==i], xlim=c(ll[i],hh[i]), ylim=c(ll[i],hh[i]),
			xlab=lname[i],ylab=lname[i],pch=19)
	fit <- lm(reps$Mean[reps$regID==i]~ParmAlls4$Mean[ParmAlls4$regID==i])
	abline(fit, lwd=2,lty=3)
	abline(0,1,lwd=2,col="red")
	text(tx1[i],ty1[i], paste("y=",round(summary(fit)$coefficients[1,1],2),
				"+",round(summary(fit)$coefficients[2,1],2),"x"), col="red",cex=2)
	
	text(tx2[i],ty2[i], paste("R2=", round(summary(fit)$r.squared,2)), col="red",cex=2)
	dev.off()
}