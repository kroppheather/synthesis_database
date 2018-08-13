##########################################################
########Continuous vegetation soil temp        ###########
########Heather Kropp started July 2018        ###########
##########################################################
##########################################################
##########################################################
### This script uses vegetation cmeasures  in tundra   ###
### to look at patterns in                             ###
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

#load libraries
library(rjags)
library(coda)
library(mcmcplots)
library(RColorBrewer)

#set up directories
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\model\\run4"
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\plots"
Nrun <- 4
#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")

#read in continuous vege cover

datSP <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\spcov.csv")
#match datSP siteid name
colnames(datSP)[5] <- "siteid"

#read in lai/ndvi
datL <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\LAI.csv")

datM <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\moss.csv")
colnames(datM)[6] <- "siteid"

siteinfo <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\siteinfo.csv")
colnames(siteinfo)[1] <- "siteid"
#join to vegetation info so that only sites with vegetation info are included

vegeSP <- join(datV,datSP, by=c("siteid"), type="right")
vegeL <- join(datV,datL, by=c("siteid"), type="right")
vegeM <- join(datV, datM, by=c("siteid"), type="right")


#look at only non wetland tundra classes (1-5)
vegeSP <- vegeSP[vegeSP$vegeclass <=5,]
vegeL <- vegeL[vegeL$vegeclass <=5,]
vegeM <- vegeM[vegeM$vegeclass <=5,]
vegeSP <- vegeSP[vegeSP$vegeclass !=1,]
vegeL <- vegeSP[vegeL$vegeclass !=1,]
vegeM <- vegeSP[vegeM$vegeclass !=1,]
vegeSP <- vegeSP[vegeSP$vegeclass !=3,]
vegeL <- vegeSP[vegeL$vegeclass !=3,]
vegeM <- vegeSP[vegeM$vegeclass !=3,]


#get count of number of sites
length(unique(vegeSP$siteid))
length(unique(vegeL$siteid))
length(unique(vegeM$siteid))


#join siteinfo in
vegeSP <- join(vegeSP, siteinfo, by="siteid",type="left")
vegeL<- join(vegeL, siteinfo, by="siteid",type="left")
vegeM <- join(vegeM, siteinfo, by="siteid",type="left")
unique(vegeSP$loc)
unique(vegeSP$site_name)



unique(vegeL$loc)
unique(vegeL$site_name)


unique(vegeM$loc)
unique(vegeM$site_name)




#######################################
#####aggregate vegetation         ##### 
#######################################

#need to add up all vegetation covers and normalize ones that add up to over
#100%. THose are likely based on canopy ground cover rather than mix
#of species abundance
percT <- aggregate(vegeSP$perc_cov, by=list(vegeSP$siteid), FUN="sum")
colnames(percT) <- c("siteid","percTot")

vegeSP <- join(vegeSP, percT, by="siteid", type="left")

#normalize sites over 100 percent
vegeSP$perc_covN <- ifelse(vegeSP$percTot >= 100, (vegeSP$perc_cov/vegeSP$percTot)*100,vegeSP$perc_cov)

#total up species cover for each site

totC <- aggregate(vegeSP$perc_covN, by=list(vegeSP$func_type,vegeSP$siteid), FUN="sum")
colnames(totC) <- c("func_type","siteid", "percCN")
#grab any shrub


#get all shrub observations
shrub <- totC[grepl("shrub",totC$func_type)==TRUE,]
#summ up all shrub observations
shrubA <- aggregate(shrub$percCN, by=list(shrub$siteid), FUN="sum")
colnames(shrubA) <- c("siteid","shrubC")

nonvascular <- totC[grepl("moss",totC$func_type)==TRUE|grepl("lichen",totC$func_type)==TRUE|grepl("liverwort",totC$func_type)==TRUE,]

nonvascularA <-  aggregate(nonvascular$percCN, by=list(nonvascular$siteid), FUN="sum")
colnames(nonvascularA) <- c("siteid","nonvascularC")

grass <- totC[grepl("gramminoid",totC$func_type)==TRUE|grepl("sedges",totC$func_type)==TRUE|grepl("grasses",totC$func_type)==TRUE|grepl("rush",totC$func_type)==TRUE|grepl("Tall sedges",totC$func_type)==TRUE,]


grassA <-  aggregate(grass$percCN, by=list(grass$siteid), FUN="sum")
colnames(grassA) <- c("siteid","grassC")

#join all together

coverAll <- join(shrubA, grassA, by="siteid", type="full")
coverAll <- join(coverAll, nonvascularA, by="siteid", type="full")

#grass is missing a lot of observations.

coverAll2 <- join(shrubA, nonvascularA, by="siteid", type="full")
#three sites are missing shrub cover values
#four different sites are missing moss cover

#the shrub missing values are from a study where onlly plot contents were recorded so can assume not there
coverAll2$shrubC <- ifelse(is.na(coverAll2$shrubC),0,coverAll2$shrubC)

#sites 65, and 67 do not have cover estimates for moss
#site 101 does not have moss
#site 190 no moss

coverAll2$nonvascularC[coverAll2$siteid==101] <- 0
coverAll2$nonvascularC[coverAll2$siteid==190] <- 0


#omit sites with missing cover data
coverAll2 <- na.omit(coverAll2)



#######################################
#####organize LAI NDVI data       ##### 
#######################################

#get counts of ndvi and lai measurements across sites to get a feel for data
datLAI <- datL[is.na(datL$lai)==FALSE,]
countL <- aggregate(datLAI$lai, by=list(datLAI$siteid), FUN="length")

datNDVI <- datL[is.na(datL$ndvi)==FALSE,]
countN <- aggregate(datNDVI$ndvi, by=list(datNDVI$siteid), FUN="length")
colnames(countN) <- c("siteid", "ndviCount")
datNDVI <- join(datNDVI,countN, by="siteid",type="left")
#subset out sites with only one measurement
datNDVIS <- datNDVI[datNDVI$ndviCount==1,]


datNDVIM <- datNDVI[datNDVI$ndviCount>1,]

#only take average NDVI during a period where typical ndvi is measured
#at maximumal vegetation activity
#166-227
datNDVIMG <- datNDVIM[datNDVIM$doy_m>=166&datNDVIM$doy_m<=227,]
#now aggregate by site
datNDVIAV <- aggregate(datNDVIMG$ndvi, by=list(datNDVIMG$siteid),FUN="mean")
colnames(datNDVIAV) <- c("siteid","ndvi")


#make a dataframe with all of the measurements
NDVI <- data.frame(siteid=c(datNDVIS$siteid,datNDVIAV$siteid),
					ndvi=c(datNDVIS$ndvi,datNDVIAV$ndvi))
					

#######################################
#####organize soil data           ##### 
#######################################


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

#join each vegetation dataset to soil and air parameters

ParmPC <- join(ParmAll, coverAll2, by="siteid", type="inner")
ParmNDVI <- join(ParmAll, NDVI, by="siteid", type="inner")
ParmMLT <- join(ParmAll, datM, by="siteid", type="inner")

#need to join vegeclass data
ParmPC <- join(ParmPC, datV, by=c("siteid"), type="left")

#check how many observations in each vegeclass and regression
vegeCount <- aggregate(ParmPC$Mean, by=list(ParmPC$vegeclass,ParmPC$regID),FUN="length")
colnames(vegeCount) <- c("vegeclass","regID","count")
vegeCount$regvegeID <- seq(1,dim(vegeCount)[1])
#join back into parmpc
ParmPC <- join(ParmPC, vegeCount, by=c("vegeclass","regID"),type="left")



AirMean <- aggregate(ParmPC$AMean, by=list(ParmPC$regID), FUN="mean")
colnames(AirMean) <- c("regID", "Abar")

#generate dataset for monitoring regression means

mu.monitor <- data.frame(regID = rep(seq(1,3),each=100), 
				monitorAir = c(seq(-45,0, length.out=100),seq(0,35,length.out=100),seq(0,.65,length.out=100)),
				monitorDepth=rep(seq(0,20,length.out=100),times=3),
				monitorShrub=rep(seq(0,100,length.out=100),times=3),
				monitorMoss=rep(seq(0,80,length.out=100),times=3))
				
#######################################
#####read in model results        ##### 
#######################################	



#read in model results 

datM <- read.csv(paste0(modDI,"\\vege_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\vege_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))

datC$parms2 <- gsub("\\d","",datC$parms )



datB <- datC[datC$parms2=="beta",]
datB$regID <- rep(vegeCount$regID,times=5)
datB$vegeID <- rep(vegeCount$vegeclass,times=5)
datB$parmID <- rep(seq(1,5),each=9)

#create significance column

datB$sigP <- ifelse(datB$X0.1.<0&datB$X99.9.<0,1,
			ifelse(datB$X0.1.>0&datB$X99.9.>0,1,0))
			
			
					
#######################################
##### make a figure               ##### 
##### comparing parameters        ##### 
#######################################
#unique vege type for plotting
vegePL <- data.frame(vegeclass=unique(datB$vegeID))
vegePL <- join(vegePL, datVI, by="vegeclass", type="left")

wd <- 40
hd <- 40

#make a panel of parameters for each regression


#x sequence
xseq <-c(1,4,7)
#axis limits
xl <- -1
xh <- 9
yl1 <- c(-25,0,.4)
yh1 <- c(0,25,.7)
yl2 <- c(-0.5,-1,-0.01)
yh2 <- c(1,0.5,0.02)
yl3 <- c(-0.2,-0.1,-0.1)
yh3 <- c(1.2,1.2,1)
yl4 <- c(-0.5,-0.5,-0.004)
yh4 <- c(0.5,0.5,0.004)
yl5 <- c(-0.2,-0.2,-0.003)
yh5 <- c(0.1,0.1,0.001)

#axis sequence
yi1 <- c(5,5,.1)
yi2 <- c(.1,.1,.002)
yi3 <- c(.2,.2,.1)
yi4 <- c(.1,.1,.001)
yi5 <- c(.05,.05,.001)

#arrow width
alw <- 5
#mean bar width
mlw <- 7
#zero line width
zlw <- 7

#y margin label
ymx <- 5

#axis label size
ax <- 5
#axis tick width
tx <- 5
#y axis line
yll <- 5 
#x axis line
xll <- 5

scol <- "tomato3"
nscol <- "grey75"
zcol <- "grey50"

#three regressions
regName <- c("Soil min vs air min","Soil max vs air max", "Time of soil min vs time of air min")


for(i in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_parm",i,".jpg"), width=6000,height=3000,
			quality=100,units="px")
			
		layout(matrix(seq(1,5),ncol=5), width=rep(lcm(wd),5),height=rep(lcm(hd),5))
			#intercept
			par(mai=c(2,2,2,2))
			
				plot(c(0,1),c(0,1), xlim=c(xl,xh), ylim=c(yl1[i],yh1[i]), axes=FALSE, xlab=" ", ylab=" ",
					type="n", xaxs="i",yaxs="i")
				
				
				
				arrows(xseq,datB$X0.1.[datB$regID==i&datB$parmID==1],xseq,datB$X99.9.[datB$regID==i&datB$parmID==1],
						code=0,lwd=alw)
				for(j in 1:dim(vegePL)[1]){
					if(datB$sigP[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1]==1){
						polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1]),
							col=scol, border=NA)

				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==1]),
							col=nscol, border=NA)
				
				
					}
				}
				arrows(xseq-1,datB$Mean[datB$regID==i&datB$parmID==1],xseq+1,datB$Mean[datB$regID==i&datB$parmID==1],
					code=0, lwd=mlw)
					
			axis(2,seq(yl1[i],yh1[i], by=yi1[i]), rep(" ",length(seq(yl1[i],yh1[i], by=yi1[i]))), lwd.ticks=tx)
			mtext(seq(yl1[i],yh1[i], by=yi1[i]), at=seq(yl1[i],yh1[i], by=yi1[i]),cex=ax,line=yll,side=2,las=2)
			
			axis(1, xseq,rep(" ",length(xseq)), lwd.ticks=tx)
			mtext(vegePL$vegename,at=xseq,cex=ax,line=xll,side=1,las=2)
			

			mtext("Intercept", side=3, line=xll,cex=ymx)
			mtext("Vegetation type", side=1, outer=TRUE,line=-15,cex=10)
			mtext(regName[i], side=3, outer=TRUE,line=-40,cex=10)
			
			box(which="plot")
			#depth
			par(mai=c(2,2,2,2))
			
				plot(c(0,1),c(0,1), xlim=c(xl,xh), ylim=c(yl2[i],yh2[i]), axes=FALSE, xlab=" ", ylab=" ",
					type="n", xaxs="i",yaxs="i")
					
				abline(h=0,lwd=zlw,lty=3,col=zcol)
				arrows(xseq,datB$X0.1.[datB$regID==i&datB$parmID==2],xseq,datB$X99.9.[datB$regID==i&datB$parmID==2],
						code=0,lwd=alw)
				for(j in 1:dim(vegePL)[1]){
					if(datB$sigP[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2]==1){
						polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2]),
							col=scol, border=NA)

				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==2]),
							col=nscol, border=NA)
				
				
					}
				}
				arrows(xseq-1,datB$Mean[datB$regID==i&datB$parmID==2],xseq+1,datB$Mean[datB$regID==i&datB$parmID==2],
					code=0, lwd=mlw)
			
			
			axis(2,seq(yl2[i],yh2[i], by=yi2[i]), rep(" ",length(seq(yl2[i],yh2[i], by=yi2[i]))), lwd.ticks=tx)
			mtext(seq(yl2[i],yh2[i], by=yi2[i]), at=seq(yl2[i],yh2[i], by=yi2[i]),cex=ax,line=yll,side=2,las=2)
			axis(1, xseq,rep(" ",length(xseq)), lwd.ticks=tx)
			mtext(vegePL$vegename,at=xseq,cex=ax,line=xll,side=1,las=2)
			mtext("Depth", side=3, line=xll,cex=ymx)
			box(which="plot")	
			#air temperature
			par(mai=c(2,2,2,2))
			
				plot(c(0,1),c(0,1), xlim=c(xl,xh), ylim=c(yl3[i],yh3[i]), axes=FALSE, xlab=" ", ylab=" ",
					type="n", xaxs="i",yaxs="i")
				abline(h=0,lwd=zlw,lty=3,col=zcol)
				arrows(xseq,datB$X0.1.[datB$regID==i&datB$parmID==3],xseq,datB$X99.9.[datB$regID==i&datB$parmID==3],
						code=0,lwd=alw)
				for(j in 1:dim(vegePL)[1]){
					if(datB$sigP[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3]==1){
						polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3]),
							col=scol, border=NA)

				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==3]),
							col=nscol, border=NA)
				
				
					}
				}
				arrows(xseq-1,datB$Mean[datB$regID==i&datB$parmID==3],xseq+1,datB$Mean[datB$regID==i&datB$parmID==3],
					code=0, lwd=mlw)
			
			
			axis(2,seq(yl3[i],yh3[i], by=yi3[i]), rep(" ",length(seq(yl3[i],yh3[i], by=yi3[i]))), lwd.ticks=tx)
			mtext(seq(yl3[i],yh3[i], by=yi3[i]), at=seq(yl3[i],yh3[i], by=yi3[i]),cex=ax,line=yll,side=2,las=2)
			axis(1, xseq,rep(" ",length(xseq)), lwd.ticks=tx)
			mtext(vegePL$vegename,at=xseq,cex=ax,line=xll,side=1,las=2)
			mtext("Air", side=3, line=xll,cex=ymx)
			box(which="plot")	
			#shrub
			par(mai=c(2,2,2,2))
			
				plot(c(0,1),c(0,1), xlim=c(xl,xh), ylim=c(yl4[i],yh4[i]), axes=FALSE, xlab=" ", ylab=" ",
					type="n", xaxs="i",yaxs="i")
				abline(h=0,lwd=zlw,lty=3,col=zcol)	
				arrows(xseq,datB$X0.1.[datB$regID==i&datB$parmID==4],xseq,datB$X99.9.[datB$regID==i&datB$parmID==4],
						code=0,lwd=alw)
				for(j in 1:dim(vegePL)[1]){
					if(datB$sigP[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4]==1){
						polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4]),
							col=scol, border=NA)

				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==4]),
							col=nscol, border=NA)
				
				
					}
				}
				arrows(xseq-1,datB$Mean[datB$regID==i&datB$parmID==4],xseq+1,datB$Mean[datB$regID==i&datB$parmID==4],
					code=0, lwd=mlw)	
					
					
			axis(2,seq(yl4[i],yh4[i], by=yi4[i]), rep(" ",length(seq(yl4[i],yh4[i], by=yi4[i]))), lwd.ticks=tx)
			mtext(seq(yl4[i],yh4[i], by=yi4[i]), at=seq(yl4[i],yh4[i], by=yi4[i]),cex=ax,line=yll,side=2,las=2)
			axis(1, xseq,rep(" ",length(xseq)), lwd.ticks=tx)
			mtext(vegePL$vegename,at=xseq,cex=ax,line=xll,side=1,las=2)
			mtext("% shrub cover", side=3, line=xll,cex=ymx)
			box(which="plot")
			#moss
			par(mai=c(2,2,2,2))
			
				plot(c(0,1),c(0,1), xlim=c(xl,xh), ylim=c(yl5[i],yh5[i]), axes=FALSE, xlab=" ", ylab=" ",
					type="n", xaxs="i",yaxs="i")
				abline(h=0,lwd=zlw,lty=3,col=zcol)
				arrows(xseq,datB$X0.1.[datB$regID==i&datB$parmID==5],xseq,datB$X99.9.[datB$regID==i&datB$parmID==5],
						code=0,lwd=alw)
				for(j in 1:dim(vegePL)[1]){
					if(datB$sigP[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5]==1){
						polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5]),
							col=scol, border=NA)

				}else{
					polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
							c(datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5],
								datB$X75.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5],
								datB$X25.[datB$vegeID==vegePL$vegeclass[j]&datB$regID==i&datB$parmID==5]),
							col=nscol, border=NA)
				
				
					}
				}
				arrows(xseq-1,datB$Mean[datB$regID==i&datB$parmID==5],xseq+1,datB$Mean[datB$regID==i&datB$parmID==5],
					code=0, lwd=mlw)	
			
			axis(2,seq(yl5[i],yh5[i], by=yi5[i]), rep(" ",length(seq(yl5[i],yh5[i], by=yi5[i]))), lwd.ticks=tx)
			mtext(seq(yl5[i],yh5[i], by=yi5[i]), at=seq(yl5[i],yh5[i], by=yi5[i]),cex=ax,line=yll,side=2,las=2)			
			axis(1, xseq,rep(" ",length(xseq)), lwd.ticks=tx)
			mtext(vegePL$vegename,at=xseq,cex=ax,line=xll,side=1,las=2)
			mtext("% moss cover", side=3, line=xll,cex=ymx)
			box(which="plot")
		
	dev.off()		
}			



