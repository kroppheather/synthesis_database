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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\model\\run1"
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\plots"
Nrun <- 1
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


#now see how many years and sites are actually in each regression
PCcount <- aggregate(ParmPC$Mean, by=list(ParmPC$siteid,ParmPC$regID),FUN="length")
NDcount <- aggregate(ParmNDVI$Mean, by=list(ParmNDVI$siteid,ParmNDVI$regID),FUN="length")
MLTcount <- aggregate(ParmMLT$Mean, by=list(ParmMLT$siteid,ParmMLT$regID),FUN="length")

colnames(PCcount) <- c("siteid","regID","count")
colnames(NDcount) <- c("siteid","regID","count")
colnames(MLTcount) <- c("siteid","regID","count")

PCcount <- join(PCcount,datV, by="siteid",type="left")
NDcount <- join(NDcount,datV, by="siteid",type="left")
MLTcount <- join(MLTcount,datV, by="siteid",type="left")
#subset MLT so only looking in tundra
MLTcount <- MLTcount[MLTcount$vegeclass<6,]

PCcount[PCcount$regID==1,]

PCcount[PCcount$regID==1&PCcount$count>1,]
dim(PCcount[PCcount$regID==1&PCcount$count>1,])
NDcount[NDcount$regID==1&NDcount$count>1,]
MLTcount[MLTcount$regID==1&MLTcount$count>1,]
dim(NDcount[NDcount$regID==1&NDcount$count>1,])
dim(MLTcount[MLTcount$regID==1&MLTcount$count>1,])

#these will be dramatically smaller coverages in the tundra but worth investigating

PCcount <- join(PCcount,siteinfo, by="siteid",type="left")
NDcount <- join(NDcount,siteinfo, by="siteid",type="left")
MLTcount <- join(MLTcount,siteinfo, by="siteid",type="left")


#subset the %cover to have sites with more than 1 depth and year observation
PCcount <- PCcount[PCcount$count>2,]
#create regID 
PCcount$regsiteID <- seq(1,dim(PCcount)[1])
#join cover data for regressions
PCdata <- join(PCcount, coverAll2, by="siteid", type="left")


#join the regression site id back into ParmPC
#first make a smaller dataframe to not join tomuch
PCIDj <- data.frame(regsiteID=PCcount$regsiteID,regID=PCcount$regID, siteid=PCcount$siteid)

ParmPC <- join(ParmPC, PCIDj, by=c("siteid","regID"), type="inner")


AirMean <- aggregate(ParmPC$AMean, by=list(ParmPC$regID), FUN="mean")
colnames(AirMean) <- c("regID", "Abar")

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

datA <- datC[datC$parms2=="a",]
datA$regID <- rep(seq(1,3),times=3)
datA$parmID <- rep(seq(1,3),eac=3)

datB <- datC[datC$parms2=="b",]
datB$regID <- rep(seq(1,3),times=3)
datB$parmID <- rep(seq(1,3),eac=3)

datD <- datC[datC$parms2=="c",]
datD$regID <- rep(seq(1,3),times=3)
datD$parmID <- rep(seq(1,3),eac=3)
parmR <- list(datA,datB,datD)

beta0 <- datC[datC$parms=="beta0",]
beta1 <- datC[datC$parms=="beta1",]
beta2 <- datC[datC$parms=="beta2",]
#add identifying information onto betas
beta0 <- cbind(beta0,PCdata)
beta1 <- cbind(beta1,PCdata)
beta2 <- cbind(beta2,PCdata)

#######################################
#####look at the regression parms ##### 
#######################################
wd <- 30
hd <- 35

parmL <- c("a","b","c")

xl <- 0
xh <- 3
xs <- 1.5

yrA <- c(.5,.5,.005,.005,.005,.005,.005,.005,.005)
yrB <- c(.1,.1,.005,.005,.005,.005,.005,.005,.005)
yrD <- c(.5,.5,.5,.005,.005,.005,.005,.005,.005)


ylA <- data.frame(yl=round_any(datA$X0.2.,yrA,floor),parmID=datA$parmID,regID=datA$regID,yi=yrA)
ylB <- data.frame(yl=round_any(datB$X0.2.,yrB,floor),parmID=datB$parmID,regID=datB$regID,yi=yrB)
ylD <- data.frame(yl=round_any(datD$X0.2.,yrD,floor),parmID=datD$parmID,regID=datD$regID,yi=yrD)

yhA <- data.frame(yh=round_any(datA$X99.8.,yrA,ceiling),parmID=datA$parmID,regID=datA$regID)
yhB <- data.frame(yh=round_any(datB$X99.8.,yrB,ceiling),parmID=datB$parmID,regID=datB$regID)
yhD <- data.frame(yh=round_any(datD$X99.8.,yrD,ceiling),parmID=datD$parmID,regID=datD$regID)

yl <- list(ylA,ylB,ylD)
yh <- list(yhA,yhB,yhD)

parmName <- c("intercept","slope Shrub","slope moss")
regName <- c("Soil min vs air min","Soil max vs air max", "Time of soil min vs time of air min")
typeName <- c("intercept", "depth slope","air slope")
alw <- 3
mlw <- 3
#regression
for(i in 1:3){
	#parameter type
	for(m in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression",i,"_parm_",parmL[m],".jpg"), width=3000,height=2000,
			quality=100,units="px")
	layout(matrix(seq(1,3),ncol=3), width=rep(lcm(wd),3),height=rep(lcm(hd),3))
		
	#parameter number	
	for(j in 1:3){	
		par(mai=c(1,1,1,1))
			plot(c(0,1),c(0,1), ylim=c(yl[[m]]$yl[yl[[m]]$parmID==j&yl[[m]]$regID==i],
				yh[[m]]$yh[yh[[m]]$parmID==j&yh[[m]]$regID==i]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			abline(h=0, lwd=5,col="grey75",lty=3)	
			arrows(	xs,parmR[[m]]$X0.2.[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],
					xs,parmR[[m]]$X99.8.[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],code=0,lwd=alw)
			polygon(c(xs-1,xs-1,xs+1,xs+1),
				c(parmR[[m]]$X25.[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],
				parmR[[m]]$X75.[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],
				parmR[[m]]$X75.[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],
				parmR[[m]]$X25.[parmR[[m]]$parmID==j&parmR[[m]]$regID==i]),border=NA,col="tomato3")
			arrows(	xs-1,parmR[[m]]$Mean[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],
					xs+1,parmR[[m]]$Mean[parmR[[m]]$parmID==j&parmR[[m]]$regID==i],code=0,lwd=mlw)	
		
		axis(2,seq(	yl[[m]]$yl[yl[[m]]$parmID==j&yl[[m]]$regID==i],
				yh[[m]]$yh[yh[[m]]$parmID==j&yh[[m]]$regID==i], by=yl[[m]]$yi[yl[[m]]$parmID==j&yl[[m]]$regID==i]),
				cex.axis=5,las=2)
		axis(1, c(-1,xs,5), c("","",""), cex.axis=5)		
		mtext(paste(parmName[j]),side=1,line=5,cex=4)
		if(j==1){
		mtext(paste("regression: ",regName[i],"regression parameter: ",typeName[m]),side=3,line=-20,outer=TRUE,cex=7)
		}
				
	}		
			
					
	dev.off()			
		
	}
}		
				
				
				
#######################################
#####plot at the regression       ##### 
#######################################		

#get the range of the % cover
range(PCdata$shrubC)
range(PCdata$nonvascularC)	
#highest value is 91 in shrub and 77 in moss
#cap at 100 and 80
ycs <- seq(1,9)

colS <- brewer.pal(9,"Blues")
colM <- brewer.pal(9,"Reds")

colS <- colS[-1]
colM <- colM[-1]

redM <- numeric(0)
greenM <- numeric(0)
blueM <- numeric(0)
redS <- numeric(0)
greenS <- numeric(0)
blueS <- numeric(0)
for(i in 1:8){
	redM[i] <- col2rgb(colM[i])[1,1]
	greenM[i] <- col2rgb(colM[i])[2,1]
	blueM[i] <- col2rgb(colM[i])[3,1]
	redS[i] <- col2rgb(colS[i])[1,1]
	greenS[i] <- col2rgb(colS[i])[2,1]
	blueS[i] <- col2rgb(colS[i])[3,1]
}

#assign colors based on percentage
beta0$colShrub <- ifelse(beta0$shrubC<=12.5,colS[1],
					ifelse(beta0$shrubC>12.5&beta0$shrubC<=25,colS[2],
					ifelse(beta0$shrubC>25&beta0$shrubC<=37.5,colS[3],
					ifelse(beta0$shrubC>37.5&beta0$shrubC<=50,colS[4],
					ifelse(beta0$shrubC>50&beta0$shrubC<=67.5,colS[5],
					ifelse(beta0$shrubC>67.5&beta0$shrubC<=75,colS[6],
					ifelse(beta0$shrubC>75&beta0$shrubC<=87.5,colS[7],
					ifelse(beta0$shrubC>87.5&beta0$shrubC<100,colS[8],rgb(0,0,0)))))))))
					
beta0$colMoss <- ifelse(beta0$nonvascularC<=10,colM[1],
					ifelse(beta0$nonvascularC>10&beta0$nonvascularC<=20,colM[2],
					ifelse(beta0$nonvascularC>20&beta0$nonvascularC<=30,colM[3],
					ifelse(beta0$nonvascularC>30&beta0$nonvascularC<=40,colM[4],
					ifelse(beta0$nonvascularC>40&beta0$nonvascularC<=50,colM[5],
					ifelse(beta0$nonvascularC>50&beta0$nonvascularC<=60,colM[6],
					ifelse(beta0$nonvascularC>60&beta0$nonvascularC<=70,colM[7],
					ifelse(beta0$nonvascularC>70&beta0$nonvascularC<80,colM[8],rgb(0,0,0)))))))))					
					
beta1$colShrub <- ifelse(beta1$shrubC<=12.5,colS[1],
					ifelse(beta1$shrubC>12.5&beta1$shrubC<=25,colS[2],
					ifelse(beta1$shrubC>25&beta1$shrubC<=37.5,colS[3],
					ifelse(beta1$shrubC>37.5&beta1$shrubC<=50,colS[4],
					ifelse(beta1$shrubC>50&beta1$shrubC<=67.5,colS[5],
					ifelse(beta1$shrubC>67.5&beta1$shrubC<=75,colS[6],
					ifelse(beta1$shrubC>75&beta1$shrubC<=87.5,colS[7],
					ifelse(beta1$shrubC>87.5&beta1$shrubC<100,colS[8],rgb(0,0,0)))))))))
					
beta1$colMoss <- ifelse(beta1$nonvascularC<=10,colM[1],
					ifelse(beta1$nonvascularC>10&beta1$nonvascularC<=20,colM[2],
					ifelse(beta1$nonvascularC>20&beta1$nonvascularC<=30,colM[3],
					ifelse(beta1$nonvascularC>30&beta1$nonvascularC<=40,colM[4],
					ifelse(beta1$nonvascularC>40&beta1$nonvascularC<=50,colM[5],
					ifelse(beta1$nonvascularC>50&beta1$nonvascularC<=60,colM[6],
					ifelse(beta1$nonvascularC>60&beta1$nonvascularC<=70,colM[7],
					ifelse(beta1$nonvascularC>70&beta1$nonvascularC<80,colM[8],rgb(0,0,0)))))))))	
					
					
beta2$colShrub <- ifelse(beta2$shrubC<=12.5,colS[1],
					ifelse(beta2$shrubC>12.5&beta2$shrubC<=25,colS[2],
					ifelse(beta2$shrubC>25&beta2$shrubC<=37.5,colS[3],
					ifelse(beta2$shrubC>37.5&beta2$shrubC<=50,colS[4],
					ifelse(beta2$shrubC>50&beta2$shrubC<=67.5,colS[5],
					ifelse(beta2$shrubC>67.5&beta2$shrubC<=75,colS[6],
					ifelse(beta2$shrubC>75&beta2$shrubC<=87.5,colS[7],
					ifelse(beta2$shrubC>87.5&beta2$shrubC<100,colS[8],rgb(0,0,0)))))))))
					
beta2$colMoss <- ifelse(beta2$nonvascularC<=10,colM[1],
					ifelse(beta2$nonvascularC>10&beta2$nonvascularC<=20,colM[2],
					ifelse(beta2$nonvascularC>20&beta2$nonvascularC<=30,colM[3],
					ifelse(beta2$nonvascularC>30&beta2$nonvascularC<=40,colM[4],
					ifelse(beta2$nonvascularC>40&beta2$nonvascularC<=50,colM[5],
					ifelse(beta2$nonvascularC>50&beta2$nonvascularC<=60,colM[6],
					ifelse(beta2$nonvascularC>60&beta2$nonvascularC<=70,colM[7],
					ifelse(beta2$nonvascularC>70&beta2$nonvascularC<80,colM[8],rgb(0,0,0)))))))))						
					
#add color for points
SiteCol <- unique(data.frame(siteid=beta1$siteid, colMoss=beta1$colMoss,colShrub=beta1$colShrub))
ParmPC <- join(ParmPC, SiteCol, by="siteid",type="left")					
#now join to soil data					
	


	
wd <- 20
hd <- 20

yli <- c(-40,-5,0)
yhi <- c(5,30,.65)
xlA <- c(-45,0,-45)
xhA <- c(0,35,0)
xlD <- -1
xhD <- 21
pcx <- 3
slw <- 4
regCent <- function(x,y0,y1,xbar){
	y0+(y1*(x-xbar))
}
startR <- c(0,22,44)
#regression
for(i in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_all_",i,".jpg"), width=2000,height=2000,
			quality=100,units="px")
	layout(matrix(seq(1,4),ncol=2,byrow=TRUE), width=rep(lcm(wd),4),height=rep(lcm(hd),4))
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlD,xhD),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$depth,ParmPC$Mean,pch=19,col=paste(ParmPC$colShrub),cex=pcx)
			for(j in 1:22){
				points(seq(0,20),regCent(seq(0,20),beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],
							beta1$Mean[beta1$regID==i&beta1$regsiteID==j+startR[i]],0),
							col=paste(beta1$colShrub[beta0$regID==i&beta0$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
			}
			box(which="plot")
			
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlA[i],xhA[i]),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$AMean,ParmPC$Mean,pch=19,col=paste(ParmPC$colShrub),cex=pcx)
			
			for(j in 1:22){
				points(seq(yli[i],yhi[i]),regCent(seq(yli[i],yhi[i]),beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],
							beta2$Mean[beta2$regID==i&beta2$regsiteID==j+startR[i]],AirMean$Abar[i]),
							col=paste(beta1$colShrub[beta0$regID==i&beta0$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
			}
			box(which="plot")			
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlD,xhD),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$depth,ParmPC$Mean,pch=19,col=paste(ParmPC$colMoss),cex=pcx)
			
			for(j in 1:22){
				points(seq(0,20),regCent(seq(0,20),beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],
							beta1$Mean[beta1$regID==i&beta1$regsiteID==j+startR[i]],0),
							col=paste(beta1$colMoss[beta0$regID==i&beta0$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
			}
			box(which="plot")
			
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlA[i],xhA[i]),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$AMean,ParmPC$Mean,pch=19,col=paste(ParmPC$colMoss),cex=pcx)
						for(j in 1:22){
				points(seq(yli[i],yhi[i]),regCent(seq(yli[i],yhi[i]),beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],
							beta2$Mean[beta2$regID==i&beta2$regsiteID==j+startR[i]],AirMean$Abar[i]),
							col=paste(beta1$colMoss[beta0$regID==i&beta0$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
			}
			box(which="plot")					
	dev.off()
}	