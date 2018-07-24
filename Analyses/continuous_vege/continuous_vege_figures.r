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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\model\\run3"
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\continuous\\plots"
Nrun <- 3
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


#generate dataset for monitoring regression means

mu.monitor <- data.frame(regID = rep(seq(1,3),each=100), 
				monitorAir = c(seq(-45,0, length.out=100),seq(0,35,length.out=100),seq(0,.65,length.out=100)),
				monitorDepth=rep(seq(0,20,length.out=100),times=3))
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

#create significance column
datA$sigP <- ifelse(datA$X0.2.<0&datA$X99.8.<0,"*",
			ifelse(datA$X0.2.>0&datA$X99.8.>0,"*","NS"))
datB$sigP <- ifelse(datB$X0.2.<0&datB$X99.8.<0,"*",
			ifelse(datB$X0.2.>0&datB$X99.8.>0,"*","NS"))
datD$sigP <- ifelse(datD$X0.2.<0&datD$X99.8.<0,"*",
			ifelse(datD$X0.2.>0&datD$X99.8.>0,"*","NS"))

datA$signP <- ifelse(datA$X0.2.<0&datA$X99.8.<0,"-",
			ifelse(datA$X0.2.>0&datA$X99.8.>0,"+"," "))			
datB$signP <- ifelse(datB$X0.2.<0&datB$X99.8.<0,"-",
			ifelse(datB$X0.2.>0&datB$X99.8.>0,"+"," "))
datD$signP <- ifelse(datD$X0.2.<0&datD$X99.8.<0,"-",
			ifelse(datD$X0.2.>0&datD$X99.8.>0,"+"," "))			
			
parmR <- list(datA,datB,datD)

beta0 <- datC[datC$parms=="beta0",]
beta1 <- datC[datC$parms=="beta1",]
beta2 <- datC[datC$parms=="beta2",]
#add identifying information onto betas
beta0 <- cbind(beta0,PCdata)
beta1 <- cbind(beta1,PCdata)
beta2 <- cbind(beta2,PCdata)

#subset out extreme examples
Xbeta0M <- datC[datC$parms=="Xbeta0M",]
Xbeta0N <- datC[datC$parms=="Xbeta0N",]
Xbeta0S <-  datC[datC$parms=="Xbeta0S",]
Xbeta1M <- datC[datC$parms=="Xbeta1M",]
Xbeta1N <- datC[datC$parms=="Xbeta1N",]
Xbeta1S <-  datC[datC$parms=="Xbeta1S",]
Xbeta2M <- datC[datC$parms=="Xbeta2M",]
Xbeta2N <- datC[datC$parms=="Xbeta2N",]
Xbeta2S <-  datC[datC$parms=="Xbeta2S",]

#add significance to beta1 and beta 2
beta1$sigP <- ifelse(beta1$X0.2.<0&beta1$X99.8.<0,1,
				ifelse(beta1$X0.2.>0&beta1$X99.8.>0,1,0))

beta2$sigP <- ifelse(beta2$X0.2.<0&beta2$X99.8.<0,1,
				ifelse(beta2$X0.2.>0&beta2$X99.8.>0,1,0))
#add significance to example betas of extremes				
Xbeta1M$sigP <- ifelse(Xbeta1M$X0.2.<0&Xbeta1M$X99.8.<0,1,
				ifelse(Xbeta1M$X0.2.>0&Xbeta1M$X99.8.>0,1,0))				
Xbeta1S$sigP <- ifelse(Xbeta1S$X0.2.<0&Xbeta1S$X99.8.<0,1,
				ifelse(Xbeta1S$X0.2.>0&Xbeta1S$X99.8.>0,1,0))					
Xbeta1N$sigP <- ifelse(Xbeta1N$X0.2.<0&Xbeta1N$X99.8.<0,1,
				ifelse(Xbeta1N$X0.2.>0&Xbeta1N$X99.8.>0,1,0))
				
Xbeta2M$sigP <- ifelse(Xbeta2M$X0.2.<0&Xbeta2M$X99.8.<0,1,
				ifelse(Xbeta2M$X0.2.>0&Xbeta2M$X99.8.>0,1,0))				
Xbeta2S$sigP <- ifelse(Xbeta2S$X0.2.<0&Xbeta2S$X99.8.<0,1,
				ifelse(Xbeta2S$X0.2.>0&Xbeta2S$X99.8.>0,1,0))					
Xbeta2N$sigP <- ifelse(Xbeta2N$X0.2.<0&Xbeta2N$X99.8.<0,1,
				ifelse(Xbeta2N$X0.2.>0&Xbeta2N$X99.8.>0,1,0))				
				
#pull out regression means
mu.site.air <- datC[datC$parms2=="mu.site.air[,",]
mu.site.depth <- datC[datC$parms2=="mu.site.depth[,",]



#accidentally monitored betas for all regressions not just the ones that they are in. Pull out the matching betas that belong
#first indicate what each stands for
mu.site.air$regsiteID <- rep(seq(1,dim(PCdata)[1]), each=300)
mu.site.air$regID <- rep(rep(seq(1,3), each=100),times=66)

mu.site.depth$regsiteID <- rep(seq(1,dim(PCdata)[1]), each=300)
mu.site.depth$regID <- rep(rep(seq(1,3), each=100),times=66)

#each regression has 22 ids in it so subset to pull out the right one
mu.site.airp1 <- mu.site.air[mu.site.air$regID==1&mu.site.air$regsiteID<=22,]
mu.site.airp2 <- mu.site.air[mu.site.air$regID==2&mu.site.air$regsiteID>22&mu.site.air$regsiteID<=44,]
mu.site.airp3 <- mu.site.air[mu.site.air$regID==3&mu.site.air$regsiteID>44&mu.site.air$regsiteID<=66,]

mu.site.airp <- rbind(mu.site.airp1,mu.site.airp2,mu.site.airp3)

mu.site.depthp1 <- mu.site.depth[mu.site.depth$regID==1&mu.site.depth$regsiteID<=22,]
mu.site.depthp2 <- mu.site.depth[mu.site.depth$regID==2&mu.site.depth$regsiteID>22&mu.site.depth$regsiteID<=44,]
mu.site.depthp3 <- mu.site.depth[mu.site.depth$regID==3&mu.site.depth$regsiteID>44&mu.site.depth$regsiteID<=66,]

mu.site.depth <- rbind(mu.site.depthp1,mu.site.depthp2,mu.site.depthp3)



#join other info
mu.site.air <- join(mu.site.air, PCdata, by="regsiteID",type="left")
mu.site.depth <- join(mu.site.depth, PCdata, by="regsiteID",type="left")






#pull out regression means for example of extremes
mu.XN.air <- datC[datC$parms2=="mu.X.air[,",]
mu.XN.depth <- datC[datC$parms2=="mu.X.depth[,",]

mu.XM.air <- datC[datC$parms2=="mu.Xmoss.air[,",]
mu.XM.depth <- datC[datC$parms2=="mu.Xmoss.depth[,",]

mu.XS.air <- datC[datC$parms2=="mu.Xshrub.air[,",]
mu.XS.depth <- datC[datC$parms2=="mu.Xshrub.depth[,",]


#pull out the appropriate regression
mu.XN.air <- rbind(mu.XN.air[1:100,],mu.XN.air[401:500,],mu.XN.air[801:900,])
mu.XN.air$regID <- rep(seq(1,3),each=100)

mu.XN.depth <- rbind(mu.XN.depth[1:100,],mu.XN.depth[401:500,],mu.XN.depth[801:900,])
mu.XN.depth$regID <- rep(seq(1,3),each=100)

mu.XM.air <- rbind(mu.XM.air[1:100,],mu.XM.air[401:500,],mu.XM.air[801:900,])
mu.XM.air$regID <- rep(seq(1,3),each=100)

mu.XM.depth <- rbind(mu.XM.depth[1:100,],mu.XM.depth[401:500,],mu.XM.depth[801:900,])
mu.XM.depth$regID <- rep(seq(1,3),each=100)


mu.XS.air <- rbind(mu.XS.air[1:100,],mu.XS.air[401:500,],mu.XS.air[801:900,])
mu.XS.air$regID <- rep(seq(1,3),each=100)

mu.XS.depth <- rbind(mu.XS.depth[1:100,],mu.XS.depth[401:500,],mu.XS.depth[801:900,])
mu.XS.depth$regID <- rep(seq(1,3),each=100)


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


					
mu.site.air$colMoss <- ifelse(mu.site.air$nonvascularC<=10,colM[1],
					ifelse(mu.site.air$nonvascularC>10&mu.site.air$nonvascularC<=20,colM[2],
					ifelse(mu.site.air$nonvascularC>20&mu.site.air$nonvascularC<=30,colM[3],
					ifelse(mu.site.air$nonvascularC>30&mu.site.air$nonvascularC<=40,colM[4],
					ifelse(mu.site.air$nonvascularC>40&mu.site.air$nonvascularC<=50,colM[5],
					ifelse(mu.site.air$nonvascularC>50&mu.site.air$nonvascularC<=60,colM[6],
					ifelse(mu.site.air$nonvascularC>60&mu.site.air$nonvascularC<=70,colM[7],
					ifelse(mu.site.air$nonvascularC>70&mu.site.air$nonvascularC<80,colM[8],rgb(0,0,0)))))))))	

mu.site.air$colShrub <- ifelse(mu.site.air$shrubC<=12.5,colS[1],
					ifelse(mu.site.air$shrubC>12.5&mu.site.air$shrubC<=25,colS[2],
					ifelse(mu.site.air$shrubC>25&mu.site.air$shrubC<=37.5,colS[3],
					ifelse(mu.site.air$shrubC>37.5&mu.site.air$shrubC<=50,colS[4],
					ifelse(mu.site.air$shrubC>50&mu.site.air$shrubC<=67.5,colS[5],
					ifelse(mu.site.air$shrubC>67.5&mu.site.air$shrubC<=75,colS[6],
					ifelse(mu.site.air$shrubC>75&mu.site.air$shrubC<=87.5,colS[7],
					ifelse(mu.site.air$shrubC>87.5&mu.site.air$shrubC<100,colS[8],rgb(0,0,0)))))))))

mu.site.depth$colShrub <- ifelse(mu.site.depth$shrubC<=12.5,colS[1],
					ifelse(mu.site.depth$shrubC>12.5&mu.site.depth$shrubC<=25,colS[2],
					ifelse(mu.site.depth$shrubC>25&mu.site.depth$shrubC<=37.5,colS[3],
					ifelse(mu.site.depth$shrubC>37.5&mu.site.depth$shrubC<=50,colS[4],
					ifelse(mu.site.depth$shrubC>50&mu.site.depth$shrubC<=67.5,colS[5],
					ifelse(mu.site.depth$shrubC>67.5&mu.site.depth$shrubC<=75,colS[6],
					ifelse(mu.site.depth$shrubC>75&mu.site.depth$shrubC<=87.5,colS[7],
					ifelse(mu.site.depth$shrubC>87.5&mu.site.depth$shrubC<100,colS[8],rgb(0,0,0)))))))))
					
mu.site.depth$colMoss <- ifelse(mu.site.depth$nonvascularC<=10,colM[1],
					ifelse(mu.site.depth$nonvascularC>10&mu.site.depth$nonvascularC<=20,colM[2],
					ifelse(mu.site.depth$nonvascularC>20&mu.site.depth$nonvascularC<=30,colM[3],
					ifelse(mu.site.depth$nonvascularC>30&mu.site.depth$nonvascularC<=40,colM[4],
					ifelse(mu.site.depth$nonvascularC>40&mu.site.depth$nonvascularC<=50,colM[5],
					ifelse(mu.site.depth$nonvascularC>50&mu.site.depth$nonvascularC<=60,colM[6],
					ifelse(mu.site.depth$nonvascularC>60&mu.site.depth$nonvascularC<=70,colM[7],
					ifelse(mu.site.depth$nonvascularC>70&mu.site.depth$nonvascularC<80,colM[8],rgb(0,0,0)))))))))						


#colors for means

#set up colors for transparency
for(i in 1:dim(mu.site.depth)[1]){
	mu.site.depth$redShrub[i] <- col2rgb(paste(mu.site.depth$colShrub[i]))[1,1]
	mu.site.depth$greenShrub[i] <- col2rgb(paste(mu.site.depth$colShrub[i]))[2,1]
	mu.site.depth$blueShrub[i] <- col2rgb(paste(mu.site.depth$colShrub[i]))[3,1]
	mu.site.depth$redMoss[i] <- col2rgb(paste(mu.site.depth$colMoss[i]))[1,1]
	mu.site.depth$greenMoss[i] <- col2rgb(paste(mu.site.depth$colMoss[i]))[2,1]
	mu.site.depth$blueMoss[i] <- col2rgb(paste(mu.site.depth$colMoss[i]))[3,1]
	
	
	mu.site.air$redShrub[i] <- col2rgb(paste(mu.site.air$colShrub[i]))[1,1]
	mu.site.air$greenShrub[i] <- col2rgb(paste(mu.site.air$colShrub[i]))[2,1]
	mu.site.air$blueShrub[i] <- col2rgb(paste(mu.site.air$colShrub[i]))[3,1]
	mu.site.air$redMoss[i] <- col2rgb(paste(mu.site.air$colMoss[i]))[1,1]
	mu.site.air$greenMoss[i] <- col2rgb(paste(mu.site.air$colMoss[i]))[2,1]
	mu.site.air$blueMoss[i] <- col2rgb(paste(mu.site.air$colMoss[i]))[3,1]
}


					
#add color for points
SiteCol <- unique(data.frame(siteid=beta1$siteid, colMoss=beta1$colMoss,colShrub=beta1$colShrub))
ParmPC <- join(ParmPC, SiteCol, by="siteid",type="left")					
#now join to soil data					
	


	
wd <- 20
hd <- 20

yli <- c(-40,0,0)
yhi <- c(0,30,.65)
xlA <- c(-45,0,0)
xhA <- c(0,35,.65)
xlD <- -1
xhD <- 21
pcx <- 3
slw <- 4
si <- c(1,1,.01)
yii <- c(5,5,.1)

xTA <- c(-5,25,.55)
xTD <- 15
yT <- c(-30,5,.25)
cxT <- 10
mx <- 3
mly <- 6
mlx <- 4
tx <- 6
yname <- c("Minimum soil temperature","Maximum soil temperature","Time of soil minimum")
xname <- c("Air minimum", "Air maximum","Air time of minimum")

regCent <- function(x,y0,y1,xbar){
	y0+(y1*(x-xbar))
}
startR <- c(0,22,44)
#regression
for(i in 1:3){
	jpeg(paste0(plotDI,"\\run",Nrun,"\\regression_all_",i,".jpg"), width=3000,height=2000,
			quality=100,units="px")
	layout(matrix(seq(1,8),ncol=4,byrow=TRUE), width=rep(lcm(wd),8),height=rep(lcm(hd),8))
		#plot example of extreme
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlD,xhD),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
					
			if(Xbeta1N$sigP[i]==1){
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(mu.XN.depth$X0.2.[mu.XN.depth$regID==i],
						rev(mu.XN.depth$X99.8.[mu.XN.depth$regID==i])),
						col=rgb(redS[1]/255,
						greenS[1]/255,
						blueS[1]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(rep(Xbeta0N$X0.2.[i],100),
						rev(rep(Xbeta0N$X99.8.[i],100))),
						col=rgb(redS[1]/255,
						greenS[1]/255,
						blueS[1]/255,.35),border=NA)
			}
			if(Xbeta1S$sigP[i]==1){
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(mu.XS.depth$X0.2.[mu.XS.depth$regID==i],
						rev(mu.XS.depth$X99.8.[mu.XS.depth$regID==i])),
						col=rgb(redS[8]/255,
						greenS[8]/255,
						blueS[8]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(rep(Xbeta0S$X0.2.[i],100),
						rev(rep(Xbeta0S$X99.8.[i],100))),
						col=rgb(redS[8]/255,
						greenS[8]/255,
						blueS[8]/255,.35),border=NA)
			}	

			if(Xbeta1N$sigP[i]==1){

					points(mu.monitor$monitorDepth[mu.monitor$regID==i],
						mu.XN.depth$Mean[mu.XN.depth$regID==i],
							col=paste(colS[1]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0N$Mean[i],lwd=slw,
							col=paste(colS[1]), lty=2)
			
				}

			if(Xbeta1S$sigP[i]==1){

					points(mu.monitor$monitorDepth[mu.monitor$regID==i],
						mu.XS.depth$Mean[mu.XS.depth$regID==i],
							col=paste(colS[8]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0S$Mean[i],lwd=slw,
							col=paste(colS[8]), lty=2)
			
				}
				
				
		axis(2, seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]), rep("",length(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]))),lwd.ticks=tx)
		mtext(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),at=seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),
				side=2,line=mly,cex=mx,las=2)
		text(xTD,yT[i], paste(datB$sigP[datB$parmID==2&datB$regID==i],datB$signP[datB$parmID==2&datB$regID==i]),cex=cxT)
		box(which="plot")
		legend("topleft",c("0 % shrub", "100% shrub"), pch=19, col=c(colS[1],colS[8]), cex=5, bty="n")
		mtext(paste0(yname[i]), outer=TRUE, side=2, line=-20, cex=5)
		#plot data
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlD,xhD),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$depth[ParmPC$regID==i],ParmPC$Mean[ParmPC$regID==i],pch=19,col=paste(ParmPC$colShrub),cex=pcx)
			
			for(j in 1:22){
			
			
				if(beta1$sigP[beta1$regID==i&beta1$regsiteID==j+startR[i]]==1){
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(mu.site.depth$X0.2.[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]],
						rev(mu.site.depth$X99.8.[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]])),
						col=rgb(mu.site.depth$redShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$greenShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$blueShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(rep(beta0$X0.2.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100),
						rev(rep(beta0$X99.8.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100))),
						col=rgb(mu.site.depth$redShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$greenShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$blueShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,.35),border=NA)
			}
			}
			for(j in 1:22){
				if(beta1$sigP[beta1$regID==i&beta1$regsiteID==j+startR[i]]==1){

					points(mu.monitor$monitorDepth[mu.monitor$regID==i],
						mu.site.depth$Mean[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]],
							col=paste(mu.site.depth$colShrub[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
				}else{
					abline(h=beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],lwd=slw,
							col=paste(beta0$colShrub[beta0$regID==i&beta0$regsiteID==j+startR[i]]), lty=2)
			
				}
			}
			
			box(which="plot")

		#data
		
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlA[i],xhA[i]),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$AMean[ParmPC$regID==i],ParmPC$Mean[ParmPC$regID==i],pch=19,col=paste(ParmPC$colShrub),cex=pcx)
			for(j in 1:22){
				if(beta2$sigP[beta2$regID==i&beta2$regsiteID==j+startR[i]]==1){
				
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(mu.site.air$X0.2.[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]],
						rev(mu.site.air$X99.8.[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]])),
						col=rgb(mu.site.air$redShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$greenShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$blueShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(rep(beta0$X0.2.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100),
						rev(rep(beta0$X99.8.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100))),
						col=rgb(mu.site.air$redShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$greenShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$blueShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,.35),border=NA)
				
				}
			}
			for(j in 1:22){
				if(beta2$sigP[beta2$regID==i&beta2$regsiteID==j+startR[i]]==1){
					points(mu.monitor$monitorAir[mu.monitor$regID==i],
						mu.site.air$Mean[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]],
							col=paste(mu.site.air$colShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
				}else{
					abline(h=beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],lwd=slw,
							col=paste(beta0$colShrub[beta0$regID==i&beta0$regsiteID==j+startR[i]]), lty=2)

					}
							
			}
			
			box(which="plot")	

		#plot extreme example
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlA[i],xhA[i]),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		if(Xbeta2N$sigP[i]==1){
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(mu.XN.air$X0.2.[mu.XN.air$regID==i],
						rev(mu.XN.air$X99.8.[mu.XN.air$regID==i])),
						col=rgb(redS[1]/255,
						greenS[1]/255,
						blueS[1]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(rep(Xbeta0N$X0.2.[i],100),
						rev(rep(Xbeta0N$X99.8.[i],100))),
						col=rgb(redS[1]/255,
						greenS[1]/255,
						blueS[1]/255,.35),border=NA)
			}
			if(Xbeta2S$sigP[i]==1){
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(mu.XS.air$X0.2.[mu.XS.air$regID==i],
						rev(mu.XS.air$X99.8.[mu.XS.air$regID==i])),
						col=rgb(redS[8]/255,
						greenS[8]/255,
						blueS[8]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(rep(Xbeta0S$X0.2.[i],100),
						rev(rep(Xbeta0S$X99.8.[i],100))),
						col=rgb(redS[8]/255,
						greenS[8]/255,
						blueS[8]/255,.35),border=NA)
			}	

			if(Xbeta2N$sigP[i]==1){

					points(mu.monitor$monitorAir[mu.monitor$regID==i],
						mu.XN.air$Mean[mu.XN.air$regID==i],
							col=paste(colS[1]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0N$Mean[i],lwd=slw,
							col=paste(colS[1]), lty=2)
			
				}

			if(Xbeta2S$sigP[i]==1){

					points(mu.monitor$monitorAir[mu.monitor$regID==i],
						mu.XS.air$Mean[mu.XS.air$regID==i],
							col=paste(colS[8]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0S$Mean[i],lwd=slw,
							col=paste(colS[8]), lty=2)
			
				}
			text(xTA[i],yT[i], paste(datD$sigP[datD$parmID==2&datD$regID==i],datD$signP[datD$parmID==2&datD$regID==i]),cex=cxT)

		axis(4, seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]), rep("",length(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]))),lwd.ticks=tx)
		mtext(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),at=seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),
				side=4,line=mly,cex=mx,las=2)			
		box(which="plot")
		mtext(paste0(yname[i]), outer=TRUE, side=4, line=-20, cex=5)
		#plot extreme example
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlD,xhD),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)		
		if(Xbeta1N$sigP[i]==1){
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(mu.XN.depth$X0.2.[mu.XN.depth$regID==i],
						rev(mu.XN.depth$X99.8.[mu.XN.depth$regID==i])),
						col=rgb(redM[1]/255,
						greenM[1]/255,
						blueM[1]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(rep(Xbeta0N$X0.2.[i],100),
						rev(rep(Xbeta0N$X99.8.[i],100))),
						col=rgb(redM[1]/255,
						greenM[1]/255,
						blueM[1]/255,.35),border=NA)
			}
			if(Xbeta1M$sigP[i]==1){
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(mu.XM.depth$X0.2.[mu.XM.depth$regID==i],
						rev(mu.XM.depth$X99.8.[mu.XM.depth$regID==i])),
						col=rgb(redM[8]/255,
						greenM[8]/255,
						blueM[8]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(rep(Xbeta0M$X0.2.[i],100),
						rev(rep(Xbeta0M$X99.8.[i],100))),
						col=rgb(redM[8]/255,
						greenM[8]/255,
						blueM[8]/255,.35),border=NA)
			}	

			if(Xbeta1N$sigP[i]==1){

					points(mu.monitor$monitorDepth[mu.monitor$regID==i],
						mu.XN.depth$Mean[mu.XN.depth$regID==i],
							col=paste(colM[1]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0N$Mean[i],lwd=slw,
							col=paste(colM[1]), lty=2)
			
				}

			if(Xbeta1M$sigP[i]==1){

					points(mu.monitor$monitorDepth[mu.monitor$regID==i],
						mu.XM.depth$Mean[mu.XM.depth$regID==i],
							col=paste(colM[8]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0M$Mean[i],lwd=slw,
							col=paste(colM[8]), lty=2)
			
				}

		text(xTD,yT[i], paste(datB$sigP[datB$parmID==3&datB$regID==i],datB$signP[datB$parmID==3&datB$regID==i]),cex=cxT)

		axis(2, seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]), rep("",length(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]))),lwd.ticks=tx)
		mtext(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),at=seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),
				side=2,line=mly,cex=mx,las=2)		
				
		axis(1, seq(0,15,by=5), rep("",length(seq(0,15,by=5))),lwd.ticks=tx)
		mtext(seq(0,15,by=5),at=seq(0,15,by=5),
				side=1,line=mly,cex=mx)				
				
		box(which="plot")
		mtext("Depth (cm)",  side=1, line=15, cex=5)
		legend("topleft",c("0 % moss", "100% moss"), pch=19, col=c(colM[1],colM[8]), cex=5, bty="n")
		
		#plot data	
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlD,xhD),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$depth[ParmPC$regID==i],ParmPC$Mean[ParmPC$regID==i],pch=19,col=paste(ParmPC$colMoss),cex=pcx)
			for(j in 1:22){
			
			
				if(beta1$sigP[beta1$regID==i&beta1$regsiteID==j+startR[i]]==1){
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(mu.site.depth$X0.2.[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]],
						rev(mu.site.depth$X99.8.[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]])),
						col=rgb(mu.site.depth$redMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$greenMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$blueMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorDepth[mu.monitor$regID==i],rev(mu.monitor$monitorDepth[mu.monitor$regID==i])),
						c(rep(beta0$X0.2.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100),
						rev(rep(beta0$X99.8.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100))),
						col=rgb(mu.site.depth$redMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$greenMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,
						mu.site.depth$blueMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]/255,.35),border=NA)
			}
			}
			for(j in 1:22){
				if(beta1$sigP[beta1$regID==i&beta1$regsiteID==j+startR[i]]==1){

					points(mu.monitor$monitorDepth[mu.monitor$regID==i],
						mu.site.depth$Mean[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]],
							col=paste(mu.site.depth$colMoss[mu.site.depth$regID==i&mu.site.depth$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
				}else{
					abline(h=beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],lwd=slw,
							col=paste(beta0$colMoss[beta0$regID==i&beta0$regsiteID==j+startR[i]]), lty=2)
			
				}
			}
			box(which="plot")
			axis(1, seq(0,15,by=5), rep("",length(seq(0,15,by=5))),lwd.ticks=tx)
			mtext(seq(0,15,by=5),at=seq(0,15,by=5),
				side=1,line=mly,cex=mx)			
			mtext("Depth (cm)",  side=1, line=15, cex=5)
		
		par(mai=c(0,0,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlA[i],xhA[i]),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
				
			points(ParmPC$AMean[ParmPC$regID==i],ParmPC$Mean[ParmPC$regID==i],pch=19,col=paste(ParmPC$colMoss),cex=pcx)
			
			for(j in 1:22){
				if(beta2$sigP[beta2$regID==i&beta2$regsiteID==j+startR[i]]==1){
				
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(mu.site.air$X0.2.[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]],
						rev(mu.site.air$X99.8.[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]])),
						col=rgb(mu.site.air$redMoss[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$greenMoss[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$blueMoss[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(rep(beta0$X0.2.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100),
						rev(rep(beta0$X99.8.[beta0$regID==i&beta0$regsiteID==j+startR[i]],100))),
						col=rgb(mu.site.air$redShrub[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$greenMoss[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,
						mu.site.air$blueMoss[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]/255,.35),border=NA)
				
				}
			}
			for(j in 1:22){
				if(beta2$sigP[beta2$regID==i&beta2$regsiteID==j+startR[i]]==1){
					points(mu.monitor$monitorAir[mu.monitor$regID==i],
						mu.site.air$Mean[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]],
							col=paste(mu.site.air$colMoss[mu.site.air$regID==i&mu.site.air$regsiteID==j+startR[i]]),
							lwd=slw, type="l")
				}else{
					abline(h=beta0$Mean[beta0$regID==i&beta0$regsiteID==j+startR[i]],lwd=slw,
							col=paste(beta0$colMoss[beta0$regID==i&beta0$regsiteID==j+startR[i]]), lty=2)

					}
							
			}
			box(which="plot")	
		axis(1, seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]), rep("",length(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]))),lwd.ticks=tx)
		mtext(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),at=seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),
				side=1,line=mly,cex=mx)
		mtext(paste(xname[i]),  side=1, line=15, cex=5)		
	#plot extreme example
	par(mai=c(0,0,0,0))
		
		plot(c(0,1),c(0,1), ylim=c(yli[i],yhi[i]), xlim=c(xlA[i],xhA[i]),
			xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
	if(Xbeta2N$sigP[i]==1){
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(mu.XN.air$X0.2.[mu.XN.air$regID==i],
						rev(mu.XN.air$X99.8.[mu.XN.air$regID==i])),
						col=rgb(redM[1]/255,
						greenM[1]/255,
						blueM[1]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(rep(Xbeta0N$X0.2.[i],100),
						rev(rep(Xbeta0N$X99.8.[i],100))),
						col=rgb(redM[1]/255,
						greenM[1]/255,
						blueM[1]/255,.35),border=NA)
			}
			if(Xbeta2M$sigP[i]==1){
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(mu.XM.air$X0.2.[mu.XM.air$regID==i],
						rev(mu.XM.air$X99.8.[mu.XM.air$regID==i])),
						col=rgb(redM[8]/255,
						greenM[8]/255,
						blueM[8]/255,.35),border=NA)
				}else{
					polygon(c(mu.monitor$monitorAir[mu.monitor$regID==i],rev(mu.monitor$monitorAir[mu.monitor$regID==i])),
						c(rep(Xbeta0M$X0.2.[i],100),
						rev(rep(Xbeta0M$X99.8.[i],100))),
						col=rgb(redM[8]/255,
						greenM[8]/255,
						blueM[8]/255,.35),border=NA)
			}	

			if(Xbeta2N$sigP[i]==1){

					points(mu.monitor$monitorAir[mu.monitor$regID==i],
						mu.XN.air$Mean[mu.XN.air$regID==i],
							col=paste(colM[1]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0N$Mean[i],lwd=slw,
							col=paste(colM[1]), lty=2)
			
				}

			if(Xbeta2M$sigP[i]==1){

					points(mu.monitor$monitorAir[mu.monitor$regID==i],
						mu.XM.air$Mean[mu.XM.air$regID==i],
							col=paste(colM[8]),
							lwd=slw, type="l")
				}else{
					abline(h=Xbeta0M$Mean[i],lwd=slw,
							col=paste(colM[8]), lty=2)
			
				}	
	text(xTA[i],yT[i], paste(datD$sigP[datD$parmID==3&datD$regID==i],datD$signP[datD$parmID==3&datD$regID==i]),cex=cxT)
		axis(4, seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]), rep("",length(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]))),lwd.ticks=tx)
		mtext(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),at=seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),
				side=4,line=mly,cex=mx,las=2)
	axis(1, seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]), rep("",length(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]))),lwd.ticks=tx)
		mtext(seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),at=seq(yli[i]+yii[i],yhi[i]-yii[i],by=yii[i]),
				side=1,line=mly,cex=mx)	
	box(which="plot")
	mtext(paste(xname[i]),  side=1, line=15, cex=5)	
	
	dev.off()
}	