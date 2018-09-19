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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\interannual\\model\\run8"
Nrun <- 8
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
#subset sites with at least 5 years
YearSub <- YearCount[YearCount$nYear >=5,]

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
#most vegeclasses only have 1-3 sites with at least 2 years of data
#need to subset vege classes to focus on ones with more sites
#subset so that there are at least 5 different sites in a vegeclass
siteNl <- siteNl[siteNl$count>=5,]
#join back into Yearsub
YearSub <- join(YearSub, siteNl, by="vegeclass", type="inner")
#make a dataframe with only info needed for joining
YearSub2 <- data.frame(depth=YearSub$depth, siteid=YearSub$siteid,vegeclass=YearSub$vegeclass)
#subset ParmAll to only focus on interannual sites
ParmAlls <- join(ParmAll, YearSub2, by=c("depth","siteid"), type="inner") 


#organize previous air components
#organize past air maximum focusing on past 4 years
Tmax <- SoilParm[SoilParm$parm=="TmaxS",]


#matching for 1 year into the past
Tmax1 <- Tmax
Tmax1$wyear <- Tmax1$wyear+1
colnames(Tmax1)[1:4] <- paste0(colnames(Tmax1)[1:4],"Max1")
#matching for 2 year into the past
Tmax2 <- Tmax
Tmax2$wyear <- Tmax2$wyear+2
colnames(Tmax2)[1:4] <- paste0(colnames(Tmax2)[1:4],"Max2")
#matching for 3 year into the past
Tmax3 <- Tmax
Tmax3$wyear <- Tmax3$wyear+3
colnames(Tmax3)[1:4] <- paste0(colnames(Tmax3)[1:4],"Max3")
#matching for 4 year into the past
Tmax4 <- Tmax
Tmax4$wyear <- Tmax4$wyear+4
colnames(Tmax4)[1:4] <- paste0(colnames(Tmax4)[1:4],"Max4")

#join to ParmAlls

ParmAlls1 <- join(ParmAlls,Tmax1, by=c("siteid","wyear","depth"),type="left")
ParmAlls2 <- join(ParmAlls1, Tmax2, by=c("siteid","wyear","depth"), type="left")
ParmAlls3 <- join(ParmAlls2, Tmax3, by=c("siteid","wyear","depth"), type="left")
ParmAlls4 <- join(ParmAlls3, Tmax4, by=c("siteid","wyear","depth"), type="left")

#omit any data with NA because that means there aren't enough preceding years
ParmAlls5 <- na.omit(ParmAlls4)

#make regVege table
regVegeDF <- unique(data.frame(regID=ParmAlls5$regID,vegeclass=ParmAlls5$vegeclass)) 
regVegeDF$regvegeID <- seq(1,dim(regVegeDF)[1])

#join into dataframe
ParmAlls6 <- join(ParmAlls5,regVegeDF, by=c("regID","vegeclass"), type="left")

#calculate average air temp in each regression
airTempCurrentm <- aggregate(ParmAlls6$AMean,by=list(ParmAlls6$regID),FUN="mean")

colnames(airTempCurrentm) <- c("regID","meanA")

#calculate average past temp across sites
		
aveAnt <- aggregate(c(ParmAlls6$MeanMax1,ParmAlls6$MeanMax2,ParmAlls6$MeanMax3,ParmAlls6$MeanMax4), 
						by=list(c(ParmAlls6$regID,ParmAlls6$regID,ParmAlls6$regID,ParmAlls6$regID)),
						FUN="mean")
colnames(aveAnt) <- c("regID","tempAve")


#read in model results 

datM <- read.csv(paste0(modDI,"\\inter_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\inter_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))

datC$parms2 <- gsub("\\W","",gsub("\\d","",datC$parms ))


reps <- datC[datC$parms2=="repSoilP",]
reps$regID <- ParmAlls6$regID
plot(ParmAlls6$Mean[ParmAlls6$regID==1],reps$Mean[reps$regID==1])
plot(ParmAlls6$Mean[ParmAlls6$regID==2],reps$Mean[reps$regID==2])
plot(ParmAlls6$Mean[ParmAlls6$regID==3],reps$Mean[reps$regID==3])

fit1 <- lm(reps$Mean[reps$regID==1]~ParmAlls6$Mean[ParmAlls6$regID==1])
summary(fit1)
fit2 <- lm(reps$Mean[reps$regID==2]~ParmAlls6$Mean[ParmAlls6$regID==2])
summary(fit2)
fit3 <- lm(reps$Mean[reps$regID==3]~ParmAlls6$Mean[ParmAlls6$regID==3])
summary(fit3)