##########################################################
########Soil moisture analysis script          ###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script extracts soil moisture from the        ###
### database output, matches it to temperature parms   ###
### and runs a linear model to analyze patterns in     ###
### air and soil temperature coupling                  ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
##########################################################
##########################################################
library(lubridate)

##########################################
##first grab soil temperature parameters##
##########################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\vegetation_analyses\\temp_parm_extract.r")


###########################################
## read in soil moisture data from the db##
###########################################
#read in soil moisture
datVW <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil_moist.csv", na.strings=c("NaN","NA")) 

#read in soil dataset
Sdesc <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil.csv", na.strings=c("NaN","NA"))

#get a count of daily observations per site, year, depth

VWcount <- aggregate(datVW$swc, by=list(datVW$year_sm, datVW$sw_depth,datVW$site_id), FUN="length")
colnames(VWcount) <- c("year", "depth", "siteid", "count")

#get the unique number of sites with data

VWsiteall <- unique(VWcount$siteid)
To.use <- data.frame(site_id=VWsiteall)


################################################
##match up to organic vs mineral layer #########
################################################
#pull out organic layer only for the sites with vwc
Organic <- join(Sdesc, To.use, by="site_id", type="inner" )
#omit any sites with a missing organic layer
Organic <- Organic[is.na(Organic$organic_thick)==FALSE,]

#join back to the vwc data
VWC <- join(Organic, datVW, by="site_id", type="inner")
VWC$orgID <- ifelse(VWC$sw_depth <= VWC$organic_thick, 1, 2)

#only focus on upper 20cm soil moisture for not
VWC2 <- VWC[VWC$sw_depth <= 20,]


################################################
##get average over summer months       #########
################################################


#grab out months of june, july, august, and september
monthStart <- c(152, 182, 213, 244)
monthEnd <- c(181,212,243,273)
monthLength <- (monthEnd+1)-monthStart

#make a leap year id
VWC2$leapID <- ifelse(leap_year(VWC2$year_sm)==TRUE,1,0)

#pull out vwc data in each monthly period
VWCmonth <- list()
for(i in 1:length(monthStart)){
    VWCmonth[[i]] <- VWC2[VWC2$doy_sm>=(monthStart[i]+VWC2$leapID)&VWC2$doy_sm<=(monthEnd[i]+VWC2$leapID),]
    VWCmonth[[i]]$monthID <- rep(i, dim(VWCmonth[[i]])[1])
}
#turn back into a data frame
 
VWCsub <- ldply(VWCmonth)

#pull out only data that is needed
VWCsub2 <- data.frame(site_id=VWCsub$site_id,  year_sm= VWCsub$year_sm, monthID= VWCsub$monthID,
                        sw_depth=VWCsub$sw_depth, orgID=VWCsub$orgID, swc=VWCsub$swc)
VWCsub2 <- na.omit(VWCsub2)

#now aggregate by month
VWmean <- aggregate(VWCsub2$swc, by=list(VWCsub2$sw_depth, VWCsub2$orgID, VWCsub2$year_sm, VWCsub2$site_id, VWCsub2$monthID ), FUN="mean")
colnames(VWmean) <- c("depth", "orgID", "wyear", "siteid", "monthID", "vwc.ave")
VWn <- aggregate(VWCsub2$swc, by=list(VWCsub2$sw_depth, VWCsub2$orgID, VWCsub2$year_sm, VWCsub2$site_id, VWCsub2$monthID ), FUN="length")
colnames(VWn) <- c("depth", "orgID", "wyear", "siteid", "monthID", "vwc.n")

VWmean$vwc.n <- VWn$vwc.n

#only take average when at least 90 % of days included all months 27 days is 90% rounded

VWn90 <- VWmean[VWmean$vwc.n >= 27, ]

#see how many observations there are at each site in each layer

VWcount<-aggregate(VWn90$depth, by=list(VWn90$orgID,VWn90$wyear,VWn90$siteid,VWn90$monthID ), FUN="length")
#most sites only have one sensor in organic vs mineral layer <20cm so assume that one observation is
#sufficient and just average in each layer if more

VWlayer <- aggregate(VWn90$vwc.ave, by=list(VWn90$orgID,VWn90$wyear,VWn90$siteid,VWn90$monthID ), FUN="mean")
colnames(VWlayer) <- c("orgID", "wyear", "siteid", "monthID", "vwc")

#only 2 sites have soil texture in the filtered data so not worth pursuing a relative water content analysis


################################################
##now match up to output               #########
################################################
#start by looking at only organic soil
VWlayerO <- VWlayer[VWlayer$orgID == 1,]

#make a table to first get any ids that should be matched

VWsID <- unique(data.frame(wyear=VWlayerO$wyear,siteid=VWlayerO$siteid))


#now split up by months to make each in column
VWJuneOT <- VWlayerO[VWlayerO$monthID==1, ] 
VWJuneO <- data.frame(wyear=VWJuneOT$wyear,siteid=VWJuneOT$siteid,vwcJune=VWJuneOT$vwc)
VWJulyOT <- VWlayerO[VWlayerO$monthID==2, ] 
VWJulyO <- data.frame(wyear=VWJulyOT$wyear,siteid=VWJulyOT$siteid,vwcJuly=VWJulyOT$vwc)
VWAugOT <- VWlayerO[VWlayerO$monthID==3, ] 
VWAugO <- data.frame(wyear=VWAugOT$wyear,siteid=VWAugOT$siteid,vwcAug=VWAugOT$vwc)
VWSeptOT <- VWlayerO[VWlayerO$monthID==4, ] 
VWSeptO <- data.frame(wyear=VWSeptOT$wyear,siteid=VWSeptOT$siteid,vwcSept=VWSeptOT$vwc)
#######match to N factors####
#start by only grabbing years and sites that vwc data exists for
Nsub <- join(Nfactor, VWsID, by=c("wyear", "siteid"), type="inner" )

#now join each month
NswcMonth <- join(Nsub, VWJuneO, by=c("wyear", "siteid"), type="left")

NswcMonth <- join(NswcMonth, VWJulyO, by=c("wyear", "siteid"), type="left")

NswcMonth <- join(NswcMonth, VWAugO, by=c("wyear", "siteid"), type="left")

NswcMonth <- join(NswcMonth, VWSeptO, by=c("wyear", "siteid"), type="left")

plot(NswcMonth$vwcAug[NswcMonth$parm=="Tn"],NswcMonth$Mean[NswcMonth$parm=="Tn"], pch=19)

#now look at soil temp parms

Tsub <- join(SoilParm, VWsID, by=c("wyear", "siteid"), type="inner" )

TswcMonth <- join(Tsub, VWJuneO, by=c("wyear", "siteid"), type="left")

TswcMonth <-join(TswcMonth, VWJulyO, by=c("wyear", "siteid"), type="left")
TswcMonth <-join(TswcMonth, VWAugO, by=c("wyear", "siteid"), type="left")
TswcMonth <-join(TswcMonth, VWSeptO, by=c("wyear", "siteid"), type="left")