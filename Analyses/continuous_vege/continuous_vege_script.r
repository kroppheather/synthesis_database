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

#grass is missing a lot of observations. It is difficult to say whether or not 
#it was there or it just wasn't measured so exclude it from analysis

coverAll2 <- join(shrubA, nonvascularA, by="siteid", type="full")