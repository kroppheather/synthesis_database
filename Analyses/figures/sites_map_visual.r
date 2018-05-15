#############################################################
### reads in proximity info on sites to calculate ###########
### how many points overlap for visualization in  ###########
### GIS                                           ###########
#############################################################
library(plyr)
######################
#  Data              #
######################
#read in data
datNear <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\Near70k.txt")
datFID <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\FID_sites.txt")
#3 or more site
datMult <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\agg70k_join.txt")

######################
#  organize comp     #
######################

#organize so that comps can be compared twice
datNear$COMP <-	ifelse(datNear$IN_FID < datNear$NEAR_FID, paste0(datNear$IN_FID,"_",datNear$NEAR_FID),paste0(datNear$NEAR_FID,"_",datNear$IN_FID))

datNearU <- datNear[!duplicated(datNear$COMP),]


#all sites to compare
UFID <- unique(data.frame(site_id=c(datNearU$IN_FID,datNearU$NEAR_FID)))

#now compare to three site level
#need to find how many sites are in more than one comparision

#first see if any two sites don't come up in any comparisions
#count the number of times a site comes up as an input id or and near id
sitesInCount <- aggregate(datNearU$IN_FID, by=list(datNearU$IN_FID), FUN="length")
colnames(sitesInCount) <- c("site_id","INcount")
sitesNearCount <- aggregate(datNearU$NEAR_FID, by=list(datNearU$NEAR_FID), FUN="length")

colnames(sitesNearCount) <- c("site_id","NEARcount")


siteCount <- join(UFID,sitesNearCount, by=c("site_id"), type="left")
siteCount <- join(siteCount,sitesInCount, by=c("site_id"), type="left")
siteCount$NEARcount <-ifelse(is.na(siteCount$NEARcount),0, siteCount$NEARcount)
siteCount$INcount <-ifelse(is.na(siteCount$INcount),0, siteCount$INcount)

siteCount$Total <- siteCount$NEARcount+siteCount$INcount

oneComp <- siteCount[siteCount$Total==1,]

oneCompIN <- data.frame(IN_FID=oneComp$site_id, INoneC = rep(1,dim(oneComp)[1]))

datNearU2 <- join(datNearU, oneCompIN, by="IN_FID", type="left")
datNearU2$INoneC <- ifelse(is.na(datNearU2$INoneC),0,datNearU2$INoneC)

oneCompNEAR <- data.frame(NEAR_FID=oneComp$site_id, NEARoneC = rep(1,dim(oneComp)[1]))
datNearU3 <- join(datNearU2, oneCompNEAR, by="NEAR_FID", type="left")
datNearU3$NEARoneC <- ifelse(is.na(datNearU3$NEARoneC),0,datNearU3$NEARoneC)


#one site only will be all in siteinfo without an id in near
#two sites will each get their own cluster id
TwoSiteNear <- datNearU3[datNearU3$INoneC==1&datNearU3$NEARoneC==1,]

#check that there are no repeated site ids
dim(datMult)

length(unique(datMult$site_id))

#start a data frame that comes up with a cluster id starting with center points
clusterID <- data.frame(clusterID=datMult$ORIG_FID+1, lat=datMult$POINT_Y,long=datMult$POINT_X,site_id=datMult$site_id)

#now make a clusterID for 2 site comp
#first join siteid info
#subset to relevent info
SUBTWO <- data.frame(datFID[,1:2],datFID[,4:5])
TwoSiteNear$clutserID <- seq(17,dim(TwoSiteNear)[1]+16)
INNEAR <- data.frame(FID=TwoSiteNear$IN_FID,clusterID=TwoSiteNear$clutserID)
NEARNEAR <- data.frame(FID=TwoSiteNear$NEAR_FID,clusterID=TwoSiteNear$clutserID)

SUBTWO2 <- join(INNEAR,SUBTWO,by="FID",type="inner")
SUBTWO3 <- join(NEARNEAR,SUBTWO,by="FID",type="inner")

#calculate the mean of the two points
SUBTWO4 <- rbind(SUBTWO2,SUBTWO3)

latTWO <- aggregate(SUBTWO4$lat,by=list(SUBTWO4$clusterID),FUN="mean")
lonTWO <- aggregate(SUBTWO4$lon,by=list(SUBTWO4$clusterID),FUN="mean")
colnames(latTWO) <- c("clusterID","latM")
colnames(lonTWO) <- c("clusterID", "lonM")

SUBTWO5 <- join(SUBTWO4, latTWO, by="clusterID", type="left")
SUBTWO6 <- join(SUBTWO5, lonTWO, by="clusterID", type="left")

#reorganize to join with multisites
TWOCOMP <- data.frame(clusterID=SUBTWO6$clusterID,lat=SUBTWO6$latM,long=SUBTWO6$lonM,site_id=SUBTWO6$site_id)


#combine comp
ClusterComp <- rbind(clusterID,TWOCOMP)

#check if repeat
dim(ClusterComp)
length(unique(ClusterComp$site_id))

#remove duplicates because

ClusterComp2 <- ClusterComp[!duplicated(ClusterComp$site_id),]

#now add in the rest of the sites

#make a list of siteID that are not in the cluster
SITEALL <- data.frame(site_id=datFID$site_id)
SITETEMP <- join(SITEALL,ClusterComp2, by="site_id", type="left")
siteMissing <- SITETEMP$site_id[is.na(SITETEMP$clusterID)]

SUBMissing <-SUBTWO[siteMissing,]

SingleADD <- data.frame(clusterID=seq(31,dim(SUBMissing)[1]+30), lat=SUBMissing$lat,long=SUBMissing$lon,site_id=SUBMissing$site_id)


ClusterComp3 <- rbind(ClusterComp2,SingleADD)

#now join vegeclass
vegeDF <- data.frame(site_id=datFID$site_id,vegeclass=datFID$vege_class)

ClusterComp4 <- join(ClusterComp3,vegeDF,by="site_id",type="left")

#count how many sites are in each cluster class and vegeid
ClusterVege <- aggregate(ClusterComp4$site_id, by=list(ClusterComp4$vegeclass,ClusterComp4$clusterID),FUN="length")
colnames(ClusterVege) <- c("vegeclass","clusterID","count")

#get unique dataframe of cluster latlong
ClusterLoc <- unique(data.frame(clusterID=ClusterComp4$clusterID,lat=ClusterComp4$lat,long=ClusterComp4$long))

#now add site vegeclass count to each clusterID
vegeCounts <- list()
for(i in 1:9){
	vegeCounts[[i]] <- ClusterVege[ClusterVege$vegeclass==i,2:3]
	colnames(vegeCounts[[i]])[2] <- paste0("count",i)
	ClusterLoc <- join(ClusterLoc,vegeCounts[[i]],by="clusterID",type="left")
}

for(i in 4:12){
	ClusterLoc[,i] <- ifelse(is.na(ClusterLoc[,i]),0,ClusterLoc[,i])

}

ClusterLoc$siteTot <- rowSums(ClusterLoc[,4:12])

write.table(ClusterLoc,"c:\\Users\\hkropp\\Google Drive\\map_synth\\SitesClustered.txt",sep=",",row.names=FALSE)