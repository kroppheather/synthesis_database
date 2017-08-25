##########################################################################
##########Reads in Modis data output by Mike Loranty on ##################
##########11/30/16 and organizes EVI to match with stie ##################
########## and aggregate to average across observatons  ##################
##########################################################################
library(plyr)

#read in modis data
datM<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Modis_out.csv")

#read in siteinfo
datS<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\siteinfo.csv")

#get unique lat long
coord<-unique(data.frame(lat=datM$lat,long=datM$long))
#add coordinate id
coord$coordID<-seq(1,dim(coord)[1])

#now merge id into the the modis table
eviA<-join(datM,coord, by=c("lat", "long"), type="right")

#now aggregate by coordID
evim<-aggregate(eviA$max.band, by=list(eviA$coordID), FUN="mean")
evil<-aggregate(eviA$max.band, by=list(eviA$coordID), FUN="length")
evim$n<-evil$x

#now add to the coordinate table 

coord$EVI<-evim$x

#now match coordinates to the site info
#make a subsetted table
sites<-datS[,1:4]
colnames(sites)<-c("siteid", "name", "lat", "long")

eviSite<-join(sites,coord, by=c("lat","long"), type="right")

write.table(eviSite, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Site_EVI_out.csv",
			sep=",", row.names=FALSE)