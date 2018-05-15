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
datBuffer <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\Buffer70k_all_sites.txt")

#subset and rename for simpler table
datB <- data.frame(bufferID=datBuffer$TARGET_FID,lat=datBuffer$CENTROID_Y,long=datBuffer$CENTROID_X,site_id=datBuffer$site_id,
					vegeclass=datBuffer$vege_class)
					
					
#get the count of vege class in each buffer
vegeCount <- aggregate(datB$bufferID, by=list(datB$vegeclass,datB$bufferID), FUN="length") 
colnames(vegeCount) <- c("vegeclass","bufferID","count")	

#get a unique buffer id lat long

buffer <- unique(data.frame(bufferID=datB$bufferID, lat=datB$lat,long=datB$long))			

#get counts of each vege type
vegeL <- list()
for(i in 1:9){
	vegeL[[i]] <- vegeCount[vegeCount$vegeclass==i,2:3]
	colnames(vegeL[[i]])[2] <- paste0("count",i)
	buffer <- join(buffer, vegeL[[i]],by="bufferID", type="left")
}

#convert na to zero
for(i in 4:12){
	buffer[,i] <- ifelse(is.na(buffer[,i]),0,buffer[,i])

}

#add up number of sites
buffer$stieTot <- rowSums(buffer[,4:12])


write.table(buffer ,"c:\\Users\\hkropp\\Google Drive\\map_synth\\Buffer70k_siteCount.csv",sep=",",row.names=FALSE)
