##########################################################################
##########################################################################
########### Data plotting for all temp obs                  ##############
##########################################################################

library(plyr)
library(lubridate)
library(scatterplot3d)

# set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6")
#read in soil temperature
datS<-read.table("soil_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in model run status
modrun <- read.csv("site_model_statusr5.csv")
#vegetation class
datV <- read.csv("vege_class.csv")
colnames(datV)[1] <- "site_id"
#plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\figure"

#filter soil temp to only focus on sites with 20cm or less depth
datST <- datS[datS$st_depth<=20,]
#omit site 44 which has problems
datST <- datST[datST$site_id!=44,]



# set up water year
#create a leap year flag
datST$leapid<-ifelse(leap_year(datST$year)==TRUE,1,0)


datST$wdoy<-ifelse(datST$leapid==1&datST$doy<=274, datST$doy+92,
		ifelse(datST$leapid==1&datST$doy>274, datST$doy-274,
		ifelse(datST$leapid==0&datST$doy<=273,datST$doy+92,
		ifelse(datST$leapid==0&datST$doy>273,datST$doy-273,NA))))
		
datST$wyear <- ifelse(datST$leapid == 1 & datST$doy_st <  275,
				datST$year_st,
				ifelse(datST$leapid == 0 & datST$doy_st < 274,
						datST$year_st,datST$year_st+1))	
						
						
#add vegetation class	
datST <- join(datST,datV,by="site_id",type="left")					

#organize into a matrix with NA for each day
#get each unique depth, year, site
datI <- unique(data.frame(wyear=datST$wyear,st_depth=datST$st_depth,site_id=datST$site_id))
datI$sID <- seq(1,dim(datI)[1])

#join vegeclass
datI <- join(datI,datV,by="site_id",type="left")

#join index back into datST

datST <- join(datST,datI, by=c("wyear","st_depth","site_id"),type="left")	

#create a list of dataframes where each year is filled in with NA for plotting		
dayAll <- data.frame(wdoy=seq(1,366))			

#list of all sites
Soilt <- list()
Soil <- list()

for(i in 1:dim(datI)[1]){
	Soilt[[i]] <- datST[datST$sID==i,]
	Soil[[i]] <- join(Soilt[[i]],dayAll,by="wdoy",type="full")
	Soil[[i]] <- Soil[[i]][order(Soil[[i]]$wdoy),]


}

#get a list of which lists belong to each vegeclass
listV <- list()

for(i in 1:9){
	listV[[i]] <- which(datI$vegeclass==i)
}

for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual3d_class",i,".jpg"),width=1000,height=1000,quality=100)
		s3d <- scatterplot3d(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$st_depth,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(0,20),xlim=c(0,370),zlim=c(-35,35),angle=60,color=rgb(205/255,79/255,57/255,.4),
				xlab="day of year",ylab="depth",zlab="temperature",box=FALSE)
		for(j in 1:length(listV[[i]])){			
			s3d$points3d(Soil[[listV[[i]][j]]]$wdoy,Soil[[listV[[i]][j]]]$st_depth,Soil[[listV[[i]][j]]]$soil_t,type="l",col=rgb(205/255,79/255,57/255,.4))
		}
	dev.off()
}	
test <- c(rgb(86/255,180/255,233/255,.2),rgb(0/255,158/255,115/255,.2),
		rgb(213/255,94/255,0/255,.2),rgb(240/255,228/255,66/255,.2))
for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual2d_class",i,".jpg"),width=1000,height=1000,quality=100)
plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-35,35),xlim=c(0,370),col=rgb(205/255,79/255,57/255,.4),
				xlab="day of year",ylab="temperature")
		for(j in 1:length(listV[[i]])){		
			if(datI$st_depth[listV[[i]][j]]<=5){
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
					col=test[1],lwd=3)
			}
			if(datI$st_depth[listV[[i]][j]]>5&datI$st_depth[listV[[i]][j]]<=10){	
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
				col=test[2],lwd=3)	
				}
			if(datI$st_depth[listV[[i]][j]]>10&datI$st_depth[listV[[i]][j]]<=15){	
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
				col=test[3],lwd=3)	
				}	
			if(datI$st_depth[listV[[i]][j]]>15&datI$st_depth[listV[[i]][j]]<=20){	
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
				col=test[4],lwd=3)	
				}	
		}		
	dev.off()	
}		