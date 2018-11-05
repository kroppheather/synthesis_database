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

#organize into a matrix with NA for each day
#get each unique depth, year, site
datI <- unique(data.frame(wyear=datST$wyear,st_depth=datST$st_depth,site_id=datST$site_id))
datI$sID <- seq(1,dim(datI)[1])

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

}

scatterplot3d(Soil[[2]]$wdoy,Soil[[2]]$st_depth,Soil[[2]]$soil_t, type="l", ylim=c(0,20),angle=30)
points3d(Soil[[3]]$wdoy,Soil[[3]]$st_depth,Soil[[3]]$soil_t,type="l",add=TRUE)
