##############################################################################
######## This file reads in csv files of all data as of the 4th data upload
######## and looks at all temperature data
##############################################################################



###########################################
#########organize data for analysis########
###########################################
# set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\perma_analysis_backup")
#read in soil temperature
datS<-read.table("soil_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))
#need to create a list of dataframes for each site that combines air
#air temp with soil T, for each possible measurement for the day, year
#period of the data set.

#look into merge, zoo function for effiency to avoid a for loop

library(plyr)

#get number of sites
Nsite<-dim(siteinf)[1]

#merge all soil temp together but need to accomadate different conditions

tempT<-list()
S.all<-list()
Tlength<-numeric(0)
timedf<-list()

for(i in 1:Nsite){
#get the number of depths
	Tlength[i]<-length(unique(datS$st_depth[datS$site_id==i]))
	#filter out any sites that might try to loop for zero (sites with missing 
	#soil data which shouldn't be a condition for long)
	if(Tlength[i]!=0){
		#need to get all unique doy and year combinations in the dataset
		timedf[[i]]<-unique(data.frame(doy_st=datS$doy_st[datS$site_id==i], 
							year_st=datS$year[datS$site_id==i]))
		#next need to filter by if all depths are at unique times
		if(dim(timedf[[i]])[1]==dim(datS[datS$site_id==i,])[1]){
			#just include all unique measurements 
			S.all[[i]]<-datS[datS$site_id==i,]
		}else{
	
		#next need to find the depth with the most amount of measurements and 
		#order the depths based on this
			tempT[[1]]<-timedf[[i]]
				for(j in 1:Tlength[i]){
				tempT[[j+1]]<-datS[datS$site_id==i
					&datS$st_depth==unique(datS$st_depth[datS$site_id==i])[j],]
		
			}
		
	#merge all together
		S.all[[i]]<-join_all(tempT,by=c("doy_st", "year_st"), type="left")
		}
	}else{S.all[[i]]<-c(NA)}
}

#next merge air temperature in the same way
tempA<-list()
A.all<-list()
Alength<-numeric(0)
timedfa<-list()

for(i in 1:Nsite){
#get the number of depths
	Alength[i]<-length(unique(datA$air_height[datA$site_id==i]))
	
	#filter out any sites that might try to loop for zero (sites with missing 
	#air data which shouldn't be a condition for long)
	if(Alength[i]!=0){
		#need to get all unique doy and year combinations in the dataset
		timedfa[[i]]<-unique(data.frame(doy_st=datA$doy_st[datA$site_id==i], 
							year_st=datA$year_st[datA$site_id==i]))
		#next need to filter by if all depths are at unique times
		if(Alength[i]==1){
			#just include all unique measurements 
			A.all[[i]]<-datA[datA$site_id==i,]
		}else{
	
		#next need to find the depth with the most amount of measurements and 
		#order the depths based on this
			tempA[[1]]<-timedfa[[i]]
				for(j in 1:Alength[i]){
				tempA[[j+1]]<-datA[datA$site_id==i&datA$air_height==unique(datA$air_height[datA$site_id==i])[j],]
		
			}
	
	#merge all together
		A.all[[i]]<-join_all(tempA,by=c("doy_st", "year_st"), type="left")
		}

	}else{A.all[[i]]<-c(NA)}
}

#now air temperature needs to be merged with soil temperature
Temperature<-list()
for(i in 1:Nsite){
	Temperature[[i]]<-merge(S.all[[i]],A.all[[i]], by=c("doy_st", "year_st"))

}

