##########################################################################
##########################################################################
########### Data organization and plotting for all temp obs ##############
##########################################################################

# set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3")
#read in soil temperature
datS<-read.table("soil_temp_fixed_u6_out.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp_fixed_u6_out.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))

library(plyr)

#get number of sites
Nsite<-dim(siteinf)[1]
#first check to see if any sites are missing soil data
Tlength<-numeric(0)
for(i in 1:Nsite){
	#number of unique depths at each site
	Tlength[i]<-length(unique(datS$st_depth[datS$site_id==i]))
	}
	
#get sites with missing data 
Ts.missing<-which(Tlength==0)
	
#add a row of NA for any site missing data 

missing.mat<-matrix(rep(NA,6*length(Ts.missing)), nrow=length(Ts.missing))
for(i in 1:length(Ts.missing)){	
		
			missing.mat[i,6]<-Ts.missing[i]
}
colnames(missing.mat)<-colnames(datS)
datS<-rbind(datS,missing.mat)

#remove NAs from soil temp to avoid overly large datasets that are unnecessary
#this needs to be made more flexible for other data than this current round
datS<-datS[is.na(datS$soil_t)==FALSE|datS$site_id==Ts.missing[1]|datS$site_id==Ts.missing[2],]
 
#check the length again
 Tlength2<-numeric(0)
for(i in 1:Nsite){
	#number of unique depths at each site
	Tlength2[i]<-length(unique(datS$st_depth[datS$site_id==i]))
	}
	
#check that none are missing gain
Tlength2[Tlength2==0]

#check air temperature for missing data
TlengthA<-numeric(0)
for(i in 1:Nsite){
	#number of unique depths at each site
	TlengthA[i]<-length(unique(datA$air_height[datA$site_id==i]))
	}
	
#get sites with missing data 
Ta.missing<-which(TlengthA==0)
#add a row of NA for any site missing data 

missing.matA<-matrix(rep(NA,6*length(Ta.missing)), nrow=length(Ta.missing))
for(i in 1:length(Ta.missing)){	
			missing.matA[i,6]<-Ta.missing[i]
}
colnames(missing.matA)<-colnames(datA)
datA<-rbind(datA,missing.matA)



##Need some flags and to deal with duplicate depths and duplicated days before
#running this. 
NdaylengthS<-list()
duplicate<-list()
dupvec<-numeric(0)
NdaylengthA<-list()
duplicateA<-list()
dupvecA<-numeric(0)
#need to deal with sites with no data
vSiteFlag<-ifelse(Tlength==0,0,1)
for(i in 1:Nsite){
	if(vSiteFlag[i]==1){
		NdaylengthS[[i]]<-aggregate(datS$soil_t[datS$site_id==i], 
										by=list(datS$doy_st[datS$site_id==i],
										datS$year_st[datS$site_id==i],
										datS$st_depth[datS$site_id==i]),
										FUN="length")
	

		#check to see where there might be more than one observation								
		duplicate[[i]]<-NdaylengthS[[i]][NdaylengthS[[i]]$x>1,]
		dupvec[i]<-dim(duplicate[[i]])[1]

		#Air t check
		NdaylengthA[[i]]<-aggregate(datA$air_t[datA$site_id==i], 
										by=list(datA$doy_st[datA$site_id==i],
										datA$year_st[datA$site_id==i],
										datA$air_height[datA$site_id==i]),
										FUN="length")
		#check to see where there might be more than one observation		
	
		duplicateA[[i]]<-NdaylengthA[[i]][NdaylengthA[[i]]$x>1,]
		dupvecA[i]<-dim(duplicateA[[i]])[1]
	}else{
	NdaylengthA[[i]]<-0
	NdaylengthS[[i]]<-0}
	
}

siteflag.dup<-which(dupvec!=0)
siteflag.dupA<-which(dupvecA!=0)



#merge all soil temp together but need to accomadate different conditions

tempT<-list(list())
S.all<-list()
tempTuse<-list()
timedf<-list()

for(i in 1:Nsite){
#get the number of depths
	
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
			tempT<-list()
			tempT[[1]]<-timedf[[i]]
				for(j in 1:Tlength[i]){
				tempT[[j+1]]<-
				datS[datS$site_id==i&datS$st_depth==unique(datS$st_depth[datS$site_id==i])[j],]
					}


	#merge all together
		S.all[[i]]<-join_all(tempT,by=c("doy_st", "year_st"), type="left")
		}
	
}else{S.all[[i]]<-c(NA)}
}


				
				
#now deal with site 24 which is currently missing soil temp data
S.all[[24]]<-data.frame(doy_st=0,year_st=0,
						soil_t=NA,
						st_depth=NA)

					
S.all[[44]]<-data.frame(doy_st=0,year_st=0,
						soil_t=NA,
						st_depth=NA)
#check dimensions
S.dim<-list()
for(i in 1:Nsite){						
	S.dim[[i]]<-dim(S.all[[i]])
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
			tempA<-list()
			tempA[[1]]<-timedfa[[i]]
				for(j in 1:Alength[i]){
				tempA[[j+1]]<-datA[datA$site_id==i&datA$air_height==unique(datA$air_height[datA$site_id==i])[j],]
		
			}
	
	#merge all together
		A.all[[i]]<-join_all(tempA,by=c("doy_st", "year_st"), type="left")
		}

	}else{A.all[[i]]<-c(NA)}
}

#sites to exclude for now


A.all[[44]]<-data.frame(doy_st=0,year_st=0,
						air_t=NA,
						air_height=NA)
										
#check dimensions of A.all
A.dim=list()
for(i in 1:Nsite){
	A.dim[[i]]<-dim(A.all[[i]])
}										
						
						
#need a more efficient method than the for loop 
#running out of memory
#include all data since trying to look at as complete of a year as possible												
Temperature<-list()
for(i in 1:Nsite){
	Temperature[[i]]<-join(S.all[[i]],A.all[[i]], by=c("doy_st", "year_st"), type="full")

}

#check dim of temp

dimTemp<-numeric(0)
SdimV<-numeric(0)
for(i in 1:Nsite){
	dimTemp[i]<-dim(Temperature[[i]])[1]
	SdimV[i]<-S.dim[[i]][1]
}
TdimCheck<-dimTemp-SdimV


#####Need to add in further checks for data errors


################################################################################
################################################################################
################################################################################
#################### Now calculate N factors ###################################
################################################################################

#A closer look also shows site 48 has duplicate data
#All borehole sites need to be double checked for issues
#these rows will need to be deleted from the soil temperature 
#table and readded. 

#I am excluding the sites with issues in my preliminary analysis
#for the abstract, and waiting to properly fix them in the database.


#see if anysites are missing all temperature data
sitedimA<-list()
sitedimS<-list()
for(i in 1:Nsite){
	sitedimA[[i]]<-dim(A.all[[i]])
	sitedimS[[i]]<-dim(S.all[[i]])
}


#first create an index in each site that groups by time
#create 
dyear<-list()
dseq<-list()

#set up an index water year
#note: code below is similar to seasonal break in
#n factors, but it can actually be simplified
#down to an ifelse function. Should do this
# to speed things up and clean up code
######To add need to account for leap year
#set up 
leap.year<-data.frame(year=seq(1989,2016), leapID=rep(c(0,0,0,1), times=7))
leapY<-leap.year$year[leap.year$leapID==1]
colnames(leap.year)<-c("year_st", "leapID")
##########fix this
Temperaturedf<-ldply(Temperature,data.frame)

	TemperatureJ<-join(Temperaturedf,leap.year,by=c("year_st"),
							type="left")


TemperatureJ$wyear<-
		ifelse(TemperatureJ$leapID==1&TemperatureJ$doy_st<275,
					TemperatureJ$year_st,
		ifelse(TemperatureJ$leapID==0&TemperatureJ$doy_st<274,
					TemperatureJ$year_st,TemperatureJ$year_st+1))

		
		

	
###############
#get information on number of observations in a year for each site

	AirnoNA<-na.omit(data.frame(air_T=TemperatureJ$air_t,
									wyear=TemperatureJ$wyear,
									air_height=TemperatureJ$air_height,
									site_id=TemperatureJ$site_id))


	site.obsA<-aggregate(AirnoNA$air_T, by=
							list(
								AirnoNA$wyear,
								AirnoNA$air_height,
								AirnoNA$site_id), 
								FUN="length")

