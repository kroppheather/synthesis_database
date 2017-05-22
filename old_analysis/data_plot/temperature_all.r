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





######To add need to account for leap year
#set up 
leap.year<-data.frame(year=seq(1989,2016), leapID=rep(c(0,0,0,1), times=7))

colnames(leap.year)<-c("year_st", "leapID")


#add the leap year id and label the water year for air and soil

for(i in 1:Nsite){
	A.all[[i]]<-join(A.all[[i]],leap.year,by=c("year_st"),
							type="left")
A.all[[i]]$wyear<-
		ifelse(A.all[[i]]$leapID == 1 & A.all[[i]]$doy_st <  275,
				A.all[[i]]$year_st,
				ifelse(A.all[[i]]$leapID == 0 & A.all[[i]]$doy_st < 274,
						A.all[[i]]$year_st,A.all[[i]]$year_st+1))

}
sA<-list()

srepA<-numeric(0)
siteseq<-seq(1,Nsite)
for(i in 1:Nsite){
	
	srepA[i]<-length(which(colnames(A.all[[i]])=="air_height"))
	#make a dataframe of soil T and depth
	sA[[i]]<-data.frame(A=as.vector(data.matrix(A.all[[i]][,colnames(A.all[[i]])=="air_t"])),
	depth=as.vector(data.matrix(A.all[[i]][,colnames(A.all[[i]])=="air_height"])),
	doy=rep(A.all[[i]]$doy_st, times=srepA[i]),
	year=rep(A.all[[i]]$year_st, times=srepA[i]),
	wyear=rep(A.all[[i]]$wyear, times=srepA[i]),
	siteid=rep(siteseq[i],srepA[i]*length(A.all[[i]]$wyear)))
}



Air<-ldply(sA,data.frame)
Airnn<-na.omit(Air)
								

#see how many obserations for each year in a site
	
site.obsA<-aggregate(Airnn$A, by=
					list(Airnn$depth,		
						Airnn$siteid,
						Airnn$wyear), 
					FUN="length")


#now need to get the same for soil

for(i in 1:Nsite){
	S.all[[i]]<-join(S.all[[i]],leap.year,by=c("year_st"),
							type="left")
S.all[[i]]$wyear<-
		ifelse(S.all[[i]]$leapID == 1 & S.all[[i]]$doy_st <  275,
				S.all[[i]]$year_st,
				ifelse(S.all[[i]]$leapID == 0 & S.all[[i]]$doy_st < 274,
						S.all[[i]]$year_st,S.all[[i]]$year_st+1))

}
sS<-list()

srepS<-numeric(0)
siteseq<-seq(1,Nsite)
for(i in 1:Nsite){
	
	srepS[i]<-length(which(colnames(S.all[[i]])=="st_depth"))
	#make a dataframe of soil T and depth
	sS[[i]]<-data.frame(T=as.vector(data.matrix(S.all[[i]][,colnames(S.all[[i]])=="soil_t"])),
	depth=as.vector(data.matrix(S.all[[i]][,colnames(S.all[[i]])=="st_depth"])),
	doy=rep(S.all[[i]]$doy_st, times=srepS[i]),
	year=rep(S.all[[i]]$year_st, times=srepS[i]),
	wyear=rep(S.all[[i]]$wyear, times=srepS[i]),
	siteid=rep(siteseq[i],srepS[i]*length(S.all[[i]]$wyear)))
}



Soil<-ldply(sS,data.frame)
Soilnn<-na.omit(Soil)
								

#see how many obserations for each year in a site
	
site.obsS<-aggregate(Soilnn$T, by=
					list(Soilnn$depth,		
						Soilnn$siteid,
						Soilnn$wyear), 
					FUN="length")

#start by focusing on sites with at least 75% of the year
colnames(site.obsS)<-c("depth", "siteid","wyear","n")
colnames(site.obsA)<-c("height", "siteid","wyear","n")

#how many air temp heights are there
min(unique(site.obsA$height))
max(unique(site.obsA$height))

#filter sites to use by observations that aree 75% of the 
#year and have an air temp height of 1 m at least
nfilter<-floor(365*.75)
site.obsAf<-site.obsA[site.obsA$height>=1&site.obsA$n>=nfilter,]

site.obsSf<-site.obsS[site.obsS$n>=nfilter,]
colnames(site.obsAf)<-c("depth", "siteid","wyear","n")

#now join back with full data frame to apply filter
SoilS<-join(Soil,site.obsSf,by=c("depth","siteid","wyear"), type="inner")
AirS<-join(Air,site.obsAf,by=c("depth","siteid","wyear"), type="inner")

#now look at soil temperature in plots
#make a date column that uses day of year as a decimal place
#put a little bit of space between years
SoilS$decdate<-ifelse(SoilS$year!=SoilS$wyear,
				(SoilS$wyear-1991)+(SoilS$doy-274)/367,
				(SoilS$wyear-1991)+(SoilS$doy+91)/367)
plot(SoilS$decdate[SoilS$depth<=10],SoilS$T[SoilS$depth<=10],pch=19)

#looks like a -999 got lefr in a data set so exclude n
SoilS<-SoilS[SoilS$T>-50,]
plot(SoilS$decdate[SoilS$depth<=10],SoilS$T[SoilS$depth<=10],pch=19)


#add in site information


soilsin<-function(Ta,Daysd,Ao){
		Ta+(Ao*sin(-2*3.14*Daysd))
		}
		
soilsin2<-function(Ta,Daysd,Ao,D0){
		Ta+(Ao*sin(-2*3.14*Daysd-D0))
		}
plotyear<-rep(seq(1991,2016), each=365)
plotdoy<-rep(seq(1,365), times=26)
plotseq<-(plotyear-1991)+(plotdoy/365)

plot(SoilS$decdate[SoilS$depth<=10],SoilS$T[SoilS$depth<=10],pch=19)
points(plotseq,soilsin(-5,plotdoy/365,.5), type="l",col="red")
abline(h=0)
points(plotseq,soilsin2(-5,plotdoy/365,25,-5), type="l",col="red")

plot(SoilS$decdate[SoilS$siteid==22],SoilS$T[SoilS$siteid==22],pch=19)
points(plotseq,soilsin2(0,plotdoy/365,20,0), type="l",col="red")