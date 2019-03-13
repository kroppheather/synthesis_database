##########################################################################
##########################################################################
########### Data organization and plotting for all temp obs ##############
##########################################################################
########### Update 1/31/18. Many sites have good temperature##############
########### model runs. Some sites have poor mixing and need##############
########### more thinning. There is also a new data addition##############
########### This script does not rerun temperature models   ##############
########### for sites that don't need it.                   ##############
##########################################################################
library(plyr,lib.loc="/home/hkropp/R")
library(lubridate,lib.loc="/home/hkropp/R")
library(rjags)
library(coda,lib.loc="/home/hkropp/R")
library(xtable,lib.loc="/home/hkropp/R")
library(mcmcplots,lib.loc="/home/hkropp/R")



#########################
#set up model run  ######
#########################
#run number
rn <- 17
#output dir
outDir <- "/home/hkropp/synthesis/output_u10/run17"

#########################
#read in data      ######
#########################
# set working directory
#from db backup 6
setwd("/home/hkropp/synthesis/data")
#read in soil temperature
datS<-read.table("soil_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))


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
#need to account for the unknown Air temp height at Prudoe bay
#data was taken from a met station where it is likely 2m but
#can't confirm. Going to mark as a number so doesn't get omitted
#with the rest of nas siteid 76-92 will have a na for the air temp
#height
#just give a number so setting to 1
Air$depth<-ifelse(Air$siteid>=76&Air$siteid<=92,1,Air$depth)
#omit NA
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

#now exclude all depths that are more than 20cm
SoilS<-SoilS[SoilS$depth<=20,]


AirS<-join(Air,site.obsAf,by=c("depth","siteid","wyear"), type="inner")


#now look at unique year
wyearA<-unique(AirS$wyear)
wyearS<-unique(SoilS$wyear)

#set up an index for dealing with leap year in water
#years
#need to divide the fall by 366 for leap year
#just set up a dec date sequence and then match

#create a leap year flag
SoilS$leapid<-ifelse(leap_year(SoilS$year)==TRUE,1,0)


SoilS$wdoy<-ifelse(SoilS$leapid==1&SoilS$doy<=274, SoilS$doy+92,
		ifelse(SoilS$leapid==1&SoilS$doy>274, SoilS$doy-274,
		ifelse(SoilS$leapid==0&SoilS$doy<=273,SoilS$doy+92,
		ifelse(SoilS$leapid==0&SoilS$doy>273,SoilS$doy-273,NA))))
#the .01 is added because day 273 needs to be included in the water year, but if it 
#is exactly one that bumps it to the first day of the water year.
SoilS$wdoyP<-ifelse(leap_year(SoilS$wyear)==TRUE, (SoilS$wdoy-1)/366,(SoilS$wdoy-1)/365 )

#now add to the year		
				
SoilS$decdate<-SoilS$wyear+SoilS$wdoyP



plot(SoilS$decdate[SoilS$depth<=20],SoilS$T[SoilS$depth<=20],pch=19)
#exclude outlier that is clearly a -999 output from an instrument



#now do air temperature
#create a leap year flag
AirS$leapid<-ifelse(leap_year(AirS$year)==TRUE,1,0)


AirS$wdoy<-ifelse(AirS$leapid==1&AirS$doy<=274, AirS$doy+92,
		ifelse(AirS$leapid==1&AirS$doy>274, AirS$doy-274,
		ifelse(AirS$leapid==0&AirS$doy<=273,AirS$doy+92,
		ifelse(AirS$leapid==0&AirS$doy>273,AirS$doy-273,NA))))
#the .01 is added because day 273 needs to be included in the water year, but if it 
#is exactly one that bumps it to the first day of the water year.
AirS$wdoyP<-ifelse(leap_year(AirS$wyear)==TRUE, (AirS$wdoy-1)/366,(AirS$wdoy-1)/365 )

#now add to the year		
				
AirS$decdate<-AirS$wyear+AirS$wdoyP

plot(AirS$decdate,AirS$A,pch=19)


#more -999 from instrument error 
#6999 
#exclude these from analysis
AirS<-na.omit(AirS[AirS$A> -999,])
AirS<-AirS[AirS$A<50,]
plot(AirS$decdate,AirS$A,pch=19)


#need to get unique site and depth for air temperature
AirIDS<-unique(data.frame(height=AirS$depth, siteid=AirS$siteid, wyear=AirS$wyear))
AirIDS$siteC<-seq(1,dim(AirIDS)[1])


#need to get unique site and depth for soil temperature
SoilIDS<-unique(data.frame(depth=SoilS$depth, siteid=SoilS$siteid, wyear=SoilS$wyear ))
SoilIDS$siteCS<-seq(1,dim(SoilIDS)[1])

#now match up with air siteid to only take sites with air measurements to accompany
ALLIDS<-join(SoilIDS,AirIDS, by=c("siteid","wyear"), type="inner")
#the sites here are the only sites to use

#now get a unique site id data frame
SitesALL<-data.frame(siteid=unique(ALLIDS$siteid))
SitesALL$siteM<-seq(1, dim(SitesALL)[1])

#see if anyone measured air temp at multiple heights
AirDN1<-aggregate(ALLIDS$height,by=list(ALLIDS$height,ALLIDS$siteid), FUN=length)
AirDN<-aggregate(AirDN1$Group.1, by=list(AirDN1$Group.2), FUN="length")
which(AirDN$x>1)


#will need an air depth ID still
#need to calculate new IDS based on ALL ID. 
#get unique soil IDS
SoilIDS2<-unique(data.frame(siteid=ALLIDS$siteid, depth=ALLIDS$depth, wyear=ALLIDS$wyear))
SoilIDS2$SDWS<-seq(1,dim(SoilIDS2)[1])

site.depthidS<-unique(data.frame(siteid=SoilIDS2$siteid,depth=SoilIDS2$depth))
site.depthidS$SDS<-seq(1,dim(site.depthidS)[1])



#now do the same for air temperature
AirIDS2<-unique(data.frame(siteid=ALLIDS$siteid, height=ALLIDS$height, wyear=ALLIDS$wyear))
AirIDS2$SDWA<-seq(1,dim(AirIDS2)[1])

site.heightA<-unique(data.frame(siteid=AirIDS2$siteid, height=AirIDS2$height))
site.heightA$SDA<-seq(1,dim(site.heightA)[1])

colnames(AirS)[2]<-"height"


#join sitedepth data

AIRIDNEWALL<-join(AirIDS2, site.heightA, by=c("siteid", "height"), type="left" )
SOILIDNEWALL<-join(SoilIDS2, site.depthidS, by=c("siteid", "depth"), type="left")

#now need to create the vector for getting the complete years
#Air
all.wdoyA<-list()
for(i in 1:dim(AirIDS2)[1]){
	if(leap_year(AirIDS2$wyear[i])){
		all.wdoyA[[i]]<-data.frame(wdoy=c(rep(AirIDS2$wyear[i],366)+seq(0/366,365/366,length.out=366)),
									wyear=rep(AirIDS2$wyear[i],366),
									siteid=rep(AirIDS2$siteid[i],366),
									height=rep(AirIDS2$height[i],366),
									SDWA=rep(AirIDS2$SDWA[i],366),
									SDS=rep(AIRIDNEWALL$SDA[i],366))
	}else{
		all.wdoyA[[i]]<-data.frame(wdoy=c(rep(AirIDS2$wyear[i],365)+seq(0/365,364/365,length.out=365)),
									wyear=rep(AirIDS2$wyear[i],365),
									siteid=rep(AirIDS2$siteid[i],365),
									height=rep(AirIDS2$height[i],365),
									SDWA=rep(AirIDS2$SDWA[i],365),
									SDS=rep(AIRIDNEWALL$SDA[i],365))	
		}
}
#now turn into a data frame
A.DOY<-ldply(all.wdoyA)
#Soil
all.wdoyS<-list()
for(i in 1:dim(SoilIDS2)[1]){
	if(leap_year(SoilIDS2$wyear[i])){
		all.wdoyS[[i]]<-data.frame(wdoy=c(rep(SoilIDS2$wyear[i],366)+seq(0/366,365/366,length.out=366)),
									wyear=rep(SoilIDS2$wyear[i],366),
									siteid=rep(SoilIDS2$siteid[i],366),
									depth=rep(SoilIDS2$depth[i],366),
									SDWS=rep(SoilIDS2$SDWS[i],366),
									SDS=rep(SOILIDNEWALL$SDS[i],366))
	}else{
		all.wdoyS[[i]]<-data.frame(wdoy=c(rep(SoilIDS2$wyear[i],365)+seq(0/365,364/365,length.out=365)),
									wyear=rep(SoilIDS2$wyear[i],365),
									siteid=rep(SoilIDS2$siteid[i],365),
									depth=rep(SoilIDS2$depth[i],365),
									SDWS=rep(SoilIDS2$SDWS[i],365),
									SDS=rep(SOILIDNEWALL$SDS[i],365))	
		}
}
#now turn into a data frame
S.DOY<-ldply(all.wdoyS)
colnames(S.DOY)[1]<-"decdate"
colnames(A.DOY)[1]<-"decdate"
#now join with soilM2 to fill in missing days of year
#note also in this join soil and air temps with out a pair will be dropped
#since they have already been dropped from the 
SoilM2<-join(S.DOY, SoilS, by=c("decdate", "depth","wyear","siteid"), type="left")

AirM2<-join(A.DOY, AirS, by=c("decdate","height","siteid","wyear"), type="left" )






############################################
#this will need to change if 
#using the model for temp

#turn Air M2 into a list
#get the unique sites list
sitesS<-unique(data.frame(siteid=AirM2$siteid))
sitesS$siteUID<-seq(1,dim(sitesS)[1])


AirSitesD<-list()
SoilSitesD<-list()
AirSDW<-list()
SoilSDW<-list()
AirSD<-list()
SoilSD<-list()
AirSitesD2<-list()
SoilSitesD2<-list()
IDtest<-list()
IDtestA<-list()
#pull out data in data frame to a list
for(i in 1:dim(sitesS)[1]){
	#pull out temperature by site
	AirSitesD[[i]]<-AirM2[AirM2$siteid==sitesS$siteid[i],]
	SoilSitesD[[i]]<-SoilM2[SoilM2$siteid==sitesS$siteid[i],]
	#pull out IDS by site
	AirSDW[[i]]<-AirIDS2[AirIDS2$siteid==sitesS$siteid[i],]
	AirSDW[[i]]$siteSDW<-seq(1,dim(AirSDW[[i]])[1])
	SoilSDW[[i]]<-SoilIDS2[SoilIDS2$siteid==sitesS$siteid[i],]
	SoilSDW[[i]]$siteSDW<-seq(1,dim(SoilSDW[[i]])[1])	
	#join IDs to data to get site ids
	AirSitesD2[[i]]<-join(AirSitesD[[i]],AirSDW[[i]], by=c("siteid","height","wyear","SDWA"), type="left")
	SoilSitesD2[[i]]<-join(SoilSitesD[[i]],SoilSDW[[i]], by=c("siteid","depth","SDWS"),type="left")
	SoilSitesD2[[i]]$TyrS<-SoilSitesD2[[i]]$decdate-SoilSitesD2[[i]]$wyear
	AirSitesD2[[i]]$TyrA<-AirSitesD2[[i]]$decdate-AirSitesD2[[i]]$wyear
	SoilSitesD2[[i]]$indexI<-seq(1,dim(SoilSitesD2[[i]])[1])
	AirSitesD2[[i]]$indexI<-seq(1,dim(AirSitesD2[[i]])[1])
	#now get the year for the tave2
	IDtest[[i]]<-SoilSDW[[i]][,1:3]
	IDtest[[i]]$wyear2<-SoilSDW[[i]]$wyear+1
	#for air
	IDtestA[[i]]<-AirSDW[[i]][,1:3]
	IDtestA[[i]]$wyear2<-AirSDW[[i]]$wyear+1
	}

#get all unique years in  a possible depth and year combination
IDstep2<-list()
IDstep3<-list()
IDstep2A<-list()
IDstep3A<-list()
for(i in 1:dim(sitesS)[1]){	
		#get all possible unique years for a start and end
		IDstep2[[i]]<-unique(data.frame(siteid=c(IDtest[[i]]$siteid,IDtest[[i]]$siteid), 
										depth=c(IDtest[[i]]$depth,IDtest[[i]]$depth),
										wyear=c(IDtest[[i]]$wyear,IDtest[[i]]$wyear2)))
		#order
		IDstep2[[i]]<-IDstep2[[i]][order(IDstep2[[i]]$depth,IDstep2[[i]]$wyear),]
		#create a sequence for the number of years we estimate the Temp at the beginning
		IDstep2[[i]]$Tave1ID<-seq(1, dim(IDstep2[[i]])[1])
		#duplicate with matching names for end year
		IDstep3[[i]]<-IDstep2[[i]]
		colnames(IDstep3[[i]])[3]<-"wyear2"
		colnames(IDstep3[[i]])[4]<-"Tave2D"
		##now for air
		#get all possible unique years for a start and end
		IDstep2A[[i]]<-unique(data.frame(siteid=c(IDtestA[[i]]$siteid,IDtestA[[i]]$siteid), 
										height=c(IDtestA[[i]]$height,IDtest[[i]]$height),
										wyear=c(IDtestA[[i]]$wyear,IDtestA[[i]]$wyear2)))
		#order
		IDstep2A[[i]]<-IDstep2A[[i]][order(IDstep2A[[i]]$height,IDstep2A[[i]]$wyear),]
		#create a sequence for the number of years we estimate the Temp at the beginning
		IDstep2A[[i]]$Tave1IDA<-seq(1, dim(IDstep2A[[i]])[1])
		#duplicate with matching names for end year
		IDstep3A[[i]]<-IDstep2A[[i]]
		colnames(IDstep3A[[i]])[3]<-"wyear2"
		colnames(IDstep3A[[i]])[4]<-"Tave2D"	
}

#now join to an ID table for getting Tave 1 IDS
Tave1ID<-list()
Tave2ID<-list()
Tave1IDA<-list()
Tave2IDA<-list()
SoilSitesD3<-list()
AirSitesD3<-list()
for(i in 1:dim(sitesS)[1]){	
	#this will add an id for Tave 1
	Tave1ID[[i]]<-join(IDtest[[i]], IDstep2[[i]], by=c("siteid","depth","wyear"), type="left")
	#this will add an id for Tave 2
	Tave2ID[[i]]<-join(Tave1ID[[i]], IDstep3[[i]], by=c("siteid","depth","wyear2"), type="left")
	##now for air
		#this will add an id for Tave 1
	Tave1IDA[[i]]<-join(IDtestA[[i]], IDstep2A[[i]], by=c("siteid","height","wyear"), type="left")
	#this will add an id for Tave 2
	Tave2IDA[[i]]<-join(Tave1IDA[[i]], IDstep3A[[i]], by=c("siteid","height","wyear2"), type="left")
	#now join back into Air and soil data
	SoilSitesD3[[i]]<-join(Tave2ID[[i]],SoilSitesD2[[i]], by=c("siteid","depth","wyear"), type="right")
	AirSitesD3[[i]]<-join(Tave2IDA[[i]],AirSitesD2[[i]], by=c("siteid","height","wyear"), type="right")
	
}

ALLSyearID<-ldply(Tave2ID,data.frame)
ALLAyearID<-ldply(Tave2IDA,data.frame)



#############################
###############now need to get degree days
###############to sum over.
#create index for soil
SSc<-list()
for(i in 1:dim(sitesS)[1]){	
	SSc[[i]]<-data.frame(SYlength=rep(0,dim(SoilSDW[[i]])[1]))
	SSc[[i]]$SSY<-rep(0,dim(SoilSDW[[i]])[1])
	SSc[[i]]$SEY<-rep(0,dim(SoilSDW[[i]])[1])
	
	for(j in 1:dim(SoilSDW[[i]])[1]){
		SSc[[i]]$SYlength[j]<-length(SoilSitesD3[[i]]$indexI[SoilSitesD3[[i]]$siteSDW==SoilSDW[[i]]$siteSDW[j]])
		SSc[[i]]$SSY[j]<-SoilSitesD3[[i]]$indexI[SoilSitesD3[[i]]$siteSDW==SoilSDW[[i]]$siteSDW[j]][1]
		SSc[[i]]$SEY[j]<-SoilSitesD3[[i]]$indexI[SoilSitesD3[[i]]$siteSDW==SoilSDW[[i]]$siteSDW[j]][SSc[[i]]$SYlength[j]]
	}

}

#now create index for air
ASc<-list()
for(i in 1:dim(sitesS)[1]){

	ASc[[i]]<-data.frame(AYlength=rep(0,dim(AirSDW[[i]])[1]))
	ASc[[i]]$ASY<-rep(0,dim(AirSDW[[i]])[1])
	ASc[[i]]$AEY<-rep(0,dim(AirSDW[[i]])[1])
	for(j in 1:dim(AirSDW[[i]])[1]){
	ASc[[i]]$AYlength[j]<-length(AirSitesD3[[i]]$indexI[AirSitesD3[[i]]$siteSDW==AirSDW[[i]]$siteSDW[j]])
	ASc[[i]]$ASY[j]<-AirSitesD3[[i]]$indexI[AirSitesD3[[i]]$siteSDW==AirSDW[[i]]$siteSDW[j]][1]
	ASc[[i]]$AEY[j]<-AirSitesD3[[i]]$indexI[AirSitesD3[[i]]$siteSDW==AirSDW[[i]]$siteSDW[j]][ASc[[i]]$AYlength[j]]
	}
	
}
#Need to make an index for N factors that compares each depth against each air for every year

#Next need to create an index that says when sites should be divided
#want to divide each soil
#want each soil to be divided by each air combo, but only in a year

IDforCombo<-list()
AirSDW2<-AirSDW
for(i in 1:dim(sitesS)[1]){
	colnames(AirSDW2[[i]])[5]<-"siteSDWA"
	IDforCombo[[i]]<-join(AirSDW2[[i]], SoilSDW[[i]],by=c("siteid", "wyear"), type="inner")
	IDforCombo[[i]]$Nseq<-seq(1,dim(IDforCombo[[i]])[1])
}

IDnCombo<-ldply(IDforCombo, data.frame)

#finally need to set up a flag for when a temperature measurement should not be used in 
#a site
SoilSDW3<-SoilSDW
SSc2<-SSc
CHFlag<-list()
for(i in 1:dim(sitesS)[1]){
	#set up a flag if the year jumps more than one or the depth changes
	SoilSDW3[[i]]$sFlag<-rep(0,dim(SoilSDW[[i]])[1])
	SoilSDW3[[i]]$sFlag[1]<-1
	if(dim(SoilSDW3[[i]])[1]>1){
		for(j in 2:dim(SoilSDW[[i]])[1]){
			if(SoilSDW3[[i]]$depth[j]!=SoilSDW3[[i]]$depth[j-1]|(SoilSDW3[[i]]$wyear[j]-SoilSDW3[[i]]$wyear[j-1])!=1){
				SoilSDW3[[i]]$sFlag[j]<-1
			}
		}
	}
	#now match with the corresponding row in the data using the SSC
	SSc2[[i]]$changeFlag<-SoilSDW3[[i]]$sFlag
	CHFlag[[i]]<-data.frame(ToFlag=SSc2[[i]]$SSY[SSc2[[i]]$changeFlag==1])
	CHFlag[[i]]$FlagSeq<-seq(1,dim(CHFlag[[i]])[1])
}
#now just need to make the flag variable in the soil
for(i in 1:dim(sitesS)[1]){
	SoilSitesD3[[i]]$startFlag<-rep(1, dim(SoilSitesD3[[i]])[1])
	SoilSitesD3[[i]]$startFlag[CHFlag[[i]]$ToFlag]<- -1
	SoilSitesD3[[i]]$startSeq<-rep(1, dim(SoilSitesD3[[i]])[1])
	SoilSitesD3[[i]]$startSeq[CHFlag[[i]]$ToFlag]<-CHFlag[[i]]$FlagSeq
	#now get the number of start variables as an index
	#let the default index go to one since the first
	#row is specifically accounted for as one
	#and the step function will make it impossible for data to account for it
	
}

####Sample reps per site. Sample the full data if it is less than 2000, other wise  

Soilrepsub<-list()
for(i in 1:dim(sitesS)[1]){
	if(dim(SoilSitesD3[[i]])[1]<=2000){
	Soilrepsub[[i]]<-data.frame(repID=SoilSitesD3[[i]]$indexI)
	Soilrepsub[[i]]$siteid<-rep(sitesS$siteid[i], dim(Soilrepsub[[i]])[1])
	
	}else{if(dim(SoilSitesD3[[i]])[1]>2000){

			Soilrepsub[[i]]<-data.frame(repID=sample(SoilSitesD3[[i]]$indexI,2000))
			Soilrepsub[[i]]$siteid<-rep(sitesS$siteid[i], dim(Soilrepsub[[i]])[1])
			
		}
		
	}
}


Airrepsub<-list()
for(i in 1:dim(sitesS)[1]){
	if(dim(AirSitesD3[[i]])[1]<=2000){
	Airrepsub[[i]]<-data.frame(repID=AirSitesD3[[i]]$indexI)
	Airrepsub[[i]]$siteid<-rep(sitesS$siteid[i], dim(Airrepsub[[i]])[1])
	
	}else{if(dim(AirSitesD3[[i]])[1]>2000){

			Airrepsub[[i]]<-data.frame(repID=sample(AirSitesD3[[i]]$indexI,2000))
			Airrepsub[[i]]$siteid<-rep(sitesS$siteid[i], dim(Airrepsub[[i]])[1])
		}
		
	}
}


#write SoilSites and air sites to table
#need to break up into two runs because saving too much data to home
#renaming everything to be run two
Airrepsubout<-ldply(Airrepsub, data.frame)
Soilrepsubout<-ldply(Soilrepsub, data.frame)
write.table(Airrepsubout,paste0(outDir,"/AirrepIDS.csv"),sep=",",row.names=FALSE)
write.table(Soilrepsubout,paste0(outDir,"/SoilrepIDS.csv"),sep=",",row.names=FALSE)

print("repID out")

Airtowriteout<-ldply(AirSitesD3, data.frame)
Soiltowriteout<-ldply(SoilSitesD3, data.frame)
#model
write.table(Airtowriteout,paste0(outDir,"/Tair_model.csv"),sep=",",row.names=FALSE)
write.table(Soiltowriteout,paste0(outDir,"/Tsoil_model.csv"),sep=",",row.names=FALSE)




#need to write ids to table

write.table(AirIDS2,paste0(outDir,"/AirIDS.csv"), sep=",", row.names=FALSE)
write.table(SoilIDS2,paste0(outDir,"/SoilIDS.csv"), sep=",", row.names=FALSE)

write.table(ALLSyearID,paste0(outDir,"/SoilTaveIDS_SD.csv"), sep=",", row.names=FALSE)
write.table(ALLAyearID,paste0(outDir,"/AirTaveIDS_SD.csv"), sep=",", row.names=FALSE)


write.table(IDnCombo,paste0(outDir,"/ncomboIDS.csv"), sep=",", row.names=FALSE)


print("ID write out")	
#designate the 	samples to run			
samplelist<-c("T.aveA1","TminA","TmaxA","T.aveS1","TmaxS","TminS","sig.muA","sig.muS",
				 "muS","muA", "aZero", "bZero", "zeroC", "peakWS", "peakWA", "peakSS", "peakSA",
				 "TempS.rep", "TempA.rep","pstart","Fn", "Tn","FDDA","TDDA","TDDS","FDDS", "DayZero",
				 "TaverageS","TaverageA","ThawCountN")

#divide into 10 runs
if(rn ==1){
	modRun <- sitesS[1:7,]
}
if(rn ==2){
	modRun <- sitesS[21:25,]
}
if(rn == 3){
	modRun <- sitesS[41:52,]
}
if(rn == 4){
	modRun <- sitesS[61:77,]
}
if(rn == 5){
	modRun <- sitesS[81:88,]
}		
if(rn == 6){
	modRun <- sitesS[101:114,]
}
if(rn ==7){
	modRun <- sitesS[8:10,]
}
if(rn ==8){
	modRun <- sitesS[11:15,]
}		 
if(rn ==9){
	modRun <- sitesS[15:20,]
}

if(rn ==10){
	modRun <- sitesS[26:30,]
}
if(rn ==11){
	modRun <- sitesS[31:35,]
}
if(rn ==12){
	modRun <- sitesS[35:40,]
}

if(rn ==13){
	modRun <- sitesS[52:55,]
}
if(rn ==14){
	modRun <- sitesS[55:60,]
}
if(rn ==15){
	modRun <- sitesS[78:80,]
}
if(rn ==16){
	modRun <- sitesS[89:95,]
}
if(rn ==17){
	modRun <- sitesS[95:100,]
}
#for(i in 1:dim(sitesS)[1]){
for(k in 1:dim(modRun)[1]){
#make the data list for the model
	i <- modRun$siteUID[k]
datalist<-list(NobsA=dim(AirSitesD3[[i]])[1], TempA=AirSitesD3[[i]]$A, 
				T.yrA=AirSitesD3[[i]]$TyrA,
				NobsS=dim(SoilSitesD3[[i]])[1], TempS=SoilSitesD3[[i]]$T,
				 T.yrS=SoilSitesD3[[i]]$TyrS,
				 NSDWA=dim(AirSDW[[i]])[1],
				NSDWS=dim(SoilSDW[[i]])[1], SDWS=SoilSitesD3[[i]]$siteSDW, SDWA=AirSitesD3[[i]]$siteSDW,
				NTaveS=dim(IDstep2[[i]])[1],NTaveA=dim(IDstep2A[[i]])[1],
				IDAS=AirSitesD3[[i]]$Tave1IDA,IDAE=AirSitesD3[[i]]$Tave2D, IDSS=SoilSitesD3[[i]]$Tave1ID,
				IDSE=SoilSitesD3[[i]]$Tave2D, startflag=SoilSitesD3[[i]]$startFlag,startSub=SoilSitesD3[[i]]$startSeq,
				NrepS=dim(Soilrepsub[[i]])[1],NrepA=dim(Airrepsub[[i]])[1],
				SrepSub=Soilrepsub[[i]]$repID,ArepSub=Airrepsub[[i]]$repID,
				Nstart=dim(CHFlag[[i]])[1], SSY=SSc[[i]]$SSY,SEY=SSc[[i]]$SEY, AEY=ASc[[i]]$AEY, ASY=ASc[[i]]$ASY,
				Ncombo=dim(IDforCombo[[i]])[1], SoilIND=IDforCombo[[i]]$siteSDW,AirIND=IDforCombo[[i]]$siteSDWA)

print(paste("start initialize site number", i))
temp.modI<-jags.model(file="/home/hkropp/github/synthesis_database/Temp_model/temperature_mod_code.r",
						data=datalist,
						n.adapt=100000,
						n.chains=3)


print(paste("initialize done site number ",i ))		
#specify sample run				
n.iter.i=1200000
n.thin=600
codaobj.init = coda.samples(temp.modI,variable.names=samplelist,
                       n.iter=n.iter.i, thin=n.thin)
					   
					   
print(paste("samples done done site number= ", i))

#pull out model stats
Mod.out<-summary(codaobj.init)
dir.create(paste0(outDir,"/site",modRun$siteid[k]))

write.table(Mod.out$statistics, paste0(outDir,"/site",modRun$siteid[k],"/Temp_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(Mod.out$quantiles, paste0(outDir,"/site",modRun$siteid[k],"/Temp_mod_quant.csv"),
			sep=",",row.names=TRUE)
			
print(paste("summary out site number ",i)	)

#save coda

chain1<-as.matrix(codaobj.init[[1]])
write.table(chain1,paste0(outDir,"/site",modRun$siteid[k],"/chain1_coda.csv"), sep=",")
chain2<-as.matrix(codaobj.init[[2]])
write.table(chain2,paste0(outDir,"/site",modRun$siteid[k],"/chain2_coda.csv"), sep=",")
chain3<-as.matrix(codaobj.init[[3]])
write.table(chain3,paste0(outDir,"/site",modRun$siteid[k],"/chain3_coda.csv"), sep=",")
			
print(paste("coda out site number ", i))	
#run mcmc plots on key params
dir.create(paste0(outDir,"/site",modRun$siteid[k],"/history"))	
mcmcplot(codaobj.init, parms=c("T.aveA1","TminA","TmaxA","T.aveS1",
			"TmaxS","TminS","sig.muA","sig.muS","aZero", "bZero", "zeroC","peakWS", "peakWA", "peakSS", "peakSA"),
			dir=paste0(outDir,"/site",modRun$siteid[k],"/history"))		
#get summary and save to file

print(paste("mcmcplot out site number ", i))	
			


	
}	