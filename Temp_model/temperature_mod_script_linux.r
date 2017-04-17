##########################################################################
##########################################################################
########### Data organization and plotting for all temp obs ##############
##########################################################################
library(plyr,lib.loc="/home/hkropp/R")
library(lubridate,lib.loc="/home/hkropp/R")
library(rjags)
library(coda,lib.loc="/home/hkropp/R")
library(xtable,lib.loc="/home/hkropp/R")
library(mcmcplots,lib.loc="/home/hkropp/R")


# set working directory
setwd("/home/hkropp/synthesis/data_u7")
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
SoilS$wdoyP<-ifelse(leap_year(SoilS$wyear)==TRUE, SoilS$wdoy/366.5,SoilS$wdoy/365.5 )

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
AirS$wdoyP<-ifelse(leap_year(AirS$wyear)==TRUE, AirS$wdoy/366.5,AirS$wdoy/365.5 )

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
		all.wdoyA[[i]]<-data.frame(wdoy=c(rep(AirIDS2$wyear[i],366)+seq(1/366.5,366/366.5,length.out=366)),
									wyear=rep(AirIDS2$wyear[i],366),
									siteid=rep(AirIDS2$siteid[i],366),
									height=rep(AirIDS2$height[i],366),
									SDWA=rep(AirIDS2$SDWA[i],366),
									SDS=rep(AIRIDNEWALL$SDA[i],366))
	}else{
		all.wdoyA[[i]]<-data.frame(wdoy=c(rep(AirIDS2$wyear[i],365)+seq(1/365.5,365/365.5,length.out=365)),
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
		all.wdoyS[[i]]<-data.frame(wdoy=c(rep(SoilIDS2$wyear[i],366)+seq(1/366.5,366/366.5,length.out=366)),
									wyear=rep(SoilIDS2$wyear[i],366),
									siteid=rep(SoilIDS2$siteid[i],366),
									depth=rep(SoilIDS2$depth[i],366),
									SDWS=rep(SoilIDS2$SDWS[i],366),
									SDS=rep(SOILIDNEWALL$SDS[i],366))
	}else{
		all.wdoyS[[i]]<-data.frame(wdoy=c(rep(SoilIDS2$wyear[i],365)+seq(1/365.5,365/365.5,length.out=365)),
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




#create index sequence
SoilM2$indexI<-seq(1, dim(SoilM2)[1])
AirM2$indexI<-seq(1, dim(AirM2)[1])

#create index to some for degree days
AYlength<-numeric(0)
ASY<-numeric(0)
AEY<-numeric(0)
for(i in 1:dim(AirIDS2)[1]){
	AYlength[i]<-length(AirM2$indexI[AirM2$SDWA==AirIDS2$SDWA[i]])
	ASY[i]<-AirM2$indexI[AirM2$SDWA==AirIDS2$SDWA[i]][1]
	AEY[i]<-AirM2$indexI[AirM2$SDWA==AirIDS2$SDWA[i]][AYlength[i]]
}
#create index for soil
SYlength<-numeric(0)
SSY<-numeric(0)
SEY<-numeric(0)
for(i in 1:dim(SoilIDS2)[1]){
	SYlength[i]<-length(SoilM2$indexI[SoilM2$SDWS==SoilIDS2$SDWS[i]])
	SSY[i]<-SoilM2$indexI[SoilM2$SDWS==SoilIDS2$SDWS[i]][1]
	SEY[i]<-SoilM2$indexI[SoilM2$SDWS==SoilIDS2$SDWS[i]][SYlength[i]]
}

#check to see if indes are in order
SoilIDCHECK<-data.frame(SoilIDS2,SSY,SEY)
SCHECK1<-SoilIDCHECK$SSY[2:dim(SoilIDCHECK)[1]]-SoilIDCHECK$SEY[1:(dim(SoilIDCHECK)[1]-1)]
if(length(which(SCHECK1>1))!=0){
	print("ERROR Improper join arrangement")
}else{print("Proper join arrangement for soil and depth")}

AirIDCHECK<-data.frame(AirIDS2,ASY,AEY)
ACHECK1<-AirIDCHECK$ASY[2:dim(AirIDCHECK)[1]]-AirIDCHECK$AEY[1:(dim(AirIDCHECK)[1]-1)]
if(length(which(ACHECK1>1))!=0){
	print("ERROR Improper air join arrangement")
}else{print("Proper join arrangement for air and depth")}


#now get the start and end to autoregressive series
AIRAR<-join(site.heightA, AirIDCHECK, by=c("siteid", "height"), type="left")
#get the starting point
AR_Astart<-aggregate(AIRAR$ASY, by=list(AIRAR$SDA), FUN="min")
colnames(AR_Astart)<-c("SDA","ATstart")
#get the ending point
AR_Aend<-aggregate(AIRAR$AEY, by=list(AIRAR$SDA), FUN="max")
colnames(AR_Aend)<-c("SDA","ATend")


#now do soil
#now get the start and end to autoregressive series
SoilAR<-join(site.depthidS, SoilIDCHECK, by=c("siteid", "depth"), type="left")
#get the starting point
AR_Sstart<-aggregate(SoilAR$SSY, by=list(SoilAR$SDS), FUN="min")
colnames(AR_Sstart)<-c("SDS","STstart")
#get the ending point
AR_Send<-aggregate(SoilAR$SEY, by=list(SoilAR$SDS), FUN="max")
colnames(AR_Send)<-c("SDS","STend")

#now make sequence of flags
startFLAGS<-rep(1, dim(SoilM2)[1])
startFLAGS[AR_Sstart$STstart]<- -1

startFLAGA<-rep(1, dim(AirM2)[1])
startFLAGA[AR_Astart$ATstart]<- -1
#Next need to create an index that says when sites should be divided
#want to divide each soil
#want each soil to be divided by each air combo, but only in a year
IDforCombo<-join(AirIDS2,SoilIDS2, by=c("siteid","wyear"), type="inner")
IDforCombo$Nseq<-seq(1,dim(IDforCombo)[1])

#generate replicate id subset
Soilrepsub<-list()
for(i in 1:dim(SoilIDS2)[1]){
	Soilrepsub[[i]]<-sample(SoilM2$indexI[SoilM2$SDWS==i],5)
}
SoilrepsubV<-unlist(Soilrepsub)

Airrepsub<-list()
for(i in 1:dim(AirIDS2)[1]){
	Airrepsub[[i]]<-sample(AirM2$indexI[AirM2$SDWA==i],5)
}
AirrepsubV<-unlist(Airrepsub)

write.table(AirrepsubV, "/home/hkropp/synthesis/output_u7m3/AirrepID.csv", sep=",", row.names=FALSE)
print("repA_out")
write.table(SoilrepsubV, "/home/hkropp/synthesis/output_u7m3/SoilrepID.csv", sep=",", row.names=FALSE)
print("reps_out")
#model
#write.table(AirM2,"c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod2\\output_u7m2\\Tair_model.csv",sep=",",row.names=FALSE)
#write.table(SoilM2,"c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod2\\output_u7m2\\Tsoil_model.csv",sep=",",row.names=FALSE)


datalist<-list(NobsA=dim(AirM2)[1], TempA=AirM2$A, site.depthidA=AirM2$SDS,T.yrA=AirM2$decdate-1991,yearA=floor(AirM2$decdate-1991),
				NobsS=dim(SoilM2)[1], TempS=SoilM2$T,site.depthidS=SoilM2$SDS, T.yrS=SoilM2$decdate-1991,yearS=floor(SoilM2$decdate-1991),
				NsitedepthA=dim(site.heightA)[1],NsitedepthS=dim(site.depthidS)[1], NSDWA=dim(AirIDS2)[1],
				NSDWS=dim(SoilIDS2)[1], SDWS=SoilM2$SDWS, SDWA=AirM2$SDWA,Ncombo=dim(IDforCombo)[1],
				 AirIND=IDforCombo$SDWA,SoilIND=IDforCombo$SDWS,
				 ASY=ASY, AEY=AEY,SSY=SSY,SEY=SEY,
				 NrepS=length(SoilrepsubV), SrepSub=SoilrepsubV,NrepA=length(AirrepsubV),
				 ArepSub=AirrepsubV, startFLAGS=startFLAGS, startFLAGA=startFLAGA)
				
samplelist<-c("T.aveA1","T.aveA2","TminA","TmaxA","T.aveS1","T.aveS2","TmaxS","TminS","sig.muA","sig.muS","startA","startS","Fn","Tn", 
				"TempA", "TempS","TempA.rep", "TempS.rep", "soilAR", "airAR")


temp.modI<-jags.model(file="/home/hkropp/github/synthesis_database/Temp_model/temperature_mod_code.r",
						data=datalist,
						n.adapt=5000,
						n.chains=3, n.thin=2)


print("initialize done")						
n.iter.i=1000
n.thin=2
codaobj.init = coda.samples(temp.modI,variable.names=samplelist,
                       n.iter=n.iter.i, thin=n.thin)
					   
					   
print("samples done done")


Mod.out<-summary(codaobj.init)


write.table(Mod.out$statistics, "/home/hkropp/synthesis/output_u7m3/Temp_mod7_stats.csv",
			sep=",",row.names=TRUE)
write.table(Mod.out$quantiles, "/home/hkropp/synthesis/output_u7m3/Temp_mod7_quant.csv",
			sep=",",row.names=TRUE)
			
print("summary out")	


save(codaobj.init, file="/home/hkropp/synthesis/output_u7m3/mod7_coda.R")
			
print("coda out")	

			
mcmcplot(codaobj.init, dir="/home/hkropp/synthesis/output_u7m3")		
#get summary and save to file

print("mcmcplot out")	
			

#need to write ids to table

write.table(AirIDS2,"/home/hkropp/synthesis/output_u7m3/AirIDS.csv", sep=",", row.names=FALSE)
write.table(SoilIDS2,"/home/hkropp/synthesis/output_u7m3/SoilIDS.csv", sep=",", row.names=FALSE)

write.table(site.heightA,"/home/hkropp/synthesis/output_u7m3/AirIDS_SD.csv", sep=",", row.names=FALSE)
write.table(site.depthidS,"/home/hkropp/synthesis/output_u7m3/SoilIDS_SD.csv", sep=",", row.names=FALSE)

write.table(IDforCombo, "/home/hkropp/synthesis/output_u7m3/NfactorIDS.csv",sep=",", row.names=FALSE)
print("ID write out")	
			