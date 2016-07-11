##############################################################################
######## This file reads in csv files of all data as of the 4th data upload
######## and looks at all temperature data
##############################################################################


#############################################
#############################################
####This analysis revealed some problems with
####the data, and so some sites need to be
####fixed in the database:
#site 13: year off by 1 from 1997 (fixed for this file)
#         same for air temp
#site 18: has two different measurements of soil temp 
#         at a depth of zero
#site 19: starting in 2005, another year mismatch
#         (fixed for this file)
#site 42: need to delete data from duplicate observations
#         from May 5 2008- May 31 2008, 122-152 for each depth
#         database obs id: 355407-355433(last row to delete),
#         357976-358002, 360543-350569,36111-363137,365313-365339,
#         368245-368271, 370811-370837,373017-373043,375220-375246,
#         377787-377813,380357-380383,382927-382953,386224-386250,
#         388794-388820, 391365-391391,
#  		June 1 2011-October 8 2011  152-281 second duplicate delete
#         database obs id: 356632-356761, 359198-359327,361766-361895,
#         364332-364461,366534-366664,369467-369596,372034-372163,
#         374239-374368,376441-376571,379009-379138,381579-381708,
#         384149-384278,387447-387578,390019-390148,392590-392719
#	Air: 126-152 2008 and 152-281 2011  117724-117750 and 118948-119078

#site 43: duplicate data on 152 2011-281 only for depths 100, 200, 280
#         deleted second dulicate. database obs id: 416347-416476,
#         418919-419048, 421491-421620
#	Air: 152-281 2011 121521-121650

#site 44:  college all of the soil depths other than air 
#		   and surface are not reliable so do not use
#          the soil temperature data
#site 45: 2010 152 second observation, 2011 152-225 deleted second observation
#			depths 1,7,9,16,18,24,
#           435019,435458-435531,441722,442160-442233, 443329-443402,
#           447372-447445,450580,451008-451081,455475-455547
#   Air: 152- 225 2011 126995-127068
#site 46: Air 60- 151 2007 129917-130008
#site 47: Air 152-171 2011 153-236 2012 134053-134072 and
#         134503-134586
#site 48: 2011 152-227, 15,30,60 duplicate deleted second
#          524250-524325,529576-529651,537166-537241
#    Air: 152-227 2011 136039-136114
#site 49: 2011 152-214 10,40,
#          545089-545151, 551858-551919
#    Air: 152-214 2011 137562-

#####

###########################################
#########organize data for analysis########
###########################################
# set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\perma_analysis_backup")
#read in soil temperature
datS<-read.table("soil_temp_fix.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))
#need to create a list of dataframes for each site that combines air
#air temp with soil T, for each possible measurement for the day, year
#period of the data set.

library(plyr)

#get number of sites
Nsite<-dim(siteinf)[1]
#first check to see if any sites are missing soil data
Tlength<-numeric(0)
for(i in 1:Nsite){
	#number of unique depths at each site
	Tlength[i]<-length(unique(datS$st_depth[datS$site_id==i]))
	}
#add a row of NA for any site missing data 
for(i in 1:Nsite){	
	for(j in 1:length(Tlength[Tlength==0])){
		if(Tlength[i]==0){
			datS[dim(datS)[1]+j,]<-c(NA,NA,NA,NA,NA,i)
		}
		
	}
}


##Need some flags and to deal with duplicate depths and duplicated days before
#running this. 
NdaylengthS<-list()
duplicate<-list()
dupvec<-numeric(0)
NdaylengthA<-list()
duplicateA<-list()
dupvecA<-numeric(0)
for(i in 1:Nsite){

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
	
}

siteflag.dup<-which(dupvec!=0)
siteflag.dupA<-which(dupvecA!=0)

#went back to raw data
#Problem with site 13 is that I used the original year label in the label was 
#incorrect. Went back and fixed in the backup csv (changed name), but needs
#to be fixed in the database

#merge all soil temp together but need to accomadate different conditions

tempT<-list()
S.all<-list()

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
		}else{if(i!=44|i!=18){
	
		#next need to find the depth with the most amount of measurements and 
		#order the depths based on this
			tempT[[1]]<-timedf[[i]]
				for(j in 1:Tlength[i]){
				tempT[[j+1]]<-datS[datS$site_id==i
					&datS$st_depth==unique(datS$st_depth[datS$site_id==i])[j],]
		
			}
		
	#merge all together
		S.all[[i]]<-join_all(tempT,by=c("doy_st", "year_st"), type="left")
		}else{
		S.all[[i]]<-c(NA)
		}
	}
}else{S.all[[i]]<-c(NA)}
}

#now deal with site 24 which is currently missing soil temp data
S.all[[24]]<-data.frame(doy_st=0,year_st=0,
						soil_t=NA,
						st_depth=NA)

S.all[[18]]<-data.frame(doy_st=0,year_st=0,
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
A.all[[18]]<-data.frame(doy_st=0,year_st=0,
						air_t=NA,
						air_height=NA)
						

A.all[[44]]<-data.frame(doy_st=0,year_st=0,
						air_t=NA,
						air_height=NA)
												
						
						
#need a more efficient method than the for loop 
#running out of memory
												
Temperature<-list()
for(i in 1:Nsite){
	Temperature[[i]]<-join(S.all[[i]],A.all[[i]], by=c("doy_st", "year_st"), type="left")

}

#check dim of temp
dimTemp<-numeric(0)
dimTcheck<-numeric(0)
for(i in 1:Nsite){
	dimTemp[i]<-dim(Temperature[[i]])[1]
	dimTcheck[i]<-dimTemp[[i]][1]-S.dim[[i]][1]
}

#####Need to add in further checks for data errors


#######################################
#######################################
#######################################
### Now calculate N factors ###########
#######################################

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

#set up an index that goes by year and winter or summer for each site
######To add need to account for leap year

for(i in 1:Nsite){
	#set up index for year
	dyear[[i]]<-unique(Temperature[[i]]$year_st)
	dseq[[i]]<-seq(1,length(unique(Temperature[[i]]$year_st)))
	#set up vector to replace Na with index
	Temperature[[i]]$w.index<-rep(999,dim(Temperature[[i]])[1])
	Temperature[[i]]$w.year<-rep(-999,dim(Temperature[[i]])[1])
	Temperature[[i]]$s.index
	for(j in 1:dim(Temperature[[i]])[1]){
		for(m in 1:length(dyear[[i]])){
		#index for Jan for May
			if(Temperature[[i]]$doy_st[j]<122
				&Temperature[[i]]$year_st[j]==dyear[[i]][m]){
				Temperature[[i]]$w.index[j]<-1
				Temperature[[i]]$w.year[j]<-dyear[[i]][m]
			}else{
				if(Temperature[[i]]$doy_st[j]>275
					&Temperature[[i]]$year_st[j]==dyear[[i]][m]){
					Temperature[[i]]$w.index[j]<-1
					Temperature[[i]]$w.year[j]<-dyear[[i]][m+1]
				}
			}
		
		
		}

	}
}
#now create summer index

for(i in 1:Nsite){
	Temperature[[i]]$s.index<-rep(999,dim(Temperature[[i]])[1])
	for(j in 1:dim(Temperature[[i]])[1]){
		for(m in 1:length(dyear[[i]])){
			if(Temperature[[i]]$w.index[j]!=1
				&Temperature[[i]]$year_st[j]==dyear[[i]][m]){
				Temperature[[i]]$s.index[j]<-1
			}
		}
	}

}




#now need to deal with depth again
sT<-list()
sdep<-list()
srep<-numeric(0)
siteseq<-seq(1,Nsite)
for(i in 1:Nsite){
	
	srep[i]<-length(which(colnames(Temperature[[i]])=="st_depth"))
	#make a dataframe of soil T and depth
	sT[[i]]<-data.frame(T=as.vector(data.matrix(Temperature[[i]][,colnames(Temperature[[i]])=="soil_t"])),
	depth=as.vector(data.matrix(Temperature[[i]][,colnames(Temperature[[i]])=="st_depth"])),
	w.ind=rep(Temperature[[i]]$w.ind, times=srep[i]),
	s.ind=rep(Temperature[[i]]$s.ind, times=srep[i]),
	doy=rep(Temperature[[i]]$doy_st, times=srep[i]),
	year=rep(Temperature[[i]]$year_st, times=srep[i]),
	wyear=rep(Temperature[[i]]$w.year, times=srep[i]),
	siteid=rep(siteseq[i],srep[i]*length(Temperature[[i]]$w.ind)))
}

#compile air temperature data
sA<-list()

srepA<-numeric(0)
siteseq<-seq(1,Nsite)
for(i in 1:Nsite){
	
	srepA[i]<-length(which(colnames(Temperature[[i]])=="air_height"))
	#make a dataframe of soil T and depth
	sA[[i]]<-data.frame(A=as.vector(data.matrix(Temperature[[i]][,colnames(Temperature[[i]])=="air_t"])),
	depth=as.vector(data.matrix(Temperature[[i]][,colnames(Temperature[[i]])=="air_height"])),
	w.ind=rep(Temperature[[i]]$w.ind, times=srepA[i]),
	s.ind=rep(Temperature[[i]]$s.ind, times=srepA[i]),
	doy=rep(Temperature[[i]]$doy_st, times=srepA[i]),
	year=rep(Temperature[[i]]$year_st, times=srepA[i]),
	wyear=rep(Temperature[[i]]$w.year, times=srepA[i]),
	siteid=rep(siteseq[i],srepA[i]*length(Temperature[[i]]$w.ind)))
}

sT.n<-list()
sT.c<-list()
Scountw<-list()

#start aggregating data
for(i in 1:Nsite){
	#see how many observations for winter and summer
	#if all observations are present
	#154 in summer
	#212 in winter (+1 leap year)
	#now see how many days are in each index
	sT.n[[i]]<-na.omit(sT[[i]])
	sT.c[[i]]<-dim(sT.n[[i]])
	#see which data have full winter days
		if(length(sT.n[[i]]$T[sT.n[[i]]$w.ind==1])!=0){
			Scountw[[i]]<-aggregate(sT.n[[i]]$T[sT.n[[i]]$w.ind==1],
					by=list(sT.n[[i]]$depth[sT.n[[i]]$w.ind==1],
						sT.n[[i]]$wyear[sT.n[[i]]$w.ind==1]), FUN="length")
		}else{Scountw[[i]]<-data.frame(Group.1=NA,Group.2=NA, x=NA)}
		Scountw[[i]]$site<-rep(i,dim(Scountw[[i]])[1])
}

#get air temperature counts

sA.n<-list()
sA.c<-list()
Acountw<-list()

#start aggregating data
for(i in 1:Nsite){
	#see how many observations for winter and summer
	#if all observations are present
	#154 in summer
	#212 in winter (+1 leap year)
	#now see how many days are in each index
	sA.n[[i]]<-na.omit(sA[[i]])
	sA.c[[i]]<-dim(sA.n[[i]])
	#see which data have full winter days
		if(length(sA.n[[i]]$A[sA.n[[i]]$w.ind==1])!=0){
			Acountw[[i]]<-aggregate(sA.n[[i]]$A[sA.n[[i]]$w.ind==1],
					by=list(sA.n[[i]]$depth[sA.n[[i]]$w.ind==1],
						sA.n[[i]]$wyear[sA.n[[i]]$w.ind==1]), FUN="length")
		}else{Acountw[[i]]<-data.frame(Group.1=NA,Group.2=NA, x=NA)}
		Acountw[[i]]$site<-rep(i,dim(Acountw[[i]])[1])
}

		
#now see full summer
Scounts<-list()
for(i in 1:Nsite){
		if(length(sT.n[[i]]$T[sT.n[[i]]$s.ind==1])!=0){
			Scounts[[i]]<-aggregate(sT.n[[i]]$T[sT.n[[i]]$s.ind==1],
					by=list(sT.n[[i]]$depth[sT.n[[i]]$s.ind==1],
						sT.n[[i]]$year[sT.n[[i]]$s.ind==1]), FUN="length")
		}else{Scounts[[i]]<-data.frame(Group.1=NA,Group.2=NA, x=NA)}
		Scounts[[i]]$site<-rep(i,dim(Scounts[[i]])[1])
}
#air summer
Acounts<-list()
for(i in 1:Nsite){
		if(length(sA.n[[i]]$A[sA.n[[i]]$s.ind==1])!=0){
			Acounts[[i]]<-aggregate(sA.n[[i]]$A[sA.n[[i]]$s.ind==1],
					by=list(sA.n[[i]]$depth[sA.n[[i]]$s.ind==1],
						sA.n[[i]]$year[sA.n[[i]]$s.ind==1]), FUN="length")
		}else{Acounts[[i]]<-data.frame(Group.1=NA,Group.2=NA, x=NA)}
		Acounts[[i]]$site<-rep(i,dim(Acounts[[i]])[1])
}


#turn list of summer and winter index count into data frame for soil and air
Scountsdf<-ldply(Scounts,data.frame)
Wcountsdf<-ldply(Scountw,data.frame)
Acountwdf<-ldply(Acountw,data.frame)
Acountsdf<-ldply(Acounts,data.frame)

#get rid of any NAs and observations that don't span a full season
S.use<-na.omit(Scountsdf[Scountsdf$x==154,])
W.use<-na.omit(Wcountsdf[Wcountsdf$x==212|Wcountsdf$x==213,])
AS.use<-na.omit(Acountsdf[Acountsdf$x==154,])
AW.use<-na.omit(Acountwdf[Acountwdf$x==212|Acountwdf$x==213,])
#change column names
colnames(S.use)<-c("depth", "year","count","siteid")
colnames(W.use)<-c("depth", "wyear","count","siteid")
colnames(AW.use)<-c("depth", "wyear","count","siteid")
colnames(AS.use)<-c("depth", "year","count","siteid")
#turn list into a dataframe for all obs without NA included
#for both soil and air
sTdf<-ldply(sT.n)
sAdf<-ldply(sA.n)
#subset the observations by only site and depth with data for complete season 
S.forcalc<-join(sTdf,S.use, by=c("depth", "year","siteid"), type="inner")
W.forcalc<-join(sTdf,W.use, by=c("depth", "wyear","siteid"), type="inner")
AS.forcalc<-join(sAdf,AS.use, by=c("depth", "year","siteid"), type="inner")
AW.forcalc<-join(sAdf,AW.use, by=c("depth", "wyear","siteid"), type="inner")
#get all the days for freezing and thawing indices
S.thaw<-S.forcalc[S.forcalc$T>0,]
W.freeze<-W.forcalc[W.forcalc$T<0,]
AS.thaw<-AS.forcalc[AS.forcalc$A>0,]
AW.freeze<-AW.forcalc[AW.forcalc$A<0,]
#add up all days that are freezing or thawing for degree days
Sthaw.dd<-aggregate(S.thaw$T, 
			by=list(S.thaw$depth,S.thaw$year,S.thaw$siteid), FUN="sum")
Sfreeze.dd<-aggregate(W.freeze$T, 
			by=list(W.freeze$depth,W.freeze$wyear,W.freeze$siteid), FUN="sum")		
Athaw.dd<-aggregate(AS.thaw$A, 
			by=list(AS.thaw$depth,AS.thaw$year,AS.thaw$siteid), FUN="sum")
Afreeze.dd<-aggregate(AW.freeze$A, 
			by=list(AW.freeze$depth,AW.freeze$wyear,AW.freeze$siteid), FUN="sum")
#look at some data
plot(Sthaw.dd$Group.1[Sthaw.dd$Group.1>10],Sthaw.dd$x[Sthaw.dd$Group.1>10])
plot(Sfreeze.dd$Group.2[Sfreeze.dd$Group.3==15],Sfreeze.dd$x[Sfreeze.dd$Group.3==15])
#rename columns		
colnames(Sthaw.dd)<-c("depth","year","siteid","T")
colnames(Sfreeze.dd)<-c("depth","wyear","siteid","T")
colnames(Afreeze.dd)<-c("height","wyear","siteid","AT")	
colnames(Athaw.dd)<-c("height","year","siteid","AT")				
			
#now calculate nfactor
#soil/air
#need to make a table of of freezing and thawing where data matches
Winter.data<-join(Sfreeze.dd,Afreeze.dd, by=c("wyear","siteid"), type="inner")
Summer.data<-join(Sthaw.dd,Athaw.dd,by=c("year","siteid"), type="inner")
#calculate n factor
Winter.data$n<-Winter.data$T/Winter.data$AT
Summer.data$n<-Summer.data$T/Summer.data$AT

#look at some n factors
plot(Winter.data$wyear[Winter.data$depth<10],
		Winter.data$n[Winter.data$depth<10],pch=19)
		
plot(Winter.data$wyear[Winter.data$depth>10
		Winter.data$n[Winter.data$depth>10],pch=19)
		
#merge the site info table to look at more patterns
colnames(siteinf)[1]<-c("siteid")

#create a quick site index
region<-c("Sweden1", "AK1","AK1","AK1","AK1","AK2","AK2","AK2","AK2",
			"Greenland1","Greenland1","Canada1","Canada1","Canada1",
			"Canada1","Canada1","Canada2","Canada2","Canada2","Canada2",
			"Russia1","AK3","AK3","AK3","AK3","AK3","AK3","AK3","AK3",
			rep("AK4",12),rep("AK5",3), "AK6","AK7","AK7","AK8","AK8",
			rep("Russia2", 6),"Canada3","Canada3","Canada4", "Canada4",
			"Russia3", "Russia3", "Norway1", "Russia4", "Russia4")
siteinf$region<-region

N.wint<-join(Winter.data,siteinf,by=c("siteid"), type="inner")
N.summ<-join(Summer.data,siteinf,by=c("siteid"), type="inner")


########################################
##############Some quick initial plots##
########################################

plot(N.wint$lat[N.wint$depth<10],N.wint$n[N.wint$depth<10], pch=19,
	xlab="latitude",ylab="Winter N factor")
new_v<-ifelse(N.wint$vege_z=="boreal forest", "boreal",
		ifelse(	N.wint$vege_z=="boreal", "boreal",
		ifelse(	N.wint$vege_z=="tundra", "tundra",
		ifelse(	is.na(N.wint$vege_z),"unclassified","unclassified"))))
	
Snew_v<-ifelse(N.summ$vege_z=="boreal forest", "boreal",
		ifelse(	N.summ$vege_z=="boreal", "boreal",
		ifelse(	N.summ$vege_z=="tundra", "tundra",
		ifelse(	is.na(N.summ$vege_z),"unclassified","unclassified"))))
		
#get sample size
Wnumbs<-aggregate(N.wint$n[N.wint$depth<10], by=list(as.factor(new_v[N.wint$depth<10])),
		FUN="length")
		
Snumbs<-aggregate(N.summ$n[N.summ$depth<10],by=list(as.factor(new_v[N.summ$depth<10])),
		FUN="length")
#make box plot
par(mfrow=c(1,2))	
plot(N.wint$n[N.wint$depth<10]~as.factor(new_v[N.wint$depth<10]), pch=19,
	xlab="biome",ylab="Winter N factor")
text(seq(1.25,3.25),rep(1,3), c("n=15","n=111","n=6"), cex=2)	
plot(N.summ$n[N.summ$depth<10]~as.factor(new_v[N.summ$depth<10]), pch=19,
	xlab="biome",ylab="Summer N factor")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)
text(seq(1.25,3.25),rep(1.4,3), c("n=82","n=201","n=8"), cex=2)

#make plot of latitude
par(mfrow=c(1,2))	
plot(N.wint$lat[N.wint$depth<10&new_v=="tundra"],N.wint$n[N.wint$depth<10&new_v=="tundra"], pch=19,
	xlab="latitude",ylab="Winter N factor", col="cadetblue3")
points(N.wint$lat[N.wint$depth<10&new_v=="boreal"],N.wint$n[N.wint$depth<10&new_v=="boreal"], pch=19,
	 col="forestgreen")
legend(66,1,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")
plot(N.summ$lat[N.summ$depth<10&Snew_v=="tundra"],N.summ$n[N.summ$depth<10&Snew_v=="tundra"], pch=19,
	xlab="latitude",ylab="Summer N factor", col="cadetblue3")		
points(N.summ$lat[N.summ$depth<10&Snew_v=="boreal"],N.summ$n[N.summ$depth<10&Snew_v=="boreal"], pch=19,
	col="forestgreen")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)	

#make plot of year
par(mfrow=c(1,2))	
plot(N.wint$wyear[N.wint$depth<10&new_v=="tundra"],N.wint$n[N.wint$depth<10&new_v=="tundra"], pch=19,
	xlab="year",ylab="Winter N factor", col="cadetblue3")
points(N.wint$wyear[N.wint$depth<10&new_v=="boreal"],N.wint$n[N.wint$depth<10&new_v=="boreal"], pch=19,
	 col="forestgreen")
	legend(2005,1,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")
plot(N.summ$year[N.summ$depth<10&Snew_v=="tundra"],N.summ$n[N.summ$depth<10&Snew_v=="tundra"], pch=19,
	xlab="latitude",ylab="Summer N factor", col="cadetblue3")		
points(N.summ$year[N.summ$depth<10&Snew_v=="boreal"],N.summ$n[N.summ$depth<10&Snew_v=="boreal"], pch=19,
	col="forestgreen")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)

#create a quick site index
region<-c("Sweden1", "AK1","AK1","AK1","AK1","AK2","AK2","AK2","AK2",
			"Greenland1","Greenland1","Canada1","Canada1","Canada1",
			"Canada1","Canada1","Canada2","Canada2","Canada2","Canada2",
			"Russia1","AK3","AK3","AK3","AK3","AK3","AK3","AK3","AK3",
			rep("AK4",12),rep("AK5",3), "AK6","AK7","AK7","AK8","AK8",
			rep("Russia2", 6),"Canada3","Canada3","Canada4", "Canada4",
			"Russia3", "Russia3", "Norway1", "Russia4", "Russia4")
			
#make box plot

plot(N.wint$n[N.wint$depth<10]~as.factor(N.wint$region[N.wint$depth<10]), pch=19,
	xlab="region",ylab="Winter N factor")
text(seq(1.25,3.25),rep(1,3), c("n=15","n=111","n=6"), cex=2)	
plot(N.summ$n[N.summ$depth<10]~as.factor(N.summ$region[N.summ$depth<10]), pch=19,
	xlab="biome",ylab="Summer N factor")
		