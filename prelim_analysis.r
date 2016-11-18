##############################################################################
######## This file reads in csv files of all data as of the 5th data upload
######## and looks at all temperature data
##############################################################################


###########################################
#########organize data for analysis########
###########################################
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
												
Temperature<-list()
for(i in 1:Nsite){
	Temperature[[i]]<-join(S.all[[i]],A.all[[i]], by=c("doy_st", "year_st"), type="left")

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
#subset temperature by season
SummT<-sTdf[sTdf$s.ind==1,]
WintT<-sTdf[sTdf$w.ind==1,]
ASummT<-sAdf[sAdf$s.ind==1,]
AWintT<-sAdf[sAdf$w.ind==1,]
#join based on what data to use to meet criteria
S.forcalc<-join(SummT,S.use, by=c("depth", "year","siteid"), type="inner")
W.forcalc<-join(WintT,W.use, by=c("depth", "wyear","siteid"), type="inner")
AS.forcalc<-join(ASummT,AS.use, by=c("depth", "year","siteid"), type="inner")
AW.forcalc<-join(AWintT,AW.use, by=c("depth", "wyear","siteid"), type="inner")
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
		
plot(Winter.data$wyear[Winter.data$depth>10],
		Winter.data$n[Winter.data$depth>10],pch=19)
		
#merge the site info table to look at more patterns
colnames(siteinf)[1]<-c("siteid")


N.wint<-join(Winter.data,siteinf,by=c("siteid"), type="inner")
N.summ<-join(Summer.data,siteinf,by=c("siteid"), type="inner")


########################################
##############Some quick initial plots##
########################################
{
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

plot(N.summ$n[N.summ$depth<10]~as.factor(new_v[N.summ$depth<10]), pch=19,
	xlab="biome",ylab="Summer N factor")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)


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
	xlab="year",ylab="Winter N factor", col="cadetblue3", xlim=c(1987,2016))
points(N.wint$wyear[N.wint$depth<10&new_v=="boreal"],N.wint$n[N.wint$depth<10&new_v=="boreal"], pch=19,
	 col="forestgreen")
	legend(2005,1,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")
plot(N.summ$year[N.summ$depth<10&Snew_v=="tundra"],N.summ$n[N.summ$depth<10&Snew_v=="tundra"], pch=19,
	xlab="year",ylab="Summer N factor", col="cadetblue3")		
points(N.summ$year[N.summ$depth<10&Snew_v=="boreal"],N.summ$n[N.summ$depth<10&Snew_v=="boreal"], pch=19,
	col="forestgreen")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)

#create a quick site index


plot(N.wint$n[N.wint$depth<10]~as.factor(N.wint$loc[N.wint$depth<10]), pch=19,
	xlab="region",ylab="Winter N factor")

plot(N.summ$n[N.summ$depth<10]~as.factor(N.summ$loc[N.summ$depth<10]), pch=19,
	xlab="biome",ylab="Summer N factor")
	
	}
	
######################################################################################################
######################################################################################################
##################### See if N factors can be corrected if incomplete#################################
##################### Start by better assessing incomplete data  #####################################
######################################################################################################

		
#figure out how sensitive correcting N factors where 90% of more of the data is present in a season.
#Start by looking at how sensitive excluding data and doing the calculation is on datasets where
#the enitre dataset is present for the season.

#first explore how much data changes

SdayLow<-round(0.95*154,0)
WdayLow<-round(0.95*212,0)

#See what data could be corrected
S.toCor<-na.omit(Scountsdf[Scountsdf$x<154&Scountsdf$x>=SdayLow,])
W.toCor<-na.omit(Wcountsdf[Wcountsdf$x<212&Wcountsdf$x>=WdayLow,])
AS.toCor<-na.omit(Acountsdf[Acountsdf$x<154&Acountsdf$x>=SdayLow,])
AW.toCor<-na.omit(Acountwdf[Acountwdf$x<212&Acountwdf$x>=WdayLow,])
#Note adjusting N factor for 95% of the season appears to drastically increase
#data available for analysis so it is worth exploring

############################################
#############!!!!!!!!!!!####################
#####!!! No need to run everytime!!!!#######
#############!!!!!!!!!!!####################
############################################
{
#now need to look at how much a correction for degree days could influence it
#look at soil

#first exclude 5% of the days under 4 scenarios:
#1. all at beginning
#2. all at end
#3. all in middle
#4. dispersed randomly
S.missing<-154-SdayLow
W.missing<-212-WdayLow
SeasonLength<-c(154,212,154,212)
#set up missing data scenarios
#missing at the beginning
MissingSubtract<-c(S.missing,W.missing,S.missing,W.missing)
BegSeasS<-MissingSubtract
BegSeasE<-SeasonLength
#block missing at the end
EndSeasS<-rep(1,4)
EndSeasE<-SeasonLength-MissingSubtract
#block missing in the middle
MidSeasS1<-rep(1,4)
MidSeasE1<-c(77,106,77,106)
MidSeasS2<-MidSeasE1+MissingSubtract
MidSeasE2<-SeasonLength
Midseas<-list()
for(i in 1:4){
	Midseas[[i]]<-c(seq(MidSeasS1[i],MidSeasE1[i]),seq(MidSeasS2[i],MidSeasE2[i]))

}

#random days missing
RSeasListWg<-sample(seq(1,212),size=WdayLow)
RSeasListW<-sort(RSeasListWg)
RSeasListSg<-sample(seq(1,154),size=SdayLow)
RSeasListS<-sort(RSeasListSg)
RSeas<-list(RSeasListS,RSeasListW,RSeasListS,RSeasListW)

#after data is pulled out to use,and data is filtered for proper length/season 
#S.for calc, W.forcalc, AS.forcalc, AW.forcalc

#need to get site list
Subsitelist<-list()
Subsitelist[[1]]<-unique(data.frame(site=S.forcalc$siteid, depth=S.forcalc$depth, year=S.forcalc$year))
Subsitelist[[2]]<-unique(data.frame(site=W.forcalc$siteid, depth=W.forcalc$depth, wyear=W.forcalc$wyear))
Subsitelist[[3]]<-unique(data.frame(site=AS.forcalc$siteid, depth=AS.forcalc$depth, year=AS.forcalc$year))
Subsitelist[[4]]<-unique(data.frame(site=AW.forcalc$siteid, depth=AW.forcalc$depth, wyear=AW.forcalc$wyear))

#make a list of data to subset
Dataforallcalcs<-list(S.forcalc,W.forcalc,AS.forcalc,AW.forcalc)

#get list for each
SFORCALC.list<-list()

for(i in 1:dim(Subsitelist[[1]])[1]){
	SFORCALC.list[[i]]<-Dataforallcalcs[[1]][Dataforallcalcs[[1]]$siteid==Subsitelist[[1]]$site[i]&
												Dataforallcalcs[[1]]$year==Subsitelist[[1]]$year[i]&
												Dataforallcalcs[[1]]$depth==Subsitelist[[1]]$depth[i],]
}
WFORCALC.list<-list()
for(i in 1:dim(Subsitelist[[2]])[1]){
	WFORCALC.list[[i]]<-Dataforallcalcs[[2]][Dataforallcalcs[[2]]$siteid==Subsitelist[[2]]$site[i]&
												Dataforallcalcs[[2]]$wyear==Subsitelist[[2]]$wyear[i]&
												Dataforallcalcs[[2]]$depth==Subsitelist[[2]]$depth[i],]
}
ASFORCALC.list<-list()
for(i in 1:dim(Subsitelist[[3]])[1]){
	ASFORCALC.list[[i]]<-Dataforallcalcs[[3]][Dataforallcalcs[[3]]$siteid==Subsitelist[[3]]$site[i]&
												Dataforallcalcs[[3]]$year==Subsitelist[[3]]$year[i]&
												Dataforallcalcs[[3]]$depth==Subsitelist[[3]]$depth[i],]
}
AWFORCALC.list<-list()
for(i in 1:dim(Subsitelist[[4]])[1]){
	AWFORCALC.list[[i]]<-Dataforallcalcs[[4]][Dataforallcalcs[[4]]$siteid==Subsitelist[[4]]$site[i]&
												Dataforallcalcs[[4]]$wyear==Subsitelist[[4]]$wyear[i]&
												Dataforallcalcs[[4]]$depth==Subsitelist[[4]]$depth[i],]
}




##now the data can be excluded according to each scenario
#summer soil
begSeasonMS<-list()
endSeasonMS<-list()
midSeasonMS<-list()
ranSeasonMS<-list()

#winter soil
begSeasonMW<-list()
endSeasonMW<-list()
midSeasonMW<-list()
ranSeasonMW<-list()

#summer air
begSeasonMAS<-list()
endSeasonMAS<-list()
midSeasonMAS<-list()
ranSeasonMAS<-list()

#winter air
begSeasonMAW<-list()
endSeasonMAW<-list()
midSeasonMAW<-list()
ranSeasonMAW<-list()
#summer soil
for(j in 1:dim(Subsitelist[[1]])[1]){
	begSeasonMS[[j]]<-SFORCALC.list[[j]][BegSeasS[1]:BegSeasE[1],]
	endSeasonMS[[j]]<-SFORCALC.list[[j]][EndSeasS[1]:EndSeasE[1],]
	midSeasonMS[[j]]<-SFORCALC.list[[j]][Midseas[[1]],]
	ranSeasonMS[[j]]<-SFORCALC.list[[j]][RSeas[[1]],]
}
#winter soil
for(j in 1:dim(Subsitelist[[2]])[1]){
	begSeasonMW[[j]]<-WFORCALC.list[[j]][BegSeasS[2]:BegSeasE[2],]
	endSeasonMW[[j]]<-WFORCALC.list[[j]][EndSeasS[2]:EndSeasE[2],]
	midSeasonMW[[j]]<-WFORCALC.list[[j]][Midseas[[2]],]
	ranSeasonMW[[j]]<-WFORCALC.list[[j]][RSeas[[2]],]
}
#summer air
for(j in 1:dim(Subsitelist[[3]])[1]){
	begSeasonMAS[[j]]<-ASFORCALC.list[[j]][BegSeasS[3]:BegSeasE[3],]
	endSeasonMAS[[j]]<-ASFORCALC.list[[j]][EndSeasS[3]:EndSeasE[3],]
	midSeasonMAS[[j]]<-ASFORCALC.list[[j]][Midseas[[3]],]
	ranSeasonMAS[[j]]<-ASFORCALC.list[[j]][RSeas[[3]],]
}

#winter air
for(j in 1:dim(Subsitelist[[4]])[1]){
	begSeasonMAW[[j]]<-AWFORCALC.list[[j]][BegSeasS[4]:BegSeasE[4],]
	endSeasonMAW[[j]]<-AWFORCALC.list[[j]][EndSeasS[4]:EndSeasE[4],]
	midSeasonMAW[[j]]<-AWFORCALC.list[[j]][Midseas[[4]],]
	ranSeasonMAW[[j]]<-AWFORCALC.list[[j]][RSeas[[4]],]

}

#now need to calculate freezing, thawing, and n factors with datasets
#first turn subsetted datasets into dataframes
begS<-ldply(begSeasonMS,data.frame)
endS<-ldply(endSeasonMS,data.frame)
midS<-ldply(midSeasonMS,data.frame)
ranS<-ldply(ranSeasonMS,data.frame)

begW<-ldply(begSeasonMW,data.frame)
endW<-ldply(endSeasonMW,data.frame)
midW<-ldply(midSeasonMW,data.frame)
ranW<-ldply(ranSeasonMW,data.frame)

begAS<-ldply(begSeasonMAS,data.frame)
endAS<-ldply(endSeasonMAS,data.frame)
midAS<-ldply(midSeasonMAS,data.frame)
ranAS<-ldply(ranSeasonMAS,data.frame)

begAW<-ldply(begSeasonMAW,data.frame)
endAW<-ldply(endSeasonMAW,data.frame)
midAW<-ldply(midSeasonMAW,data.frame)
ranAW<-ldply(ranSeasonMAW,data.frame)

###get thawing together
begS.thaw<-begS[begS$T>0,]
endS.thaw<-endS[endS$T>0,]
midS.thaw<-midS[midS$T>0,]
ranS.thaw<-ranS[ranS$T>0,]


begAS.thaw<-begAS[begAS$A>0,]
endAS.thaw<-endAS[endAS$A>0,]
midAS.thaw<-midAS[midAS$A>0,]
ranAS.thaw<-ranAS[ranAS$A>0,]



#get freezing together

begW.freeze<-begW[begW$T<0,]
endW.freeze<-endW[endW$T<0,]
midW.freeze<-midW[midW$T<0,]
ranW.freeze<-ranW[ranW$T<0,]


begAW.freeze<-begAW[begAW$A<0,]
endAW.freeze<-endAW[endAW$A<0,]
midAW.freeze<-midAW[midAW$A<0,]
ranAW.freeze<-ranAW[ranAW$A<0,]


#add up all days that are freezing or thawing for degree days
begSthaw.dd<-aggregate(begS.thaw$T, 
			by=list(begS.thaw$depth,begS.thaw$year,begS.thaw$siteid), FUN="sum")
begASthaw.dd<-aggregate(begAS.thaw$A, 
			by=list(begAS.thaw$depth,begAS.thaw$year,begAS.thaw$siteid), FUN="sum")
	
endSthaw.dd<-aggregate(endS.thaw$T, 
			by=list(endS.thaw$depth,endS.thaw$year,endS.thaw$siteid), FUN="sum")
endASthaw.dd<-aggregate(endAS.thaw$A, 
			by=list(endAS.thaw$depth,endAS.thaw$year,endAS.thaw$siteid), FUN="sum")	

midSthaw.dd<-aggregate(midS.thaw$T, 
			by=list(midS.thaw$depth,midS.thaw$year,midS.thaw$siteid), FUN="sum")
midASthaw.dd<-aggregate(midAS.thaw$A, 
			by=list(midAS.thaw$depth,midAS.thaw$year,midAS.thaw$siteid), FUN="sum")	

ranSthaw.dd<-aggregate(ranS.thaw$T, 
			by=list(ranS.thaw$depth,ranS.thaw$year,ranS.thaw$siteid), FUN="sum")
ranASthaw.dd<-aggregate(ranAS.thaw$A, 
			by=list(ranAS.thaw$depth,ranAS.thaw$year,ranAS.thaw$siteid), FUN="sum")				

			
begWfreeze.dd<-aggregate(begW.freeze$T, 
			by=list(begW.freeze$depth,begW.freeze$wyear,begW.freeze$siteid), FUN="sum")		

begAWfreeze.dd<-aggregate(begAW.freeze$A, 
			by=list(begAW.freeze$depth,begAW.freeze$wyear,begAW.freeze$siteid), FUN="sum")	

endWfreeze.dd<-aggregate(endW.freeze$T, 
			by=list(endW.freeze$depth,endW.freeze$wyear,endW.freeze$siteid), FUN="sum")		

endAWfreeze.dd<-aggregate(endAW.freeze$A, 
			by=list(endAW.freeze$depth,endAW.freeze$wyear,endAW.freeze$siteid), FUN="sum")	
			
midWfreeze.dd<-aggregate(midW.freeze$T, 
			by=list(midW.freeze$depth,midW.freeze$wyear,midW.freeze$siteid), FUN="sum")		

midAWfreeze.dd<-aggregate(midAW.freeze$A, 
			by=list(midAW.freeze$depth,midAW.freeze$wyear,midAW.freeze$siteid), FUN="sum")	
			
ranWfreeze.dd<-aggregate(ranW.freeze$T, 
			by=list(ranW.freeze$depth,ranW.freeze$wyear,ranW.freeze$siteid), FUN="sum")		

ranAWfreeze.dd<-aggregate(ranAW.freeze$A, 
			by=list(ranAW.freeze$depth,ranAW.freeze$wyear,ranAW.freeze$siteid), FUN="sum")	

#make lists to help clean this up
W.freezeL<-list(begWfreeze.dd,endWfreeze.dd,midWfreeze.dd,ranWfreeze.dd)
AW.freezeL<-list(begAWfreeze.dd,endAWfreeze.dd,midAWfreeze.dd,ranAWfreeze.dd)
S.thawL<-list(begSthaw.dd,endSthaw.dd,midSthaw.dd,ranSthaw.dd)
AS.thawL<-list(begASthaw.dd,endASthaw.dd,midASthaw.dd,ranASthaw.dd)

#now correct the degree days
for(i in 1:4){
	colnames(W.freezeL[[i]])<-c("depth","wyear","siteid","T")
	colnames(AW.freezeL[[i]])<-c("depth","wyear","siteid","AT")
	colnames(S.thawL[[i]])<-c("depth","year","siteid","T")
	colnames(AS.thawL[[i]])<-c("depth","year","siteid","AT")
	}
	
for(i in 1:4){	
	W.freezeL[[i]]$corDD<-W.freezeL[[i]]$T/.95
	AW.freezeL[[i]]$corADD<-AW.freezeL[[i]]$AT/.95
	S.thawL[[i]]$corDD<-S.thawL[[i]]$T/.95
	AS.thawL[[i]]$corADD<-AS.thawL[[i]]$AT/.95
}

Winter.corT<-list()
Summer.corT<-list()
for(i in 1:4){
	Winter.corT[[i]]<-join(W.freezeL[[i]],AW.freezeL[[i]], by=c("wyear","siteid"), type="inner")
	Summer.corT[[i]]<-join(S.thawL[[i]],AS.thawL[[i]], by=c("year","siteid"), type="inner")
}

for(i in 1:4){
	Winter.corT[[i]]$ncor<-Winter.corT[[i]]$corDD/Winter.corT[[i]]$corADD
	Summer.corT[[i]]$ncor<-Summer.corT[[i]]$corDD/Summer.corT[[i]]$corADD
	Winter.corT[[i]]$nerr<-Winter.corT[[i]]$T/Winter.corT[[i]]$AT
	Summer.corT[[i]]$nerr<-Summer.corT[[i]]$T/Summer.corT[[i]]$AT	
	
	colnames(Summer.corT[[i]])<-c("depth","year","siteid","Terr","corDD","height","ATerr","corADD","ncor", "nerr")
}
#the only area where the correction falls apart is that some really deep
#soil layers in the summer are almost entirely freezing. Some of these
#data only had a few days of non-freezing data and those get excluded
#this means that I have to account for this in the comparison
SummerallT<-list()
for(i in 1:4){
	SummerallT[[i]]<-join(Summer.corT[[i]], Summer.data, by=c("depth","year","siteid","height"), type="right")

}


#now compare to observed data
#items Winter.data, Summer.data, 
#need to compare each 
#start with visual comparisons
#start with soil
#beggining of season
par(mfrow=c(1,2))
plot(Winter.data$T,Winter.corT[[1]]$corDD, pch=19, col="darkolivegreen3", xlab="observed soil",
ylab="with error", main="corrected")
abline(0,1, lwd=3, col="red2")
#end of season
points(Winter.data$T,Winter.corT[[2]]$corDD, pch=19, col="deepskyblue1")
#middle of season
points(Winter.data$T,Winter.corT[[3]]$corDD, pch=19, col="lavenderblush1")
#random
points(Winter.data$T,Winter.corT[[3]]$corDD, pch=19, col="seagreen2")
#beggining of season
plot(Winter.data$T,Winter.corT[[1]]$T, pch=19, col="darkolivegreen4", xlab="observed soil",
ylab="with error", main="uncorrected")
abline(0,1, lwd=3, col="red2")
#end of season
points(Winter.data$T,Winter.corT[[2]]$T, pch=19, col="deepskyblue4")
#middle of season
points(Winter.data$T,Winter.corT[[3]]$T, pch=19, col="lavenderblush4")
#random
points(Winter.data$T,Winter.corT[[3]]$corDD, pch=19, col="seagreen4")
################################
##########summer data
#beggining of season

plot(SummerallT[[1]]$T,SummerallT[[1]]$corDD, pch=19, col="darkolivegreen3", xlab="observed soil",
ylab="with error", main="corrected", xlim=c(0,3000), ylim=c(0,3000))
abline(0,1, lwd=3, col="red2")
#end of season
points(SummerallT[[1]]$T,SummerallT[[2]]$corDD, pch=19, col="deepskyblue1")
#middle of season
points(SummerallT[[1]]$T,SummerallT[[3]]$corDD, pch=19, col="lavenderblush1")
#random
points(SummerallT[[1]]$T,SummerallT[[4]]$corDD, pch=19, col="seagreen2")
#beggining of season
plot(SummerallT[[1]]$T,SummerallT[[1]]$Terr, pch=19, col="darkolivegreen4", xlab="observed soil",
ylab="with error", main="uncorrected", xlim=c(0,3000), ylim=c(0,3000))
abline(0,1, lwd=3, col="red2")
#end of season
points(SummerallT[[1]]$T,SummerallT[[2]]$Terr, pch=19, col="deepskyblue4")
#middle of season
points(SummerallT[[1]]$T,SummerallT[[3]]$Terr, pch=19, col="lavenderblush4")
#random
points(SummerallT[[1]]$T,SummerallT[[4]]$Terr, pch=19, col="seagreen4")


#check air
#beggining of season
par(mfrow=c(1,2))
plot(Winter.data$AT,Winter.corT[[1]]$corADD, pch=19, col="darkolivegreen3", xlab="observed air",
ylab="with error", main="corrected")
abline(0,1, lwd=3, col="red2")
#end of season
points(Winter.data$AT,Winter.corT[[2]]$corADD, pch=19, col="deepskyblue1")
#middle of season
points(Winter.data$AT,Winter.corT[[3]]$corADD, pch=19, col="lavenderblush1")
#random
points(Winter.data$aT,Winter.corT[[3]]$corADD, pch=19, col="seagreen2")
#beggining of season
plot(Winter.data$AT,Winter.corT[[1]]$AT, pch=19, col="darkolivegreen4", xlab="observed air",
ylab="with error", main="uncorrected")
abline(0,1, lwd=3, col="red2")
#end of season
points(Winter.data$AT,Winter.corT[[2]]$AT, pch=19, col="deepskyblue4")
#middle of season
points(Winter.data$AT,Winter.corT[[3]]$AT, pch=19, col="lavenderblush4")
#random
points(Winter.data$AT,Winter.corT[[3]]$corADD, pch=19, col="seagreen4")


##########summer data
#beggining of season

plot(SummerallT[[1]]$AT,SummerallT[[1]]$corADD, pch=19, col="darkolivegreen3", xlab="observed air",
ylab="with error", main="corrected", xlim=c(0,3000), ylim=c(0,3000))
abline(0,1, lwd=3, col="red2")
#end of season
points(SummerallT[[1]]$AT,SummerallT[[2]]$corADD, pch=19, col="deepskyblue1")
#middle of season
points(SummerallT[[1]]$AT,SummerallT[[3]]$corADD, pch=19, col="lavenderblush1")
#random
points(SummerallT[[1]]$AT,SummerallT[[4]]$corADD, pch=19, col="seagreen2")
#beggining of season
plot(SummerallT[[1]]$AT,SummerallT[[1]]$ATerr, pch=19, col="darkolivegreen4", xlab="observed air",
ylab="with error", main="uncorrected", xlim=c(0,3000), ylim=c(0,3000))
abline(0,1, lwd=3, col="red2")
#end of season
points(SummerallT[[1]]$AT,SummerallT[[2]]$ATerr, pch=19, col="deepskyblue4")
#middle of season
points(SummerallT[[1]]$AT,SummerallT[[3]]$ATerr, pch=19, col="lavenderblush4")
#random
points(SummerallT[[1]]$AT,SummerallT[[4]]$ATerr, pch=19, col="seagreen4")


#check air
#beggining of season
par(mfrow=c(1,2))
plot(Winter.data$n,Winter.corT[[1]]$ncor, pch=19, col="darkolivegreen3", xlab="observed n",
ylab="with error", main="corrected")
abline(0,1, lwd=3, col="red2")
#end of season
points(Winter.data$n,Winter.corT[[2]]$ncor, pch=19, col="deepskyblue1")
#middle of season
points(Winter.data$n,Winter.corT[[3]]$ncor, pch=19, col="lavenderblush1")
#random
points(Winter.data$n,Winter.corT[[3]]$ncor, pch=19, col="seagreen2")
#beggining of season
plot(Winter.data$n,Winter.corT[[1]]$nerr, pch=19, col="darkolivegreen4", xlab="observed soil",
ylab="with error", main="uncorrected")
abline(0,1, lwd=3, col="red2")
#end of season
points(Winter.data$n,Winter.corT[[2]]$nerr, pch=19, col="deepskyblue4")
#middle of season
points(Winter.data$n,Winter.corT[[3]]$nerr, pch=19, col="lavenderblush4")
#random
points(Winter.data$n,Winter.corT[[3]]$nerr, pch=19, col="seagreen4")


##########summer data
#beggining of season

plot(SummerallT[[1]]$n,SummerallT[[1]]$ncor, pch=19, col="darkolivegreen3", xlab="observed n",
ylab="with error", main="corrected", xlim=c(0,2), ylim=c(0,2))
abline(0,1, lwd=3, col="red2")
#end of season
points(SummerallT[[1]]$n,SummerallT[[2]]$ncor, pch=19, col="deepskyblue1")
#middle of season
points(SummerallT[[1]]$n,SummerallT[[3]]$ncor, pch=19, col="lavenderblush1")
#random
points(SummerallT[[1]]$n,SummerallT[[4]]$ncor, pch=19, col="seagreen2")
#beggining of season
plot(SummerallT[[1]]$n,SummerallT[[1]]$nerr, pch=19, col="darkolivegreen4", xlab="observed n",
ylab="with error", main="uncorrected", xlim=c(0,2), ylim=c(0,2))
abline(0,1, lwd=3, col="red2")
#end of season
points(SummerallT[[1]]$n,SummerallT[[2]]$nerr, pch=19, col="deepskyblue4")
#middle of season
points(SummerallT[[1]]$n,SummerallT[[3]]$nerr, pch=19, col="lavenderblush4")
#random
points(SummerallT[[1]]$n,SummerallT[[4]]$nerr, pch=19, col="seagreen4")

##################data need to be filtered

#now look at prediction:
Wcor<-list()
Wuncor<-list()
Scor<-list()
Suncor<-list()

AWcor<-list()
AWuncor<-list()
AScor<-list()
ASuncor<-list()

WNcor<-list()
WNuncor<-list()
SNcor<-list()
SNuncor<-list()


for(i in 1:4){
	Wcor[[i]]<-summary(lm(Winter.corT[[i]]$corDD~Winter.data$T))
	Wuncor[[i]]<-summary(lm(Winter.corT[[i]]$T~Winter.data$T))
	Scor[[i]]<-summary(lm(SummerallT[[i]]$corDD~SummerallT[[i]]$T))
	Suncor[[i]]<-summary(lm(SummerallT[[i]]$Terr~SummerallT[[i]]$T))
	AWcor[[i]]<-summary(lm(Winter.corT[[i]]$corDD~Winter.data$T))
	AWuncor[[i]]<-summary(lm(Winter.corT[[i]]$AT~Winter.data$AT))
	AScor[[i]]<-summary(lm(SummerallT[[i]]$corADD~SummerallT[[i]]$AT))
	ASuncor[[i]]<-summary(lm(SummerallT[[i]]$ATerr~SummerallT[[i]]$AT))
	WNcor[[i]]<-summary(lm(Winter.corT[[i]]$ncor~Winter.data$n))
	WNuncor[[i]]<-summary(lm(Winter.corT[[i]]$nerr~Winter.data$n))
	SNcor[[i]]<-summary(lm(SummerallT[[i]]$ncor~SummerallT[[i]]$n))
	SNuncor[[i]]<-summary(lm(SummerallT[[i]]$nerr~SummerallT[[i]]$n))	
	}
Wcomp<-list()
Scomp<-list()	
AWcomp<-list()
AScomp<-list()	
WNcomp<-list()
SNcomp<-list()	
for(i in 1:4){
	Wcomp[[i]]<-data.frame(corrected=c(Wcor[[i]]$coefficients[1,1],Wcor[[i]]$coefficients[2,1],
										Wcor[[i]]$r.squared),
							uncorrected=c(Wuncor[[i]]$coefficients[1,1],Wuncor[[i]]$coefficients[2,1],
										Wuncor[[i]]$r.squared))
										
	Scomp[[i]]<-data.frame(corrected=c(Scor[[i]]$coefficients[1,1],Scor[[i]]$coefficients[2,1],
								Scor[[i]]$r.squared),
							uncorrected=c(Suncor[[i]]$coefficients[1,1],Suncor[[i]]$coefficients[2,1],
									Suncor[[i]]$r.squared))	
	AWcomp[[i]]<-data.frame(corrected=c(AWcor[[i]]$coefficients[1,1],AWcor[[i]]$coefficients[2,1],
										AWcor[[i]]$r.squared),
							uncorrected=c(AWuncor[[i]]$coefficients[1,1],AWuncor[[i]]$coefficients[2,1],
										AWuncor[[i]]$r.squared))
										
	AScomp[[i]]<-data.frame(corrected=c(AScor[[i]]$coefficients[1,1],AScor[[i]]$coefficients[2,1],
								AScor[[i]]$r.squared),
							uncorrected=c(ASuncor[[i]]$coefficients[1,1],ASuncor[[i]]$coefficients[2,1],
									ASuncor[[i]]$r.squared))										
	WNcomp[[i]]<-data.frame(corrected=c(WNcor[[i]]$coefficients[1,1],WNcor[[i]]$coefficients[2,1],
										WNcor[[i]]$r.squared),
							uncorrected=c(WNuncor[[i]]$coefficients[1,1],WNuncor[[i]]$coefficients[2,1],
										WNuncor[[i]]$r.squared))
										
	SNcomp[[i]]<-data.frame(corrected=c(SNcor[[i]]$coefficients[1,1],SNcor[[i]]$coefficients[2,1],
								SNcor[[i]]$r.squared),
							uncorrected=c(SNuncor[[i]]$coefficients[1,1],SNuncor[[i]]$coefficients[2,1],
									SNuncor[[i]]$r.squared))											
										
}

Wcompdf<-ldply(Wcomp,data.frame)
Scompdf<-ldply(Scomp,data.frame)
AWcompdf<-ldply(AWcomp,data.frame)
AScompdf<-ldply(AScomp,data.frame)
WNcompdf<-ldply(WNcomp,data.frame)
SNcompdf<-ldply(SNcomp,data.frame)			

Wdf<-data.frame(param=rep(c("intercept","slope","R.sq"), times=4),
				Type=rep(c("beg.seas", "end.seas", "mid.seas","rand.seas"), each=3),
				Soil.cor=Wcompdf$corrected,Soil.uncor=Wcompdf$uncorrected,
				Air.cor=AWcompdf$corrected, Air.uncor=AWcompdf$uncorrected,
				 N.cor=WNcompdf$corrected, N.uncor=WNcompdf$corrected, Var=rep("Winter FDD", 12))
			
Sdf<-data.frame(param=rep(c("intercept","slope","R.sq"), times=4),
				Type=rep(c("beg.seas", "end.seas", "mid.seas","rand.seas"), each=3),
				Soil.cor=Scompdf$corrected,Soil.uncor=Scompdf$uncorrected,
				Air.cor=AScompdf$corrected, Air.uncor=AScompdf$uncorrected,
				 N.cor=SNcompdf$corrected, N.uncor=SNcompdf$corrected,Var=rep("Summer TDD", 12))

Allcompdf<-rbind(Wdf,Sdf)				 
#write.table(Allcompdf, "c:\\Users\\hkropp\\Google Drive\\Plots_for_data_quality_check\\Comp95perc.csv",
#			sep=",", row.names=FALSE)	

}		

##############################################################################
##############################################################################
###############The data correction for 5% missing data is ####################
###############sufficient so can start correcting data #######################
##############################################################################

#change colnames of data frames with the information on sites to correct
colnames(S.toCor)<-c("depth","year","count","siteid")
colnames(W.toCor)<-c("depth","wyear","count","siteid")
colnames(AS.toCor)<-c("depth","year","count","siteid")
colnames(AW.toCor)<-c("depth","wyear","count","siteid")


#Need to calculate proportion of data present

S.toCor$prop<-round(S.toCor$count/154,2)
W.toCor$prop<-round(W.toCor$count/212,2)
AS.toCor$prop<-round(AS.toCor$count/154,2)
AW.toCor$prop<-round(AW.toCor$count/212,2)

#join based on what data to use to meet criteria
S.forcorr<-join(SummT,S.toCor, by=c("depth", "year","siteid"), type="inner")
W.forcorr<-join(WintT,W.toCor, by=c("depth", "wyear","siteid"), type="inner")
AS.forcorr<-join(ASummT,AS.toCor, by=c("depth", "year","siteid"), type="inner")
AW.forcorr<-join(AWintT,AW.toCor, by=c("depth", "wyear","siteid"), type="inner")


#get all the days for freezing and thawing indices
S.thawcorr<-S.forcorr[S.forcorr$T>0,]
W.freezecorr<-W.forcorr[W.forcorr$T<0,]
AS.thawcorr<-AS.forcorr[AS.forcorr$A>0,]
AW.freezecorr<-AW.forcorr[AW.forcorr$A<0,]
#add up all days that are freezing or thawing for degree days
#include proprotion of observations even thought it isn't something to vary by
#just to keep it in the table
Sthaw.ddtocorr<-aggregate(S.thawcorr$T, 
			by=list(S.thawcorr$prop,S.thawcorr$depth,S.thawcorr$year,S.thawcorr$siteid), FUN="sum")
Sfreeze.ddtocorr<-aggregate(W.freezecorr$T, 
			by=list(W.freezecorr$prop,W.freezecorr$depth,W.freezecorr$wyear,W.freezecorr$siteid), FUN="sum")		
Athaw.ddtocorr<-aggregate(AS.thawcorr$A, 
			by=list(AS.thawcorr$prop,AS.thawcorr$depth,AS.thawcorr$year,AS.thawcorr$siteid), FUN="sum")
Afreeze.ddtocorr<-aggregate(AW.freezecorr$A, 
			by=list(AW.freezecorr$prop,AW.freezecorr$depth,AW.freezecorr$wyear,AW.freezecorr$siteid), FUN="sum")
			
#Now correct the data
Sthaw.ddtocorr$corDD<-Sthaw.ddtocorr$x/Sthaw.ddtocorr$Group.1
Sfreeze.ddtocorr$corDD<-Sfreeze.ddtocorr$x/Sfreeze.ddtocorr$Group.1		
Athaw.ddtocorr$corDD<-Athaw.ddtocorr$x/Athaw.ddtocorr$Group.1
Afreeze.ddtocorr$corDD<-Afreeze.ddtocorr$x/Afreeze.ddtocorr$Group.1		

#now make dataframes that only have groups that are needed.
Sthaw.c<-data.frame(depth=Sthaw.ddtocorr$Group.2,year=Sthaw.ddtocorr$Group.3,siteid=
						Sthaw.ddtocorr$Group.4,T=Sthaw.ddtocorr$corDD)
Sfreeze.c<-data.frame(depth=Sfreeze.ddtocorr$Group.2,wyear=Sfreeze.ddtocorr$Group.3,siteid=
						Sfreeze.ddtocorr$Group.4,T=Sfreeze.ddtocorr$corDD)
Athaw.c<-data.frame(depth=Athaw.ddtocorr$Group.2,year=Athaw.ddtocorr$Group.3,siteid=
						Athaw.ddtocorr$Group.4,AT=Athaw.ddtocorr$corDD)
Afreeze.c<-data.frame(depth=Afreeze.ddtocorr$Group.2,wyear=Afreeze.ddtocorr$Group.3,siteid=
						Afreeze.ddtocorr$Group.4,AT=Afreeze.ddtocorr$corDD)

#need to make a table of of freezing and thawing where data matches
Winter.datac<-join(Sfreeze.c,Afreeze.c, by=c("wyear","siteid"), type="inner")
Summer.datac<-join(Sthaw.c,Athaw.c,by=c("year","siteid"), type="inner")
#calculate n factor
Winter.datac$n<-Winter.datac$T/Winter.datac$AT
Summer.datac$n<-Summer.datac$T/Summer.datac$AT

range(Summer.datac$AT)

#not add to the full data that didn't need corrections
#rename to match
colnames(Winter.datac)<-c("depth", "wyear", "siteid", "T", "height","AT","n")
#bind two together
Winter.All<-rbind(Winter.data,Winter.datac)
#make sure there are no repeat sites
checkforwinter<-aggregate(Winter.All$T, by=list(Winter.All$depth, Winter.All$wyear, Winter.All$siteid), 
							FUN="length")
which(checkforwinter$x>1)
#rename to match
colnames(Summer.datac)<-c("depth", "year", "siteid", "T", "height","AT","n")
#bind two together
Summer.All<-rbind(Summer.data,Summer.datac)
#make sure there are no repeat sites
checkforsummer<-aggregate(Summer.All$T, by=list(Summer.All$depth, Summer.All$year, Summer.All$siteid), 
							FUN="length")
which(checkforsummer$x>1)


#now combine with siteinfo

N.wintA<-join(Winter.All,siteinf,by=c("siteid"), type="inner")
N.summA<-join(Summer.All,siteinf,by=c("siteid"), type="inner")


####################################################################
####################################################################
###########Look at some initial plots###############################
####################################################################

plot(N.wintA$lat[N.wintA$depth<10],N.wintA$n[N.wintA$depth<10], pch=19,
	xlab="latitude",ylab="Winter N factor")
	
	
N.wintA$new_vA<-ifelse(N.wintA$vege_z=="boreal forest", "boreal",
		ifelse(	N.wintA$vege_z=="boreal", "boreal",
		ifelse(	N.wintA$vege_z=="tundra", "tundra",
		ifelse(	is.na(N.wintA$vege_z),"unclassified","unclassified"))))
	
N.summA$Snew_vA<-ifelse(N.summA$vege_z=="boreal forest", "boreal",
		ifelse(	N.summA$vege_z=="boreal", "boreal",
		ifelse(	N.summA$vege_z=="tundra", "tundra",
		ifelse(	is.na(N.summA$vege_z),"unclassified","unclassified"))))
		
#get sample size
Wnumbs<-aggregate(N.wintA$n[N.wintA$depth<10], by=list(as.factor(new_vA[N.wintA$depth<10])),
		FUN="length")
		
Snumbs<-aggregate(N.summA$n[N.summA$depth<10],by=list(as.factor(Snew_vA[N.summA$depth<10])),
		FUN="length")
#make box plot
par(mfrow=c(1,2))	
plot(N.wintA$n[N.wintA$depth<10]~as.factor(new_vA[N.wintA$depth<10]), pch=19,
	xlab="biome",ylab="Winter N factor")
text(1.3,1.2, paste("n=",Wnumbs$x[1]), cex=2)
text(2.3,1.2, paste("n=",Wnumbs$x[2]), cex=2)
text(3.3,1.2, paste("n=",Wnumbs$x[3]), cex=2)

plot(N.summA$n[N.summA$depth<10]~as.factor(Snew_vA[N.summA$depth<10]), pch=19,
	xlab="biome",ylab="Summer N factor")
	
text(1.3,1.65, paste("n=",Snumbs$x[1]), cex=2)
text(2.3,1.65, paste("n=",Snumbs$x[2]), cex=2)
text(3.3,1.65, paste("n=",Snumbs$x[3]), cex=2)

mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)

##########################################################################
##########################################################################
######### Revisit some of the unclassified areas to see ##################
######### what can be classified                        ##################
##########################################################################

#sites 23-28 in summer are marked as unclassified but they are all in a
#tundra zone
N.summA$Snew_vA[N.summA$siteid>=22&N.summA$siteid<=28]<-"tundra"
N.wintA$new_vA[N.wintA$siteid>=22&N.wintA$siteid<=28]<-"tundra"
#get re calculate 
Wnumbs<-aggregate(N.wintA$n[N.wintA$depth<10], by=list(as.factor(N.wintA$new_vA[N.wintA$depth<10])),
		FUN="length")
		
Snumbs<-aggregate(N.summA$n[N.summA$depth<10],by=list(as.factor(N.summA$Snew_vA[N.summA$depth<10])),
		FUN="length")

#####################################################################
######################################################################
########Redo initial plots ##########################################
#####################################################################		
		
#make box plot
par(mfrow=c(1,2))	
plot(N.wintA$n[N.wintA$depth<10]~as.factor(N.wintA$new_vA[N.wintA$depth<10]), pch=19,
	xlab="biome",ylab="Winter N factor")
text(1.3,1.2, paste("n=",Wnumbs$x[1]), cex=2)
text(2.3,1.2, paste("n=",Wnumbs$x[2]), cex=2)


plot(N.summA$n[N.summA$depth<10]~as.factor(N.summA$Snew_vA[N.summA$depth<10]), pch=19,
	xlab="biome",ylab="Summer N factor")
	
text(1.3,1.65, paste("n=",Snumbs$x[1]), cex=2)
text(2.3,1.65, paste("n=",Snumbs$x[2]), cex=2)


mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)
#make plot of latitude
par(mfrow=c(1,2))	
plot(N.wintA$lat[N.wintA$depth<10&N.wintA$new_vA=="tundra"],N.wint$n[N.wintA$depth<10&N.wintA$new_vA=="tundra"], pch=19,
	xlab="latitude",ylab="Winter N factor", col="cadetblue3")
points(N.wintA$lat[N.wint$depth<10&N.wintA$new_vA=="boreal"],N.wintA$n[N.wint$depth<10&N.wintA$new_vA=="boreal"], pch=19,
	 col="forestgreen")
legend(58,1.15,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")
plot(N.summA$lat[N.summA$depth<10&N.summA$Snew_vA=="tundra"],N.summA$n[N.summA$depth<10&N.summA$Snew_vA=="tundra"], pch=19,
	xlab="latitude",ylab="Summer N factor", col="cadetblue3")		
points(N.summA$lat[N.summA$depth<10&N.summA$Snew_vA=="boreal"],N.summA$n[N.summA$depth<10&N.summA$Snew_vA=="boreal"], pch=19,
	col="forestgreen")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)	
legend(58,1.77,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")
#make plot of year
par(mfrow=c(1,2))	
plot(N.wintA$wyear[N.wintA$depth<10&N.wintA$new_vA=="tundra"],N.wintA$n[N.wintA$depth<10&N.wintA$new_vA=="tundra"], pch=19,
	xlab="year",ylab="Winter N factor", col="cadetblue3", xlim=c(1987,2016))
points(N.wintA$wyear[N.wintA$depth<10&N.wintA$new_vA=="boreal"],N.wintA$n[N.wintA$depth<10&N.wintA$new_vA=="boreal"], pch=19,
	 col="forestgreen")
	legend(1987,1.33,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")
plot(N.summA$year[N.summA$depth<10&N.summA$Snew_vA=="tundra"],N.summA$n[N.summA$depth<10&N.summA$Snew_vA=="tundra"], pch=19,
	xlab="year",ylab="Summer N factor", col="cadetblue3")		
points(N.summA$year[N.summA$depth<10&N.summA$Snew_vA=="boreal"],N.summA$n[N.summA$depth<10&N.summA$Snew_vA=="boreal"], pch=19,
	col="forestgreen")
mtext("0-10cm depth", side=3, outer=TRUE, cex=2, line=-3)
legend(1991,1.77,c("boreal","tundra"), pch=19, col=c("forestgreen","cadetblue3"), cex=1.5, bty="n")

###############################################################################
###############################################################################
###############################################################################
############# Output N factor and DD for further analysis #####################
###############################################################################


#output summer
write.table(N.summA, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\SummerN.csv", sep=",",
			row.names=FALSE)	
write.table(N.wintA, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\WinterN.csv", sep=",",
			row.names=FALSE)	



	
##################################################################################
##################################################################################
############################Calculate T diff #####################################
##################################################################################