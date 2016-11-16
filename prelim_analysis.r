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
	
	
	
######################################################################################################
######################################################################################################
##################### See if N factors can be corrected if incomplete#################################
##################### Start by better assessing incomplete data  #####################################
######################################################################################################

S.use<-na.omit(Scountsdf[Scountsdf$x==154,])
W.use<-na.omit(Wcountsdf[Wcountsdf$x==212|Wcountsdf$x==213,])
AS.use<-na.omit(Acountsdf[Acountsdf$x==154,])
AW.use<-na.omit(Acountwdf[Acountwdf$x==212|Acountwdf$x==213,])
		
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
		
#figure out how sensitive correcting N factors where 90% of more of the data is present in a season.
#Start by looking at how sensitive excluding data and doing the calculation is on datasets where
#the enitre dataset is present for the season.

#first explore how much data changes

SdayLow<-round(0.9*154,0)
WdayLow<-round(0.9*212,0)

#See what data could be corrected
S.toCor<-na.omit(Scountsdf[Scountsdf$x<154&Scountsdf$x>=SdayLow,])
W.toCor<-na.omit(Wcountsdf[Wcountsdf$x<212&Wcountsdf$x>=WdayLow,])
AS.toCor<-na.omit(Acountsdf[Acountsdf$x<154&Acountsdf$x>=SdayLow,])
AW.toCor<-na.omit(Acountwdf[Acountwdf$x<212&Acountwdf$x>=WdayLow,])
#Note adjusting N factor for 90% of the season appears to drastically increase
#data available for analysis so it is worth exploring

#now need to look at how much a correction for degree days could influence it
#look at soil

#first exclude 10% of the days under 4 scenarios:
#1. all at beginning
#2. all at end
#3. all in middle
#4. dispersed randomly
S.missing<-154-SdayLow
W.missing<-212-WdayLow
SeasonLength<-c(154,212,154,212)
#set up missing data scenarios
MissingSubtract<-c(S.missing,W.missing,S.missing,W.missing)
BegSeasS<-MissingSubtract
BegSeasE<-SeasonLength
EndSeasS<-rep(1,4)
EndSeasE<-SeasonLength-MissingSubtract
MidSeasS
MidSeasE
RSeasList


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
	SFORCALC.list[[i]]<-Dataforallcalcs[[1]][Dataforallcalcs[[1]]$siteid==Subsitelist[[1]]$site[j]&
												Dataforallcalcs[[1]]$year==Subsitelist[[1]]$year[j]&
												Dataforallcalcs[[1]]$depth==Subsitelist[[1]]$depth[j],]
}
WFORCALC.list<-list()
for(i in 1:dim(Subsitelist[[2]])[1]){
	WFORCALC.list[[i]]<-Dataforallcalcs[[2]][Dataforallcalcs[[2]]$siteid==Subsitelist[[2]]$site[j]&
												Dataforallcalcs[[2]]$wyear==Subsitelist[[2]]$wyear[j]&
												Dataforallcalcs[[2]]$depth==Subsitelist[[2]]$depth[j],]
}
ASFORCALC.list<-list()
for(i in 1:dim(Subsitelist[[3]])[1]){
	ASFORCALC.list[[i]]<-Dataforallcalcs[[3]][Dataforallcalcs[[3]]$siteid==Subsitelist[[3]]$site[j]&
												Dataforallcalcs[[3]]$year==Subsitelist[[3]]$year[j]&
												Dataforallcalcs[[3]]$depth==Subsitelist[[3]]$depth[j],]
}
AWFORCALC.list<-list()
for(i in 1:dim(Subsitelist[[4]])[1]){
	AWFORCALC.list[[i]]<-Dataforallcalcs[[4]][Dataforallcalcs[[4]]$siteid==Subsitelist[[4]]$site[j]&
												Dataforallcalcs[[4]]$wyear==Subsitelist[[4]]$wyear[j]&
												Dataforallcalcs[[4]]$depth==Subsitelist[[4]]$depth[j],]
}


#turn into a list
ALLFORCALC.list<-list(SFORCALC.list,WFORCALC.list,ASFORCALC.list,AWFORCALC.list)

##now the data can be excluded according to each scenario

for(i in 1:4){
	for(j in 1:dim(Subsitelist[[i]]){
	begSeasonM[[i]][[j]]
	
	}

}





##################################################################################
##################################################################################
############################Calculate T diff #####################################
##################################################################################