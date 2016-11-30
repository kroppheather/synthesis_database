################################################################################
##########this file organizes snow water equivilent data #######################
##########to  be matched up with the model               #######################
################################################################################

### set wd
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

#data file from GlobSNOW data extracted by
#Mike Loranty on November 29 2016
datSW<-read.csv("site_SWE_data.csv")

#Thus just look at SWE for May-Oct
#since data starts in 1990, remove unnecessary
#years
datSWf1<-cbind(datSW[,1:17],datSW[,101:356])

#pull out date information
timeIn<-gsub("\\D","",names(datSWf1)[18:dim(datSWf1)[2]])
timeY<-as.numeric(substr(timeIn, 1,4))
timeM<-as.numeric(substr(timeIn,5,6))
timedf<-data.frame(year=timeY,month=timeM, colm=seq(18,dim(datSWf1)[2]))

#there are also some missing months
#so see how many years this is possible for
#also filter out anytime before 1989
#there should be 26 years total

#filter to focus only on winter months
timeW<-timedf[timedf$month>=10|timedf$month<5,]
#create a w year column
timeW$w.year<-ifelse(timeW$month<5,timeW$year,timeW$year+1)

#see how many winter years are missing a month
yearL<-aggregate(timeW$month, by=list(timeW$w.year), FUN="length")
#all are present except 2015. 

#exclude 2015 since it doesn't have complete data
timeW<-timeW[timeW$w.year>=1990&timeW$w.year<2015,]

#now need to replace negative values with NA

datSWF1f<-datSWf1
for(i in 18:dim(datSWf1)[2]){
	datSWF1f[,i]<-ifelse(datSWf1[,i]<0,NA,datSWf1[,i])
}


#now need to add up total swe for the season
#get the unique w years
#get the start and end to sum over for columns
#make a new list of thecolumn


wyearl<-unique(timeW$w.year)
YMstart<-numeric()
YMend<-numeric()
for(i in 1:length(wyearl)){
	YMstart[i]<-timeW$colm[timeW$w.year==wyearl[i]&timeW$month==10]
	YMend[i]<-timeW$colm[timeW$w.year==wyearl[i]&timeW$month==4]
}


sumSWEyear<-matrix(rep(NA, length(wyearl)*dim(datSWF1f)[1]), ncol=length(wyearl))
for(j in 1:dim(datSWF1f)[1]){
	for(i in 1:length(wyearl)){
	sumSWEyear[j,i]<-sum(datSWF1f[j,YMstart[i]:YMend[i]])/7
	}
}

colnames(sumSWEyear)<-paste0("SWE", wyearl)

#now add back in the site information
datSWE.all<-cbind(datSW[1:17],sumSWEyear)


#write output to file

write.table(datSWE.all, "SWE_calc_site.csv", sep=",", row.names=FALSE)