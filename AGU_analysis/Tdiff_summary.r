#################################################################################
########This script organizes data and looks at summary #########################
########stats for Tdiff for the AGU presentation        #########################
#################################################################################
library(plyr)


#read in temperature difference data
datT<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Tdiff_shallow_out.csv")

####site 74 has -999 in the data so exclude these cases
datT<-datT[datT$aT!=-999,]

#set up an id for a region
#read in data from region generated in GIS
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\region_siteinfo.csv")
reg.to.join<-data.frame(siteid=datR$site_id,regionid=datR$region_id,region.name=datR$region_nam)


#need to get together site id
Sites<-data.frame(siteid=sort.int(unique(datT$siteid)))
Sites$siteidm<-seq(1,dim(Sites)[1])

#add to datT
datTi<-join(datT,Sites, by="siteid",type="inner")

#now need to create a year id

Years<-data.frame(year=sort.int(unique(datTi$year)))
Years$yearid<-seq(1, dim(Years)[1])
#add yearid
datTii<-join(datTi,Years, by="year",type="inner")

datTii$dayX<-datTii$dayseq/153

datTiii<-join(datTii,reg.to.join,by="siteid",type="inner")

#how many datapoints for a site
regioncount<-aggregate(datTiii$TdiffA,by=list(datTiii$siteid, datTiii$regionid), FUN="length")
#see how many sites are in each region
siteReg<-aggregate(regioncount$Group.1,by=list(regioncount$Group.2), FUN="length")
#site 17 is alone in region 6 
#to avoid overparameterizing the hudson bay will be grouped with interior canada for now, region 5
datTiii$region.fix<-ifelse(datTiii$regionid==6,5,datTiii$regionid)


#need a region id for site
siteRegI<-unique(data.frame(siteidm=datTiii$siteidm,siteid=datTiii$siteid,regid=datTiii$region.fix))
sitRegII<-siteRegI[order(siteRegI$siteidm),]
#now see how many unique regions there are
newreg.df<-data.frame(regid=sort.int(unique(sitRegII$regid)))
#since 6 is missing need to create a new id
newreg.df$regidm<-seq(1,dim(newreg.df)[1])

#join to region table
siteRegIII<-join(sitRegII,newreg.df, by="regid", type="inner")


#get some T diff stats
#only look from 1-10cm
datTiii<-datTiii[datTiii$depth>0&datTiii$depth<=10,]

#get the average Tdiff at the beginning, end, and middle of the growing seas

Tdiffmid<-datTiii[datTiii$dayX>=.4&datTiii$dayX<=.6,]

#now take the average per per site and year
meanDmid<-aggregate(Tdiffmid$TdiffA,by=list(Tdiffmid$siteid,Tdiffmid$year), FUN="mean")
nDmid<-aggregate(Tdiffmid$TdiffA,by=list(Tdiffmid$siteid,Tdiffmid$year), FUN="length")
#exclude anything with limited measurements less than 14 days
meanDmid<-meanDmid[nDmid$x>14,]
nDmid<-nDmid[nDmid$x>14,]


#just focus on a few years

plot(meanDmid$Group.1[meanDmid$Group.2==2013],meanDmid$x[meanDmid$Group.2==2013],pch=19)
colnames(meanDmid)<-c("siteid", "year", "Tdiff")
#see how many observations in year
meanDyear<-aggregate(meanDmid$Tdiff, by=list(meanDmid$year), FUN="length")

#write to table
write.table(meanDmid[meanDmid$year==2013,], "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Tdiff_2013.csv", sep=",", row.names=FALSE)

write.table(meanDmid[meanDmid$year==2014,], "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Tdiff_2014.csv", sep=",", row.names=FALSE)
write.table(meanDmid[meanDmid$year==2012,], "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Tdiff_2012.csv", sep=",", row.names=FALSE)