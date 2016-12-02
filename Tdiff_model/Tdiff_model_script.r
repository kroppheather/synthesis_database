#################################################################################
########This script organizes data and runs the model for #######################
########Soil and air temperature difference data   1-10cm #######################
#################################################################################
library(plyr)
library(rjags)
library(xtable)
library(ggmcmc)
#read in temperature difference data
datT<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Tdiff_shallow_out.csv")

#set up an id for a region
#start by just treating 1 degree of lat and long a reason
#everything rounded down to the degree is treated as the same region

datT$lat.f<-floor(datT$lat)
datT$lon.f<-floor(datT$lon)
#aggregate into an unique id
regions<-unique(data.frame(lat.f=datT$lat.f,lon.f=datT$lon.f))
regions$regID<-seq(1,dim(regions)[1])

datT<-join(datT,regions,by=c("lat.f","lon.f"), type="left")

#now look at how observations in sites and regions
regSumm<-aggregate(datT$TdiffA, by=list(datT$siteid,datT$regID), FUN="length")
#now look at how many sites in each region
siteRcount<-aggregate(regSumm$Group.1,by=list(regSumm$Group.2), FUN="length")

#####this model should be done heirarchically but more thought needs to 
#####be put into how to categorize a region. It is probably more appropriate
#####to group sites by vegetation type or community.
#####This is an area to work on and think about. However, in the mean time
#####allow regression parameters to vary by site to see if the model is
##### even appropriate for Tdiff. This is because just goint off lat,long
##### results in a high number of cases where only 1 site is in a region


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

#now get data together for the model
datalist<-list(Nobs=dim(datTii)[1], Tdiff=datTii$TdiffA,SeasX=datTii$dayX, siteid=datTii$siteidm,yearid=datTii$yearid,
				Nsites=dim(Sites)[1],NyearS=dim(Years)[1], xS=rep(1,dim(Years)[1]), yS=Years$year)
				
samplelist<-c("eps.star", "rho.eps", "beta2", "beta3", "beta1star","sig.Td","sig.eps")

inits<-list(list(t.eps=1,rho.eps=.99),
			list(t.eps=1.5,rho.eps=.89),
			list(t.eps=.5,rho.eps=.79))

T.model.init=jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Tdiff_model\\Tdiff_model_code.r",
						data=datalist,
						n.adapt=1000,
						n.chains=3,
						inits=inits)
						
n.iter.i=30000
codaobj.init = coda.samples(T.model.init,variable.names=Samplelist,
                       n.iter=n.iter.i, thin=10)
