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


#now get data together for the model
datalist<-list(Nobs=dim(datTiii)[1], Tdiff=datTiii$TdiffA,SeasX=datTiii$dayX, siteid=datTiii$siteidm,yearid=datTiii$yearid,
				Nsites=dim(Sites)[1],NyearS=dim(Years)[1], xS=rep(1,dim(Years)[1]), yS=Years$year, regionid=siteRegIII$regidm,
				Nreg=dim(newreg.df)[1])
				
samplelist<-c("eps.star", "rho.eps", "beta2", "beta3", "beta4", "beta5", "beta1star","sig.Td","sig.eps","Tdiff.rep",
				"mu.beta1","mu.beta2","mu.beta3","mu.beta4","mu.beta5","sig.beta1","sig.beta2","sig.beta3","sig.beta4","sig.beta5")

inits<-list(list(t.eps=1,rho.eps=.99),
			list(t.eps=1.5,rho.eps=.89),
			list(t.eps=.5,rho.eps=.79))

T.model.init=jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Tdiff_model\\Tdiff_model_code.r",
						data=datalist,
						n.adapt=2000,
						n.chains=3,
						inits=inits)
						
n.iter.i=30000
codaobj.init = coda.samples(T.model.init,variable.names=samplelist,
                       n.iter=n.iter.i, thin=15)

codatoplot<-ggs(codaobj.init)
ggmcmc(codatoplot, family=c("beta1star","beta2","beta3","beta4","beta5"))
				   
windows(18)
plot(codaobj.init[,"mu.beta4[3]",])

Mod.out<-summary(codaobj.init)	

write.table(Mod.out$statistics, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_Tdiff_stats.csv",
			sep=",",row.names=TRUE)
write.table(Mod.out$quantiles, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_Tdiff_quant.csv",
			sep=",",row.names=TRUE)

#ouput matching T diff for results
write.table(datTiii,"c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\datTdiff_model.csv",sep=",", row.names=FALSE)