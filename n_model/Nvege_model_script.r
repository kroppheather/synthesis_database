######################################
###This script runs a model ##########
###to look at drivers in    ##########
###variation of N factors   ##########
### with vegetation data.   ##########
### However, the lack of    ##########
### consistent vegetation   ##########
### data means this is a    ##########
### very reduced number of  ##########
### N factors for comparision#########
######################################
library(plyr)
library(rjags)
library(xtable)
library(ggmcmc)

##this model version starts with just
##looking at the amount of variation
##resulting from the distance between
##sensors, location, and year

#read in winter N factor data
datWN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\WinterNvege.csv")
#summer N factor data
datSN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\SummerNvege.csv")
#read in EVI data
datEVI<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Site_EVI_out.csv")

#there is a site in the summer that clearly has some numbers
#that are not correct for soil temperature. There is likely
#a large number used for a NA. This needs to be fixed, but
#until that can happen need filter out
datSN<-datSN[datSN$T<3000,]

#now filter to only focus on 0-10cm
datWN<-datWN[datWN$depth<=10,]
datSN<-datSN[datSN$depth<=10,]

#join the region id 
#read in data from region generated in GIS
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\region_siteinfo.csv")
reg.to.join<-data.frame(siteid=datR$site_id,regionid=datR$region_id,region.name=datR$region_nam)


#now add the EVI data to the data frame
datSNiii<-join(datSN, datEVI, by=c("siteid"), type="left")
datWNiii<-join(datWN, datEVI, by=c("siteid"), type="left")
#now join region

datSNiv<-join(datSNiii,reg.to.join, by="siteid", type="left")
datWNiv<-join(datWNiii,reg.to.join, by="siteid", type="left")


#look at how many sites actually have observations for vegetation
##summer


#now see what data loss is like for
#comb1SN<-na.omit(data.frame(datSNiv[1:11],vegez=datSNiv$Snew_vA, olt=datSNiv$olt, shrubC=datSNiv$shrub.pc,  regionid=datSNiv$regionid,EVI=datSNiv$EVI,))
comb3SN<-na.omit(data.frame(datSNiv[1:11],vegez=datSNiv$Snew_vA, olt=datSNiv$olt,shrubC=datSNiv$shrub.pc, 
				mossC=datSNiv$moss.pc,EVI=datSNiv$EVI, regionid=datSNiv$regionid))

#dim(na.omit(comb1SN))[1]
dim(na.omit(comb3SN))[1]


##winter


#now see what data loss is like for
#comb1WN<-na.omit(data.frame(datWNiv[1:11],vegez=datWNiv$new_vA, olt=datWNiv$olt, shrubC=datWNiv$shrub.pc,  regionid=datWNiv$regionid,EVI=datWNiv$EVI))
comb3WN<-na.omit(data.frame(datWNiv[1:11],vegez=datWNiv$new_vA, olt=datWNiv$olt,  shrubC=datWNiv$shrub.pc, 
				mossC=datWNiv$moss.pc,regionid=datWNiv$regionid,EVI=datWNiv$EVI))


#dim(na.omit(comb1WN))[1]
dim(na.omit(comb3WN))[1]

#just stick with comb3 for now
#need to create year and regionIDs

#get unique region id

RegionS<-data.frame(regionid=sort.int(unique(comb3SN$regionid)))
RegionW<-data.frame(regionid=sort.int(unique(comb3WN$regionid)))
#regions are the same between winter and summer 
RegionS$reg.mod<-seq(1,dim(RegionS)[1])


vegeSN<-join(comb3SN,RegionS,by="regionid",type="inner")
vegeWN<-join(comb3WN,RegionS,by="regionid",type="inner")

#now need to create year
#see if the model is missing years
yearS<-data.frame(year=sort.int(unique(vegeSN$year)))
yearS$yearid<-seq(1, dim(yearS)[1])


yearW<-data.frame(wyear=sort.int(unique(vegeWN$wyear)))
yearW$yearid<-seq(1, dim(yearS)[1])

vegeSNi<-join(vegeSN,yearS,by="year",type="inner")
vegeWNi<-join(vegeWN,yearW,by="wyear",type="inner")



datamodellist<-list(NobsS=dim(vegeSNi)[1],n.factS=vegeSNi$n,
					EVIS=vegeSNi$EVI, yearS=vegeSNi$yearid,
					regIDS=vegeSNi$reg.mod,OLTS=vegeSNi$olt,
					shrubCS=vegeSNi$shrubC,mossCS=vegeSNi$mossC,
					NyearS=dim(yearS)[1],
					xS=yearS$yearid, yS=rep(1,dim(yearS)[1]),
					Nreg=dim(RegionS)[1], 
					NobsW=dim(vegeWNi)[1], n.factW=vegeWNi$n, EVIW=vegeWNi$EVI,
					OLTW=vegeWNi$olt,
					shrubCW=vegeWNi$shrubC,mossCW=vegeWNi$mossC,
					yearW=vegeWNi$yearid,regIDW=vegeWNi$reg.mod,
					NyearW=dim(yearW)[1], xW=yearW$wyear,yW=rep(1,dim(yearW)[1]))
					
Samplelist<-c("deviance", "nbeta.star1W","nbeta.star1S", "nbeta2S","nbeta3S","nbeta4S","nbeta5S",
				"nbeta2W","nbeta3W","nbeta4W","nbeta5W",
				"eps.star", "sig.S", "sig.W",
				"mu.nS","epsW.star","mu.nW",
				"rho.epsW",
				"rho.eps")
				
inits<-list(list(t.eps=1,rho.eps=.99,t.epsW=1,rho.epsW=.99),
			list(t.eps=1.5,rho.eps=.89,t.epsW=1.5,rho.epsW=.89),
			list(t.eps=.5,rho.eps=.79,t.epsW=.5,rho.epsW=.79))
			
n.model.init=jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\n_model\\Nvege_model_code.r",
						data=datamodellist,
						n.adapt=10000,
						n.chains=3,
						inits=inits)
						
n.iter.i=90000
codaobj.init = coda.samples(n.model.init,variable.names=Samplelist,
                       n.iter=n.iter.i, thin=30)
					   


windows(18)
plot(codaobj.init[,"epsW.star[26]"], ask=TRUE)	


#generate summary

Mod.out<-summary(codaobj.init)	
				



