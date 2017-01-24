#############################################################################
###########This file links vegetation data to site info #####################
########### and compares global datasets to site level  #####################
#############################################################################
library(plyr)
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3")
#read in files for site vege data
#site info
datSites<-read.table("siteinfo.csv", sep=",", head=TRUE, na.strings=c("NaN"))
#soil info
datSoil<-read.table("soil.csv", sep=",", head=TRUE, na.strings=c("NaN"))
#EVI
datEVI<-read.csv("Site_EVI_out.csv")
#read in soil and soil c data from Hugelius et al 2013
datCA<-read.csv("Export_soilsc.csv")
#read in plot level soil c data
datPC<-read.table("soilc.csv", sep=",", head=TRUE, na.strings=c("NaN"))

#read in species cover data
datSC<-read.csv("spcov2.csv", sep=",", head=TRUE, na.strings=c("NaN", "NA"))


#first compare arctic soil c to site
#carbon measu in kg
datCsub<-data.frame(siteid=datCA$site_id,lat=datCA$lat,lon=datCA$lon, 
			name=datCA$site_name, soilA=datCA$SOIL_AREA, 
			C30=datCA$SOCM_30CM, C100=datCA$SOCM_100CM)
#normalize by soil area m2
datCsub$n30<-as.numeric(datCsub$C30)/datCsub$soilA
datCsub$n100<-as.numeric(datCsub$C100)/datCsub$soilA

#convert to kg/m2
datPC$c.kg<-datPC$soilc/1000



Cplot<-data.frame(plotC=datPC$c.kg, siteid=datPC$site_id,depthobs=datPC$soilc_dep)

Call<-join(datCsub,Cplot,by="siteid", type="left")
{
#see if normalizing as c per m3
Call$dens30<-Call$n30/.3
Call$dens100<-Call$n30/1
Call$densobs<-Call$plotC/(Call$depthobs/100)


#look at how tow compare
par(mfrow=c(1,2))
plot(Call$densobs,Call$dens30, pch=19, ylim=c(0,70), xlim=c(0,70),
		xlab="Plot kg C/ m3", ylab="GC 30cm kg C/ m3")
plot(Call$densobs,Call$dens100, pch=19, ylim=c(0,70), xlim=c(0,70),
		xlab="Plot kg C/ m3", ylab="GC 100cm kg C/ m3")
		
#30cm gets closer to plot obs, see how this comares

fitC<-lm(Call$dens30~Call$densobs)
summary(fitC)


#plot with fit line
par(mfrow=c(1,2))
plot(Call$densobs,Call$dens30, pch=19, ylim=c(0,70), xlim=c(0,70),
		xlab="Plot kg C/ m3", ylab="GC 30cm kg C/ m3")
abline(fitC, lty=2, lwd=2)		
		
plot(Call$densobs,Call$dens100, pch=19, ylim=c(0,70), xlim=c(0,70),
		xlab="Plot kg C/ m3", ylab="GC 100cm kg C/ m3")
		
#plot residuals 
par(mfrow=c(1,2))
plot(Call$densobs,Call$dens30, pch=19, ylim=c(0,70), xlim=c(0,70),
		xlab="Plot kg C/ m3", ylab="GC 30cm kg C/ m3")
abline(fitC, lty=2, lwd=2)	

plot(na.omit(Call$densobs), fitC$residuals,pch=19)
abline(h=0, lty=2,lwd=2)
}

#use the 30cm kg/m2 of carbon estimate
Call.sub<-data.frame(siteid=Call$siteid,soilC=Call$n30 )


#create a smaller dataset
#first subset site info and  fill in missing tundra classification
datSites$vege_z[22:28]<-"tundra"
#now make boreal label consistent
datSites$vege_z<-ifelse(datSites$vege_z=="boreal forest", "boreal",
					ifelse(datSites$vege_z=="boreal","boreal",
					ifelse(datSites$vege_z=="tundra", "tundra",NA)))

datV<-data.frame(siteid=datSites$site_id,datSites[,2:4],biome=datSites$vege_z,
				datSites[,10:11])
				
#now combine with soil carbon data
datV<-join(datV,Call.sub, by="siteid", type="left")
######### next fix tundra missing data and plant cover classifications
#just focus on functional type right now
#get rid of any case issues
funcLC<-tolower(datSC$func_type)
#get the functional type names
Funct.all<-unique(funcLC)

#set up a vector to make names consistent 


#rename to have more consistent names
#moss here is defined as any bryophyte
funcT<- ifelse(funcLC=="Tall sedges", "sedge",
		ifelse(funcLC=="tussock sedge","tussock",
		ifelse(funcLC=="organic","bare ground",
		ifelse(funcLC=="soil","bare ground",
		ifelse(funcLC=="dryas integrifolia", "evergreen shrub",
		ifelse(funcLC=="herb","forb",
		ifelse(funcLC=="sedges","sedge",
		ifelse(funcLC=="forbs","forb",
		ifelse(funcLC=="mosses","moss",
		ifelse(funcLC=="heath","evergreen shrub",
		ifelse(funcLC=="grasses","graminoid",
		ifelse(funcLC=="gramminoid","graminoid",
		ifelse(funcLC=="bare soil","bare ground",funcLC
		)))))))))))))
		
#now check unique names again
Funct.allRN<-unique(funcT)

#now create an even simple classification that is
#just tree shrub ect
simpFT<-ifelse(funcT=="moss","moss",
		ifelse(funcT=="shrub","shrub",
		ifelse(funcT=="evergreen shrub", "shrub",
		ifelse(funcT=="deciduous shrub", "shrub",
		ifelse(funcT=="litter", "litter",
		ifelse(funcT=="bare ground", "bare ground",
		ifelse(funcT=="forb", "forb",
		ifelse(funcT=="sedge", "graminoid",
		ifelse(funcT=="tussock", "graminoid",
		ifelse(funcT=="graminoid","graminoid",
		ifelse(funcT=="deciduous tree", "tree",
		ifelse(funcT=="evergreen tree", "tree",
		ifelse(funcT=="deciduous coniferous tree", "tree","other"
				)))))))))))))

#start by aggregating across functional type
#aggregate by more detailed FT classification
perc.ftm<-aggregate(datSC$perc_cover, by=list(funcT,
											datSC$site_id),
					FUN="mean")
perc.sm<-aggregate(datSC$perc_cover, by=list(simpFT,
											datSC$site_id),
					FUN="mean")
colnames(perc.sm)<-c("FuncType", "siteid", "p.cover")	
#subset into data frames to join
Tree.c<-perc.sm[perc.sm$FuncType=="tree",]
colnames(Tree.c)[3]<-"tree.pc"
Shrub.c<-perc.sm[perc.sm$FuncType=="shrub",]
colnames(Shrub.c)[3]<-"shrub.pc"
Shrub.c<-Shrub.c[,2:3]
Moss.c<-perc.sm[perc.sm$FuncType=="moss",]
colnames(Moss.c)[3]<-"moss.pc"
Moss.c<-Moss.c[,2:3]
Ground.c<-perc.sm[perc.sm$FuncType=="bare ground",]
colnames(Ground.c)[3]<-"ground.pc"
Ground.c<-Ground.c[,2:3]
Gram.c<-perc.sm[perc.sm$FuncType=="graminoid",]
colnames(Gram.c)[3]<-"gram.pc"
Gram.c<-Gram.c[,2:3]


#now compile to a site list:
datV<-join(datV, Shrub.c, by="siteid", type="left")
datV<-join(datV, Moss.c, by="siteid", type="left")
datV<-join(datV, Ground.c, by="siteid", type="left")
datV<-join(datV, Gram.c, by="siteid", type="left")

head(datV)

#now add the soil organic layer

#subset so just the organic layer
datOLT<-data.frame(siteid=datSoil$site_id, OLT=datSoil$organic_thick )

#now join with vegetation data
datV<-join(datV, datOLT, by="siteid", type="left")

#join EVI data
#subset
datE<-data.frame(siteid=datEVI$siteid, EVI=datEVI$EVI)

datV<-join(datV, datE, by="siteid",type="left")


plot(datV$OLT,datV$soilC,pch=19 )
plot(datV$elev,datV$shrub.pc,pch=19 )
plot(datV$shrub.pc,datV$olt,pch=19 )

#write to data file
write.table(datV, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\vegetation_tempM.csv",
			sep=",", row.names=FALSE)