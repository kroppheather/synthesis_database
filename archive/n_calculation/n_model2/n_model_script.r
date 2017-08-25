library(plyr)
library(lubridate)
library(rjags)
library(coda)
library(xtable)
library(mcmcplots)

setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7")
#read in n factor outputs
datF<-read.csv("FreezeN_out.csv")
datT<-read.csv("ThawN_out.csv")
#siteinfo
datI<-read.table("siteinfo.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))

#species cover
datC<-read.table("spcov.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))
#species biomass
datB<-read.table("spec_bio.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
#read in soil organic layer data
datS<-read.table("soil.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
	
#join site info to datF and datT
colnames(datI)[1]<-"siteid"

Fall1<-join(datF,datI, by="siteid", type="left")
Tall1<-join(datT,datI, by="siteid", type="left")

#### organize vegetation data ###################
#################################################

#Names likely have slight variations in
#capitalization, plural, and spellint
#need to go through and create consistant names

#just focus on functional type right now
#get rid of any case issues
funcLC<-tolower(datC$func_type)
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
				
				
perc.ftm<-aggregate(datC$perc_cover, by=list(funcT,
											datC$site_id),
					FUN="sum")
perc.sm<-aggregate(datC$perc_cover, by=list(simpFT,
											datC$site_id),
					FUN="sum")
colnames(perc.sm)<-c("FuncType", "siteid", "p.cover")	
colnames(perc.ftm)<-c("FuncType", "siteid", "p.cover")


#some sites appear to have done percent cover in a non-additive way and 
#did ground area cover
#thus scale perc.sm to be from 0-100
perc.smM<-aggregate(perc.sm$p.cover, by=list(perc.sm$siteid), FUN="sum")
colnames(perc.smM)<-c("siteid", "max")
perc.sm2<-join(perc.sm, perc.smM, by="siteid", type="left")
perc.sm2$pcov.cor<-ifelse(perc.sm2$max>100,(perc.sm2$p.cover/perc.sm2$max)*100,perc.sm2$p.cover)

#subset into data frames to join
Tree.c<-perc.sm2[perc.sm2$FuncType=="tree",]
Tree.c<-data.frame(FuncType=Tree.c$FuncType, siteid=Tree.c$siteid, tree.pc=Tree.c$pcov.cor)


Shrub.c<-perc.sm2[perc.sm2$FuncType=="shrub",]
Shrub.c<-data.frame(FuncType=Shrub.c$FuncType, siteid=Shrub.c$siteid, shrub.pc=Shrub.c$pcov.cor)

Moss.c<-perc.sm2[perc.sm2$FuncType=="moss",]
Moss.c<-data.frame(FuncType=Moss.c$FuncType, siteid=Moss.c$siteid, moss.pc=Moss.c$pcov.cor)

Ground.c<-perc.sm2[perc.sm2$FuncType=="bare ground",]
Ground.c<-data.frame(FuncType=Ground.c$FuncType, siteid=Ground.c$siteid, ground.pc=Ground.c$pcov.cor)

Gram.c<-perc.sm2[perc.sm2$FuncType=="graminoid",]
Gram.c<-data.frame(FuncType=Gram.c$FuncType, siteid=Gram.c$siteid, gram.pc=Gram.c$pcov.cor)

#now join covers to 

Fall2<-join(Fall1,Shrub.c, by="siteid", type="left")
Tall2<-join(Tall1,Shrub.c, by="siteid", type="left")

Fall3<-join(Fall2,Moss.c, by="siteid", type="left")
Tall3<-join(Tall2,Moss.c, by="siteid", type="left")			
				

Fall4<-join(Fall3,Gram.c, by="siteid", type="left")
Tall4<-join(Tall3,Gram.c, by="siteid", type="left")		

Fall5<-join(Fall4,Ground.c, by="siteid", type="left")
Tall5<-join(Tall4,Ground.c, by="siteid", type="left")

##################################################################
#####organize organic layer thickness ############################

datOLT<-data.frame(siteid=datS$site_id, OLT=datS$organic_thick)					


Fall6<-join(Fall5,datOLT, by="siteid", type="left")
Tall6<-join(Tall5,datOLT, by="siteid", type="left")





####################################################################
#####read in EVI data  #############################################
####################################################################
datEV<-read.csv("Site_EVI_out.csv")
EVIsub<-data.frame(siteid=datEV$siteid, EVI=datEV$EVI)
#get only evi and siteid for join
Fall7<-join(Fall6, EVIsub, by="siteid", type="left")
Tall7<-join(Tall6, EVIsub, by="siteid", type="left")

#read in world clim data
datWC<-read.csv("world_clim.csv")
colnames(datWC)[1]<-"siteid"
Tall8<-join(Tall7, datWC, by="siteid", type="left")
Fall8<-join(Fall7, datWC, by="siteid", type="left")


######################################################################
########subset variables for model run ###############################
######################################################################
#eventually a missing data model will help use more datasets
#but for now look at a model run with only all data present
Tsub<-data.frame(siteid=Tall8$siteid, N.id=Tall8$ID,NT=Tall8$M, wyear=Tall8$wyear,lat=Tall8$lat,
				biome=Tall8$vege_z,OLT=Tall8$OLT, 
				depth=Tall8$depth, Tmax=Tall8$tmax, prec=Tall8$prec, 
				Tave=Tall8$tavg,Tmin=Tall8$tmin)

Tsub<-na.omit(Tsub)

Tsub$biomeID<-ifelse(Tsub$biome=="tundra", 2,1)

Fsub<-data.frame(siteid=Fall8$siteid, N.id=Fall8$ID,NT=Fall8$M, wyear=Fall8$wyear,lat=Fall8$lat,
				biome=Fall8$vege_z, OLT=Fall8$OLT, 
				depth=Fall8$depth,
				Tmax=Fall8$tmax, prec=Fall8$prec, 
				Tave=Fall8$tavg,Tmin=Fall8$tmin
				)

Fsub<-na.omit(Fsub)

Fsub$biomeID<-ifelse(Fsub$biome=="tundra", 2,1)

####NEXT:
#check all wyears are present and set up y vectors for them.

FyearID<-data.frame(wyear=sort(unique(Fsub$wyear)))
FyearID$yearID<-seq(1,dim(FyearID)[1])

TyearID<-data.frame(wyear=sort(unique(Tsub$wyear)))
TyearID$yearID<-seq(1,dim(TyearID)[1])

#all 26 years are present, can just subtract off of the wyear column

#####################################################
###now set up indicater for if the observation is in
### is in mineral vs organic
Tsub$orgID<-ifelse(Tsub$depth<=Tsub$OLT,1,2)
Fsub$orgID<-ifelse(Fsub$depth<=Fsub$OLT,1,2)


#see if we can seperate out minveral under OLT vs mineral in the entire column
#create an id just indicate if there is organic present at a site
ToltIND<-data.frame(siteid=unique(Tsub$siteid[Tsub$orgID==1]))
ToltIND$Oind<-rep(1, dim(ToltIND)[1])

FoltIND<-data.frame(siteid=unique(Fsub$siteid[Fsub$orgID==1]))
FoltIND$Oind<-rep(1, dim(FoltIND)[1])

Tsub2<-join(Tsub,ToltIND, by="siteid", type="left")
Fsub2<-join(Fsub,ToltIND, by="siteid", type="left")

Fsub2$O.id<-ifelse(is.na(Fsub2$Oind),0,Fsub2$Oind)
Tsub2$O.id<-ifelse(is.na(Tsub2$Oind),0,Tsub2$Oind)

#now set up id for 3 types of soil
Tsub2$SOILID<-ifelse(Tsub2$orgID==1,1,
				ifelse(Tsub2$orgID==2&Tsub2$O.id==1,2,3))

Fsub2$SOILID<-ifelse(Fsub2$orgID==1,1,
				ifelse(Fsub2$orgID==2&Fsub2$O.id==1,2,3))
				
				
#read in data to look at region
datR<-read.csv("region.csv")
#join to the tables
colnames(datR)[1]<-"siteid"

Tsub3<-join(Tsub2,datR, by="siteid", type="left")
Fsub3<-join(Fsub2,datR, by="siteid", type="left")

#read in vegetation data to set up vegetation class

datVC<-read.csv("vegeClass.csv")
colnames(datVC)[1]<-"siteid"

Tsub4<-join(Tsub3,datVC, by="siteid", type="left")
Fsub4<-join(Fsub3,datVC, by="siteid", type="left")



#now see if there are enough observations to model by these groups
#make a vegetation class ID that reflects the actual number
Tsub4$classID<-ifelse(Tsub4$class>=7, Tsub4$class-1,Tsub4$class)
Fsub4$classID<-ifelse(Fsub4$class>=7, Fsub4$class-1,Fsub4$class)


#classID code
#1= herb barren
#2 = grasstundra
#3= tussock tundra
#4= shrub tundra
#5= wetland
#6= evergreen boreal
#7= mixed boreal



###loook at n factor vs air and soil temp

#read in data

datAM<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod1\\output_u7\\Tair_model.csv")
datSM<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\Tmod1\\output_u7\\Tsoil_model.csv")
datAM<-na.omit(datAM)
Airmax<-aggregate(datAM$A, by=list(datAM$siteid,datAM$wyear), FUN="max")
Airmin<-aggregate(datAM$A, by=list(datAM$siteid,datAM$wyear), FUN="min")
colnames(Airmin)<-c("siteid","wyear","Amin")
colnames(Airmax)<-c("siteid","wyear","Amax")
Airmax$Amp<-Airmax$Amax-Airmin$Amin
Airave<-aggregate(datAM$A, by=list(datAM$siteid,datAM$wyear), FUN="mean")
colnames(Airave)<-c("siteid","wyear","Aave")
#now join into the Tsub

Tsub5<-join(Tsub4, Airmax, by=c("siteid","wyear"), type="left")
Tsub6<-join(Tsub5, Airmin, by=c("siteid","wyear"), type="left")
Tsub7<-join(Tsub6, Airave, by=c("siteid","wyear"), type="left")
#Fusb

Fsub5<-join(Fsub4, Airmax, by=c("siteid","wyear"), type="left")
Fsub6<-join(Fsub5, Airmin, by=c("siteid","wyear"), type="left")
Fsub7<-join(Fsub6, Airave, by=c("siteid","wyear"), type="left")



		
#now just need to aggregate the vege classid temperatures
#no difference between organic and mineral soils

ThawMaxAve<-aggregate(Tsub7$Amax, by=list(Tsub7$classID), FUN="mean")
FreezeMinAve<-aggregate(Fsub7$Amin, by=list(Fsub7$classID), FUN="mean")
colnames(ThawMaxAve)<-c("classID", "AveMax")
colnames(FreezeMinAve)<-c("classID", "AveMin")

#join for output
ExtremeAVE<-join(ThawMaxAve, FreezeMinAve, by="classID", type="inner")

#get average
	
######################################################################
######################################################################
#####set model data lists ############################################
######################################################################


datalist<-list(NobsF=dim(Fsub7)[1],
				NobsT=dim(Tsub7)[1],
				nF=Fsub7$NT,
				nT=Tsub7$NT,
				vegeID.T=Tsub7$classID,
				vegeID.F=Fsub7$classID,
				depth.T=Tsub7$depth,
				depth.F=Fsub7$depth,
				Tmax=Tsub7$Amax,Tmin=Fsub7$Amin, TminAVE=FreezeMinAve$AveMin,
				TmaxAVE=ThawMaxAve$AveMax,
				 Nvege=dim(ThawMaxAve)[1])
				
samplelist<-c("betaT1", "betaT2", "betaT3",
					"betaF1", "betaF2", "betaF3", 
					"nF.rep","nT.rep", 
					"sig.nT", "sig.nF")
					

n.modelInit<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\n_model\\n_model_code.r",
						data=datalist, n.adapt=2000, n.chains=3)
				
n.iterI=60000
n.thinI=20

codaobj.init=coda.samples(n.modelInit,variable.names=samplelist,n.iter=n.iterI,thin=n.thinI)				

ModSumm<-summary(codaobj.init)					
					
write.table(ModSumm$statistics, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\model_variaion_stats.csv",
			sep=",",row.names=TRUE)
write.table(ModSumm$quantiles, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\model_variaion_quant.csv",
			sep=",",row.names=TRUE)			

mcmcplot(codaobj.init, dir="c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\historyPlots")	

#write files for output
write.table(Tsub7, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\Thawing_n_forMod.csv", sep=",", row.names=FALSE)	
write.table(Fsub7, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\Freezing_n_forMod.csv", sep=",", row.names=FALSE)
write.table(ExtremeAVE, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n4\\AVET_forMod.csv", sep=",", row.names=FALSE)	