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

######################################################################
########subset variables for model run ###############################
######################################################################
#eventually a missing data model will help use more datasets
#but for now look at a model run with only all data present
Tsub<-data.frame(siteid=Tall7$siteid, N.id=Tall7$ID,NT=Tall7$M, wyear=Tall7$wyear,lat=Tall7$lat,
				biome=Tall7$vege_z, EVI=Tall7$EVI,OLT=Tall7$OLT, 
				depth=Tall7$depth)

Tsub<-na.omit(Tsub)

Tsub$biomeID<-ifelse(Tsub$biome=="tundra", 2,1)

Fsub<-data.frame(siteid=Fall7$siteid, N.id=Fall7$ID,NT=Fall7$M, wyear=Fall7$wyear,lat=Fall7$lat,
				biome=Fall7$vege_z, EVI=Fall7$EVI,OLT=Fall7$OLT, 
				depth=Fall7$depth
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

#read in data to look at region
datR<-read.csv("region.csv")
#join to the tables
colnames(datR)[1]<-"siteid"

Tsub2<-join(Tsub,datR, by="siteid", type="left")
Fsub2<-join(Fsub,datR, by="siteid", type="left")

#read in vegetation data to set up vegetation class

datVC<-read.csv("vegeClass.csv")
colnames(datVC)[1]<-"siteid"

Tsub3<-join(Tsub2,datVC, by="siteid", type="left")
Fsub3<-join(Fsub2,datVC, by="siteid", type="left")

#comparision of F and T n factors
FreBH<-data.frame(N=Fsub3$NT[Fsub3$classID==1])
FreBH$ID<-rep("freezing", dim(FreBH)[1])
ThaBH<-data.frame(N=Tsub3$NT[Tsub3$classID==1])
ThaBH$ID<-rep("thawing", dim(ThaBH)[1])

xN<-c(FreBH$ID,ThaBH$ID)
yN<-c(FreBH$N,ThaBH$N)
par(mai=c(1,1,1,1))
plot(as.factor(xN),yN , xlab="Temeprature type", 
	ylab="N factor", col="grey75", ylim=c(.7,1.7), cex.axis=2, cex.lab=2)
axis(4, seq(.8,1.6, by=.2), cex.axis=2)

#now see if there are enough observations to model by these groups

TOL<-aggregate(Tsub3$NT, by=list(Tsub3$orgID), FUN=length)
FOL<-aggregate(Fsub3$NT, by=list(Fsub3$orgID), FUN=length)
plot(Tsub3$depth[Tsub3$orgID==2],Tsub3$NT[Tsub3$orgID==2],pch=19)
plot(Tsub3$depth[Tsub3$orgID==1],Tsub3$NT[Tsub3$orgID==1],pch=19)

plot(Tsub3$EVI[Tsub3$orgID==2],Tsub3$NT[Tsub3$orgID==2],pch=19)
plot(Tsub3$EVI[Tsub3$orgID==1],Tsub3$NT[Tsub3$orgID==1],pch=19)

TV<-aggregate(Tsub3$NT, by=list(Tsub3$class), FUN=length)
FV<-aggregate(Fsub3$NT, by=list(Fsub3$class), FUN=length)

#make a vegetation class ID that reflects the actual number
Tsub3$classID<-ifelse(Tsub3$class>=7, Tsub3$class-1,Tsub3$class)
Fsub3$classID<-ifelse(Fsub3$class>=7, Fsub3$class-1,Fsub3$class)


plot(as.factor(Tsub3$class),Tsub3$NT)
plot(as.factor(Fsub3$class),Fsub3$NT)

TR<-aggregate(Tsub3$NT, by=list(Tsub3$region_name), FUN=length)
FR<-aggregate(Fsub3$NT, by=list(Fsub3$region_name), FUN=length)

plot(as.factor(Tsub3$region_name),Tsub3$NT)
plot(as.factor(Fsub3$region_name),Fsub3$NT)


######################################################################
######################################################################
#####set model data lists ############################################
######################################################################


datalist<-list(NobsF=dim(Fsub3)[1],
				NobsT=dim(Tsub3)[1],
				nF=Fsub3$NT,
				nT=Tsub3$NT,
				orgID.T=Tsub3$orgID,
				orgID.F=Fsub3$orgID,
				Norg=2,
				Nvegeclass=7,
				EVI.T=Tsub3$EVI,
				EVI.F=Fsub3$EVI,
				depth.T=Tsub3$depth,
				depth.F=Fsub3$depth,
				vegeID.F=Fsub3$classID,
				vegeID.T=Tsub3$classID)
				
samplelist<-c("deviance","betaT1star", "betaT2", "betaT3", "betaT4",
					"betaF1star", "betaF2", "betaF3", "betaF4",
					"nF.rep","nT.rep", "epsF.star", "epsT.star",
					"sig.epsT", "sig.epsF", "rho.epsT", "rho.epsF",
					"sig.nT", "sig.nF")
					
Initslist<-list(list(tau.epsF=25, tau.epsT=25),
				list(tau.epsF=50, tau.epsT=50),
				list(tau.epsF=5, tau.epsT=5))

n.modelInit<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\n_model\\n_model_code.r",
						data=datalist, n.adapt=2000, n.chains=3, inits=Initslist)
				
n.iterI=30000
n.thinI=10

codaobj.init=coda.samples(n.modelInit,variable.names=samplelist,n.iter=n.iterI,thin=n.thinI)				

ModSumm<-summary(codaobj.init)					
					
write.table(ModSumm$statistics, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n2\\model_variaion_stats.csv",
			sep=",",row.names=TRUE)
write.table(ModSumm$quantiles, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n2\\model_variaion_quant.csv",
			sep=",",row.names=TRUE)			

mcmcplot(codaobj.init, dir="c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n2\\historyPlots")	

#write files for output
write.table(Tsub, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n2\\Thawing_n_forMod.csv", sep=",", row.names=FALSE)	
write.table(Fsub, "c:\\Users\\hkropp\\Google Drive\\raw_data\\nmod_out\\u7_n2\\Freezing_n_forMod.csv", sep=",", row.names=FALSE)	