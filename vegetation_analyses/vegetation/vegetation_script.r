#######################################################
########this script reads in output from model 10 #####
########and impliments an empirical model         #####
########that looks at variation across vegetation #####
#######################################################
library(plyr)
library(rjags)
library(coda)
library(xtable)
library(mcmcplots)

setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out")
#read in data
datN<-read.csv("nfactor.csv")
datS<-read.csv("soilParm.csv")
datA<-read.csv("airParm.csv")

#now read in some site data
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\region.csv")
datVC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\vegeClass.csv")

colnames(datR)[1]<-"siteid"
colnames(datVC)[1]<-"siteid"

datI<-join(datR, datVC, by="siteid", type="left")

#match these data by siteid
datN1<-join(datN, datI, by="siteid", type="left")
datS1<-join(datS, datI, by="siteid", type="left")


#now match air temp min and max to N factor

datNF<-datN1[datN1$parm=="Fn",]
datNT<-datN1[datN1$parm=="Tn",]
datTmax<-datS1[datS1$parm=="TmaxS",]
datTmin<-datS1[datS1$parm=="TminS",]
datDZ<-datS1[datS1$parm=="DayZero",]
datPS<-datS1[datS1$parm=="peakSS",]
datPW<-datS1[datS1$parm=="peakWS",]


#now seperate out air to match
datTmaxA<-datA[datA$parm=="TmaxA",]
colnames(datTmaxA)[1:4]<-paste0(colnames(datTmaxA)[1:4],"A")
datTminA<-datA[datA$parm=="TminA",]
colnames(datTminA)[1:4]<-paste0(colnames(datTminA)[1:4],"A")
datPSA<-datA[datA$parm=="peakSA",]
colnames(datPSA)[1:4]<-paste0(colnames(datPSA)[1:4],"A")
datPWA<-datA[datA$parm=="peakWA",]
colnames(datPWA)[1:4]<-paste0(colnames(datPWA)[1:4],"A")


#now combine air measure with matching parm
datNF<-join(datNF, datTminA, by=c("siteid","height","wyear"), type="left")
datNT<-join(datNT, datTmaxA, by=c("siteid","height","wyear"), type="left")

datTmax<-join(datTmax, datTmaxA, by=c("siteid","wyear"), type="left")
datTmin<-join(datTmin, datTminA, by=c("siteid","wyear"), type="left")

datPS<-join(datPS,datPSA,by=c("siteid","wyear"), type="left")
datPW<-join(datPW,datPWA,by=c("siteid","wyear"), type="left")

datDZ<-join(datDZ,datTminA, by=c("siteid","wyear"), type="left")


##############################################
#########vegetation data ####################
##############################################
#siteinfo
datI<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\siteinfo.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))

#species cover
datC<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\spcov.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))
#species biomass
datB<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\spec_bio.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
#read in soil organic layer data
datS<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				

#make some data frames with only specific info

Biome<-data.frame(biome=datI$vege_z, siteid=datI$site_id)

#distrubance
Dist<-data.frame(siteid=datI$site_id, Dtype=datI$dist_type, Dtime=datI$dist_hist )
Dist<-Dist[!is.na(Dist$Dtype)|Dist$Dtype!="undisturbed",]
				
#################################################
######### Aggregate         data ################
#################################################

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

##############################################################
############## read in region data ###########################
##############################################################
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\region.csv")
colnames(datR)[1]<-"siteid"

datVC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\vegeClass.csv")

colnames(datVC)[1]<-"siteid"

className<-c("herb barren",	"gramminoid tundra", "tussock tundra",	
			"shrub tundra",	"gramminoid wetland", "needleleaf deciduous",
				"needleleaf evergreen", "mixed conifer decidous")


Coverall<-list(Shrub.c,Moss.c, Gram.c, Ground.c, datR, Biome, datVC)
CoverC<-join_all(Coverall,by="siteid", type="left")

datNF<-join(datNF, CoverC, by="siteid", type="left")
datNT<-join(datNT, CoverC, by="siteid", type="left")
datTmax<-join(datTmax, CoverC, by="siteid", type="left")
datTmin<-join(datTmin, CoverC, by="siteid", type="left")
datDZ<-join(datDZ, CoverC, by="siteid", type="left")


###############################################################
###############################################################
#start by looking at patterns across:
#plant cover, 
#compare ice wedge vs non ice wedge in the vegetation type?
#look at biome vs plant cover


#start by looking at shrub cover, moss cover by region w/ depth
#need to subset to exclude NA 

NFV<-datNF[!is.na(datNF$moss.pc)&!is.na(datNF$shrub.pc),]
NTV<-datNT[!is.na(datNT$moss.pc)&!is.na(datNT$shrub.pc),]
TmaxV<-datTmax[!is.na(datTmax$moss.pc)&!is.na(datTmax$shrub.pc),]
TminV<-datTmin[!is.na(datTmin$moss.pc)&!is.na(datTmin$shrub.pc),]
DZV<-datDZ[!is.na(datDZ$moss.pc)&!is.na(datDZ$shrub.pc),]

##################################################
#################for plotting need to get unique region 

#only 1 observation from E russia so exclude region 3
regID<-unique(data.frame(region=DZV$region, region.name=DZV$region_name ))
regID<-regID[order(regID$region),]
regID<-regID[regID$region!=3,]

regID$regUID<-seq(1,dim(regID)[1])
regID$nameF<-c("Alaska","Canada", "Islands")

#get unique vegetation class

vegID<-data.frame(vegeID=unique(NFV$class))
vegID$name<-className[vegID$vegeID]
vegID<-vegID[order(vegID$vegeID),]

#get unique biome table

biomeID<-data.frame(biome=unique(NFV$biome))
biomeID$bioID<-seq(1,dim(biomeID)[1])
#turn variables into a list
varAll<-list(NFV,NTV,TmaxV,TminV,DZV)

#see how many observations by biome x region
bioReg<-aggregate(NFV$Mean, by=list(NFV$biome,NFV$region), FUN="length")
colnames(bioReg)<-c("biome","region","count")
#do not include E Russia data because only one point in region x bio
bioReg<-bioReg[bioReg$count>1,]
bioReg$nameF<-c("Alaska","Alaska","Canada", "Islands")



#####################################################################
#####################################################################
############prepare model runs ######################################
#####################################################################
roundI<-c(0,0,0,0,0)
TexM<-numeric(0)
for(i in 1:5){
	TexM[i]<-round(mean(varAll[[i]]$MeanA),roundI[i])

}
data.name<-c("nfreeze","nthaw","Tmax","Tmin","DayZero")

bioregID<-data.frame(bioReg[,1:2], nameF=bioReg$nameF, bioregID=seq(1,dim(bioReg)[1]))

#need to combine an id for biome and for vege/region id
for(i in 1:5){
	varAll[[i]]<-join(varAll[[i]], biomeID, by="biome", type="left")
	varAll[[i]]<-join(varAll[[i]],bioregID, by=c("biome", "region"), type="left")
	varAll[[i]]<-varAll[[i]][varAll[[i]]$region!=3,]

}


depthseq<-seq(0,20,length.out=100)


airseq<-list(seq(-40,-10,length.out=100),
			seq(0,25,length.out=100),
			seq(0,25,length.out=100),
			seq(-40,-10,length.out=100),
			seq(.6,1,length.out=100),
			seq(.1,.6,length.out=100),
			seq(-40,-10,length.out=100))
			
pcseq<-seq(0,80, length.out=100)



####################################################################
####### get model runs ready. Run a model for each dataset  ########
####################################################################

##############Start by running the model for biome only

samplelist<-c("b1","b2","b3","b4","b5","sigM","sigV","rep.Xobs","mudepth","muair","mushrub","mumoss")

for(i in 1:5){
	datalist<-list(Nobs=dim(varAll[[i]])[1], Xobs=varAll[[i]]$Mean,
					vegeC=varAll[[i]]$bioID, depth=varAll[[i]]$depth,
					airM=varAll[[i]]$MeanA,airM.bar=TexM[i],shrubC=varAll[[i]]$shrub.pc,
					mossC=varAll[[i]]$moss.pc,meas.sig=varAll[[i]]$SD, Nvege=2,
					depthseq=depthseq,
					Ndepth=length(depthseq), airseq=airseq[[i]],
					Nair=length(airseq[[i]]),pcseq=pcseq,Npc=length(pcseq))
	
	
	X.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\vegetation_analyses\\vegetation\\vegetation_model_code.r",
						data=datalist,
						n.adapt=5000,
						n.chains=3)				
					
	print(paste("initialize data ",i ))		
	#specify sample run				
	n.iter.i=40000
	n.thin=20
	codaobj.init = coda.samples(X.modI,variable.names=samplelist,
                       n.iter=n.iter.i, thin=n.thin)
					   
					   
	print(paste("samples done data= ", i))

	#pull out model stats
	Mod.out<-summary(codaobj.init)

	write.table(Mod.out$statistics, 
			paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biome\\mod4\\",data.name[i],"Temp_mod_stats.csv"),
			sep=",",row.names=TRUE)
	write.table(Mod.out$quantiles, paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biome\\mod4\\",data.name[i],"Temp_mod_quant.csv"),
			sep=",",row.names=TRUE)
			
print(paste("summary out data ",i)	)


#run mcmc plots on key params
dir.create(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biome\\mod4\\",data.name[i]))	

mcmcplot(codaobj.init, parms=c("b1","b2","b3","b4","b5","sigV"),
			dir=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biome\\mod4\\",data.name[i]))		
#get summary and save to file

print(paste("mcmcplot out data ", i))	


}


##############Now run biome x region

samplelist<-c("b1","b2","b3","b4","b5","sigM","sigV","rep.Xobs","mudepth","muair","mushrub","mumoss")

for(i in 1:5){
	datalist<-list(Nobs=dim(varAll[[i]])[1], Xobs=varAll[[i]]$Mean,
					vegeC=varAll[[i]]$bioregID, depth=varAll[[i]]$depth,
					airM=varAll[[i]]$MeanA,airM.bar=TexM[i],shrubC=varAll[[i]]$shrub.pc,
					mossC=varAll[[i]]$moss.pc,meas.sig=varAll[[i]]$SD, Nvege=4,
					depthseq=depthseq,
					Ndepth=length(depthseq), airseq=airseq[[i]],
					Nair=length(airseq[[i]]),pcseq=pcseq,Npc=length(pcseq))
	
	
	X.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\vegetation_analyses\\vegetation\\vegetation_model_code.r",
						data=datalist,
						n.adapt=5000,
						n.chains=3)				
					
	print(paste("initialize data ",i ))		
	#specify sample run				
	n.iter.i=40000
	n.thin=20
	codaobj.init = coda.samples(X.modI,variable.names=samplelist,
                       n.iter=n.iter.i, thin=n.thin)
					   
					   
	print(paste("samples done data= ", i))

	#pull out model stats
	Mod.out<-summary(codaobj.init)

	write.table(Mod.out$statistics, 
			paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biomeReg\\mod4\\",data.name[i],"Temp_mod_stats.csv"),
			sep=",",row.names=TRUE)
	write.table(Mod.out$quantiles, paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biomeReg\\mod4\\",data.name[i],"Temp_mod_quant.csv"),
			sep=",",row.names=TRUE)
			
print(paste("summary out data ",i)	)


#run mcmc plots on key params
dir.create(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biomeReg\\mod4\\",data.name[i]))	

mcmcplot(codaobj.init, parms=c("b1","b2","b3","b4","b5","sigV"),
			dir=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biomeReg\\mod4\\",data.name[i]))		
#get summary and save to file

print(paste("mcmcplot out data ", i))	


}