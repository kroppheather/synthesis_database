##########################################################
########Vegetation classification analysis     ###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in soil              ###
###  to analyze patterns in                            ###
### air and shallow soil temperature coupling          ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
##########################################################
##########################################################
#libraries loaded in source script: plyr

library(rjags)
library(coda)
library(xtable)
library(mcmcplots)


##########################################
##first grab soil temperature parameters##
##########################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#read in vegetation class data
#note sites 199-222 not confirmed yet

datVC <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\vege_class\\new_class.csv")

#make sub table
newC <- data.frame(siteid=datVC$siteid, Vclass=datVC$new.class)

#join vegetation class to each parameter table
NfactorV <- join(Nfactor, newC, by="siteid", type="left")
SoilParmV <- join(SoilParm, newC, by="siteid", type="left")

#check to see how many observations in each vege class
ParmsCount <- aggregate(SoilParmV$Mean[SoilParmV$parm=="TmaxS"],
						by=list(SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]), FUN="length")
#see how many sites are unique
SiteCountL <-unique(data.frame(siteid=SoilParmV$siteid[SoilParmV$parm=="TmaxS"],
								Vclass=SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]))
#count how many sites in each class
SiteCount <- aggregate(SiteCountL$siteid, by=list(SiteCountL$Vclass), FUN="length")

#make seperate data frames fore each soil parameter

datNF<-NfactorV [NfactorV $parm=="Fn",]
datNT<-NfactorV [NfactorV $parm=="Tn",]
datTmax<-SoilParmV[SoilParmV$parm=="TmaxS",]
datTmin<-SoilParmV[SoilParmV$parm=="TminS",]
datDZ<-SoilParmV[SoilParmV$parm=="DayZero",]
datPS<-SoilParmV[SoilParmV$parm=="peakSS",]
datPW<-SoilParmV[SoilParmV$parm=="peakWS",]

#now seperate out air to match
datTmaxA<-AirParm[AirParm$parm=="TmaxA",]
colnames(datTmaxA)[1:4]<-paste0(colnames(datTmaxA)[1:4],"A")
datTminA<-AirParm[AirParm$parm=="TminA",]
colnames(datTminA)[1:4]<-paste0(colnames(datTminA)[1:4],"A")
datPSA<-AirParm[AirParm$parm=="peakSA",]
colnames(datPSA)[1:4]<-paste0(colnames(datPSA)[1:4],"A")
datPWA<-AirParm[AirParm$parm=="peakWA",]
colnames(datPWA)[1:4]<-paste0(colnames(datPWA)[1:4],"A")

#now combine air measure with matching parm
datNF<-join(datNF, datTminA, by=c("siteid","height","wyear"), type="left")
datNT<-join(datNT, datTmaxA, by=c("siteid","height","wyear"), type="left")

datTmax<-join(datTmax, datTmaxA, by=c("siteid","wyear"), type="left")
datTmin<-join(datTmin, datTminA, by=c("siteid","wyear"), type="left")

datPS<-join(datPS,datPSA,by=c("siteid","wyear"), type="left")
datPW<-join(datPW,datPWA,by=c("siteid","wyear"), type="left")

datDZ<-join(datDZ,datTminA, by=c("siteid","wyear"), type="left")

datAll<-list(datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ)

#get the average Air value for covariate centering
roundI<-c(0,0,0,0,2,2,0)
TexM<-numeric(0)
for(i in 1:7){
	TexM[i]<-round(mean(datAll[[i]]$MeanA),roundI[i])

}

data.name<-c("nfreeze","nthaw","Tmax","Tmin","Peakmax","Peakmin","DayZero")

#sequences for plotting mu

depthseq<-seq(0,20,length.out=100)


airseq<-list(seq(-40,-10,length.out=100),
			seq(0,25,length.out=100),
			seq(0,25,length.out=100),
			seq(-40,-10,length.out=100),
			seq(.6,1,length.out=100),
			seq(.1,.6,length.out=100),
			seq(-40,-10,length.out=100))
			
			
####################################################################
####### get model runs ready. Run a model for each dataset  ########
####################################################################
samplelist<-c("b1","b2","b3","sigM","sigV","rep.Xobs","mudepth","muair")

for(i in 1:7){
	datalist<-list(Nobs=dim(datAll[[i]])[1], Xobs=datAll[[i]]$Mean,
					vegeC=datAll[[i]]$Vclass, depth=datAll[[i]]$depth,
					airM=datAll[[i]]$MeanA,airM.bar=TexM[i],
					meas.sig=datAll[[i]]$SD, Nvege=9, depthseq=depthseq,
					Ndepth=length(depthseq), airseq=airseq[[i]],
					Nair=length(airseq[[i]]))
	
	
	X.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\vege_class\\vegetation_class_model_code.r",
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
			paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\",data.name[i],"Temp_mod_stats.csv"),
			sep=",",row.names=TRUE)
	write.table(Mod.out$quantiles, paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\",data.name[i],"Temp_mod_quant.csv"),
			sep=",",row.names=TRUE)
			
print(paste("summary out data ",i)	)


#run mcmc plots on key params
dir.create(paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\",data.name[i]))	

mcmcplot(codaobj.init, parms=c("b1","b2","b3","sigV"),
			dir=paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\",data.name[i]))		
#get summary and save to file

print(paste("mcmcplot out data ", i))			


		}