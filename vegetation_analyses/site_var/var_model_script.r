#######################################################
########this script reads in output from model 10 #####
########and runs a simple regression              #####
######## to look at variability in decoupling     #####
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


#see how many observations for vegeclass and region there are
#just check one parm to get an idea
#see how many observations for vegeclass and region there are
#just check one parm to get an idea

SIcheck<-aggregate(datS1$Mean[datS1$parm=="TminS"], 
				by=list(datS1$region[datS1$parm=="TminS"],
						datS1$class[datS1$parm=="TminS"]),
				FUN="length")
colnames(SIcheck)<-c("region","class","count")
#now exclude anything with a sample size of less or equal to 3
SIcheckS<-SIcheck[SIcheck$count>3,]
#now add a unique ID
SIcheckS$svID<-seq(1, dim(SIcheckS)[1])

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

datAll<-list(datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ)
#get the average Air value for covariate centering
roundI<-c(0,0,0,0,2,2,0)
TexM<-numeric(0)
for(i in 1:7){
	TexM[i]<-round(mean(datAll[[i]]$MeanA),roundI[i])

}
data.name<-c("nfreeze","nthaw","Tmax","Tmin","Peakmax","Peakmin","DayZero")
####################################################################
####### get model runs ready. Run a model for each dataset  ########
####################################################################
samplelist<-c("b1","b2","b3","sigM","sigV","rep.Xobs")

for(i in 1:7){
	datalist<-list(Nobs=dim(datAll[[i]])[1], Xobs=datAll[[i]]$Mean,
					vegeC=datAll[[i]]$class, depth=datAll[[i]]$depth,
					airM=datAll[[i]]$MeanA,airM.bar=TexM[i],
					meas.sig=datAll[[i]]$SD, Nvege=8)
	
	
	X.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\vegetation_analyses\\site_var\\var_model_code.r",
						data=datalist,
						n.adapt=5000,
						n.chains=3)				
					
	print(paste("initialize data ",i ))		
	#specify sample run				
	n.iter.i=500000
	n.thin=25
	codaobj.init = coda.samples(X.modI,variable.names=samplelist,
                       n.iter=n.iter.i, thin=n.thin)
					   
					   
	print(paste("samples done data= ", i))

	#pull out model stats
	Mod.out<-summary(codaobj.init)

	write.table(Mod.out$statistics, 
			paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\var\\mod1\\",data.name[i],"Temp_mod_stats.csv"),
			sep=",",row.names=TRUE)
	write.table(Mod.out$quantiles, paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\var\\mod1\\",data.name[i],"Temp_mod_quant.csv"),
			sep=",",row.names=TRUE)
			
print(paste("summary out data ",i)	)


#run mcmc plots on key params
dir.create(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\var\\mod1\\",data.name[i]))	

mcmcplot(codaobj.init, parms=c("b1","b2","b3","sigV"),
			dir=paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\var\\mod1\\",data.name[i]))		
#get summary and save to file

print(paste("mcmcplot out data ", i))	


}



