##########################################################
########Soil patterns                          ###########
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in soil              ###
###  to analyze patterns in                            ###
### air and shallow soil temperature coupling          ###
##########################################################
##########################################################



#######################################
#####read in data                 ##### 
#######################################

                    
#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")

#######################################
#####libraries                    ##### 
#######################################
library(rjags)
library(coda)
library(mcmcplots)
library(plyr)

#######################################
#####set directories              ##### 
#######################################

#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\pattern\\model\\run1"
Nrun <-1



#######################################
#####organize data                ##### 
#######################################


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)
AirParm <- join(AirParm, datV, by=c("siteid"), type="left")

#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$parm)


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmV)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmV[i],]
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
}
#pull out only revelevant comparisons that are meaningful

#Tmin vs Tave
#Tmax vs Tave
#set up comparisions
#x values
xcomp <- c(4,5)
ycomp <- c(6,6)
compNameX <- parmV[xcomp]
compNameY <- parmV[ycomp]
xcent <- SoilMs[xcomp]


SoilCompDF <- data.frame(xobs = c(SoilL[[xcomp[1]]]$Mean,SoilL[[xcomp[2]]]$Mean),
						yobs= c(SoilL[[ycomp[1]]]$Mean,SoilL[[ycomp[2]]]$Mean),
						xSD=c(SoilL[[xcomp[1]]]$SD,SoilL[[xcomp[2]]]$SD),
						ySD=c(SoilL[[ycomp[1]]]$SD,SoilL[[ycomp[2]]]$SD),
						vegeClass = c(SoilL[[xcomp[1]]]$vegeclass,SoilL[[xcomp[2]]]$vegeclass),			
						comp= rep(seq(1,length(xcomp)), each=dim(SoilL[[xcomp[1]]])[1]))

#data frame of vege class and comparision ids

vegeComp <- unique(data.frame(vegeClass=SoilCompDF$vegeClass,comp=SoilCompDF$comp))						
vegeComp <- vegeComp[order(vegeComp$comp,vegeComp$vegeClass),]						
vegeComp$vegeCompID <- seq(1,dim(vegeComp)[1])	

#join back into soildf
SoilCompDF2 <- join(SoilCompDF,vegeComp, by=c("vegeClass","comp"),type="left")


#make plotting data

xcompDF <- data.frame(xcomp=xcomp,ycomp=ycomp,compNameY=compNameY,compNameX=compNameX,xcent=xcent,
				xmin=c(round_any(min(SoilL[[xcomp[1]]]$pc2.5),5,floor),round_any(min(SoilL[[xcomp[2]]]$pc2.5),5,floor)),
				xmax=c(round_any(max(SoilL[[xcomp[1]]]$pc97.5),5,ceiling),round_any(max(SoilL[[xcomp[2]]]$pc97.5),5,ceiling)),
				ymin=c(round_any(min(SoilL[[ycomp[1]]]$pc2.5),5,floor),round_any(min(SoilL[[ycomp[2]]]$pc2.5),5,floor)),
				ymax=c(round_any(max(SoilL[[ycomp[1]]]$pc97.5),5,ceiling),round_any(max(SoilL[[ycomp[2]]]$pc97.5),5,ceiling)))




xplot <- matrix(rep(NA,100*dim(vegeComp)[1]),ncol=dim(vegeComp)[1])

	for(j in 1:dim(vegeComp)[1]){
		xplot[,j] <- seq(xcompDF$xmin[vegeComp$comp[j]],xcompDF$xmax[vegeComp$comp[j]],length.out=100)
	}
xplotDF <- data.frame(xplot=as.vector(xplot),vegeCompID=rep(seq(1,dim(vegeComp)[1]),each=100), comp=rep(vegeComp$comp,each=100),vegeClass=rep(vegeComp$vegeClass,each=100))




hhxplot <- matrix(rep(NA,100*dim(xcompDF)[1]), ncol=dim(xcompDF)[1])

	for(j in 1:dim(xcompDF)[1]){
		hhxplot[,j] <- seq(xcompDF$xmin[j],xcompDF$xmax[j],length.out=100)
	}


hhxplotDF <- data.frame(hhxplot=as.vector(hhxplot), comp=rep(seq(1,dim(xcompDF)[1]),each=100))


#multiple comparision quantile
mcQ <- round_any(0.05/(dim(vegeComp)[1]-1)	,0.001)
#######################################
#####set up model run             ##### 
#######################################
#data frame of all observations
datalist <- list(Nobs=dim(SoilCompDF2)[1],
				yvar=SoilCompDF2$yobs,
				sig.mod=SoilCompDF2$ySD,
				xvar=SoilCompDF2$xobs,
				sig.xvar=SoilCompDF2$xSD,
				compVege=SoilCompDF2$vegeCompID,
				compX=SoilCompDF2$comp,
				xvarCenter=xcent,
				NcompVege=dim(vegeComp)[1],
				comp=vegeComp$comp,
				Ncomp=length(xcomp),
				Nplot=dim(xplotDF)[1],
				xplot=xplotDF$xplot,
				compVegeP=xplotDF$vegeCompID,
				compP=xplotDF$comp,
				comphh=hhxplotDF$comp,
				xplothh=hhxplotDF$hhxplot,
				Nhhplot=dim(hhxplotDF)[1],
				compAdd=c(0,9,18,27,36),NVege=9)
				
				
				
parms <- c("rep.yvar","sig.compVege","beta0","beta1","mu.beta0","mu.beta1","sig.beta0","sig.beta1", "mu.plot","mu.hhplot", "slopeDiff")

comp.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\vege_type\\vege_type_pattern_model_code.r",
						data=datalist,
						n.adapt=50000,
						n.chains=3)

comp.sample <- coda.samples(comp.modI,variable.names=parms,
                       n.iter=150000, thin=50)	
					   
mcmcplot(comp.sample, parms=c("sig.compVege","beta0","beta1","mu.beta0","mu.beta1","sig.beta0","sig.beta1"),
			dir=paste0(modDI,"\\history"))	
					   
mod.out <- summary(comp.sample,  quantiles = c(mcQ,0.025, 0.25, 0.5, 0.75, 0.975,1-mcQ))

write.table(mod.out$statistics,paste0(modDI,"\\pattern_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(mod.out$quantiles,paste0(modDI,"\\pattern_mod_quant.csv"),
			sep=",",row.names=TRUE)

chain1<-as.matrix(comp.sample [[1]])
write.table(chain1,paste0(modDI,"\\chain1_coda.csv"), sep=",")
chain2<-as.matrix(comp.sample [[2]])
write.table(chain2,paste0(modDI,"\\chain2_coda.csv"), sep=",")
chain3<-as.matrix(comp.sample [[3]])
write.table(chain3,paste0(modDI,"\\chain3_coda.csv"), sep=",")				