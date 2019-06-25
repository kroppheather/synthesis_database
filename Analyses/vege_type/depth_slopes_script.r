##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
##########################################################
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in                   ###
### air and shallow soil temperature coupling          ###
### but also needs to account for depth                ###
### focus only on key soil variables: min, max, and    ###
### the timing of the minimum                          ###
##########################################################
##########################################################
### Inputs: model  output using temp_parm_extract.r    ### 
### dataframes are Nfactor, SoilParm, AirParm          ###
### AirRepID,SoilRepID, datCSM(list), datCAM (list)    ###
### datAM: air temperature, datSM soil temperature     ###
### datAT: site depth id, datAT: site height ID        ###
### datNI: n factor ids, datAI: site year height ID    ###
### datSI: site depth year ID                          ###
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

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\plots"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model\\run3"
Nrun <- 3
#indicate if a model run is occuring
modRun <- 1


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

#pull out relevant parameters
#for analysis: TminS, TmaxS, peakWS
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "peakWS") 


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmVs)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmVs[i]&SoilParm$depth<=20,]
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
}

plot(SoilL[[1]]$depth,SoilL[[1]]$Mean)

#need to see how many depths measured for each site

DepthID <- unique(data.frame(depth=SoilL[[1]]$depth, siteid=SoilL[[1]]$siteid))

#get the count of depths in a site
DepthCount <- aggregate(DepthID$depth,by=list(DepthID$siteid), FUN="length")
colnames(DepthCount) <- c("siteid", "DepthCount")

#look at sites with at least 3 depths measured
DepthCountM <- DepthCount[DepthCount$DepthCount>=3,]



#start by looking at the slopes for sites with 3 or more depths for each year and site

SoilLM <- list()

for(i in 1:length(parmVs)){
	SoilLM[[i]] <- join(SoilL[[i]], DepthCountM, by="siteid", type="inner")
	SoilLM[[i]]$parmID <- rep(i,dim(SoilLM[[i]])[1])
}

#see how many sites for each vegetation class are represented
VCid <- unique(data.frame(siteid=SoilLM[[1]]$siteid,vegeclass=SoilLM[[1]]$vegeclass))

VegeCount <- aggregate(VCid$vegeclass, by=list(VCid$vegeclass), FUN="length")
colnames(VegeCount) <- c("vegeclass","vegeCount")

#turn soilparms into a dataframe

Msoil <- ldply(SoilLM,data.frame)

#get unique id for year 
SYPid <- unique(data.frame(siteid=Msoil$siteid,wyear=Msoil$wyear,parmID=Msoil$parmID,vegeclass=Msoil$vegeclass))
SYPid$SYP <- seq(1,dim(SYPid)[1])

#join id back into soil dataframe
Msoil2 <- join(Msoil,SYPid, by=c("siteid","wyear","parmID","vegeclass"), type="left")


#look at vegeParm in SYP
vegeParmDF <- unique(data.frame(vegeclass=SYPid$vegeclass,parmID=SYPid$parmID))
vegeParmDF$vegeParmid <- seq(1, dim(vegeParmDF)[1])

#join back into SYPid
SYPid <- join(SYPid,vegeParmDF, by=c("vegeclass","parmID"), type="left")

#######################################
#####prepare model run            ##### 
#######################################
#data
datalist <- list(Nobs=dim(Msoil2)[1],
				tempParm=Msoil2$Mean,
				parmID=Msoil2$parmID, 
				SYP=Msoil2$SYP,
				depth=Msoil2$depth,
				sig.mod=Msoil2$SD,
				NSYP=dim(SYPid)[1],
				vegeParm=SYPid$vegeParmid,
				NvegeParm=dim(vegeParmDF)[1],
				Parm=vegeParmDF$parmID,
				Nparm=length(parmVs))
				
#paramters of interest
parms <- c("beta0","beta1","sigP", "mu.beta0","sig.beta0","mu.beta0C","sig.beta0C","mu.beta1","sig.beta1","mu.beta1C","sig.beta1C")			

if(modRun==1){
#start model 
vege.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\vege_type\\depth_slopes_model_code.r",
						data=datalist,
						n.adapt=50000,
						n.chains=3)

vege.sample <- coda.samples(vege.modI,variable.names=parms,
                       n.iter=120000, thin=40)	
					
#model history
mcmcplot(vege.sample, parms=c("beta0","beta1","sigP", "mu.beta0","sig.beta0","mu.beta0C","sig.beta0C","mu.beta1","sig.beta1","mu.beta1C","sig.beta1C"),
			dir=paste0(modDI,"\\history"))


			
#model output							   
mod.out <- summary(vege.sample,  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

write.table(mod.out$statistics,paste0(modDI,"\\vege_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(mod.out$quantiles,paste0(modDI,"\\vege_mod_quant.csv"),
			sep=",",row.names=TRUE)

#coda output
chain1<-as.matrix(vege.sample [[1]])
write.table(chain1,paste0(modDI,"\\chain1_coda.csv"), sep=",")
chain2<-as.matrix(vege.sample [[2]])
write.table(chain2,paste0(modDI,"\\chain2_coda.csv"), sep=",")
chain3<-as.matrix(vege.sample [[3]])
write.table(chain3,paste0(modDI,"\\chain3_coda.csv"), sep=",")		


}	



#######################################
#####look at data                 ##### 
#######################################


#read in model results 

datM <- read.csv(paste0(modDI,"\\vege_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\vege_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))


#look at slopes
slope <- datC[datC$parms=="mu.beta1",]
slope <- cbind(slope,vegeParmDF)



#slope site
slopeS <- datC[datC$parms=="beta1",]
slopeS <- cbind(slopeS,SYPid)


#join unique vegeinfo
vegMI <- data.frame(vegeclass=unique(slopeS$vegeclass))
vegMI <- join(vegMI, datVI, by="vegeclass",type="left")
#now make 3 plots of the slopes
parmName <- c("Minimum soil temperature (C)", "Maximum soil temperature (C)", "Timing of Minimum")
hd <- 30
wd <- 30

yl <- c(-.4,-.8,-.007)
yh <- c(2,.2,.011)
yls <- c(-.5,-.8,-.01)
yhs <- c(2,.2,.01)
yis <- c(.5,.2,.005)

for(i in 1:length(parmVs)){
	jpeg(paste0(plotDI,"\\model\\run",Nrun,"\\slopes_comp",parmVs[i],".jpeg"), width=8000, height=2000, units="px",quality=100)
		layout(matrix(seq(1,dim(vegMI)[1]), ncol=dim(vegMI)[1]), width=rep(lcm(wd),dim(vegMI)[1]), height=rep(lcm(hd),dim(vegMI)[1]))
		
	for(j in 1:dim(vegMI)[1]){
	par(mai=c(0,0,0,0))
	plot(c(0,1),
			c(0,1), type="n",
			xlab=" ", ylab=" ", ylim=c(yl[i],yh[i]),xlim=c(0,length(slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]])+1),
			axes=FALSE, xaxs="i",yaxs="i")
	abline(h=0, lwd=10, lty=3, col="royalblue3")
	abline(h=slope$Mean[slope$parmID==i&slope$vegeclass==vegMI$vegeclass[j]], lwd=8, col="tomato3")
	polygon(c(seq(0,length(slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]])+1, length.out=100),
				rev(seq(0,length(slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]])+1, length.out=100))),
			c(rep(slope$X2.5.[slope$parmID==i&slope$vegeclass==vegMI$vegeclass[j]],100),
				rep(slope$X97.5.[slope$parmID==i&slope$vegeclass==vegMI$vegeclass[j]],100)),
				col=rgb(205/255,79/255,57/255,.5), border=NA)
	
	points(seq(1,length(slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]])),
			slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]], pch=19, cex=5)
	
	arrows(	seq(1,length(slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]])),
		slopeS$X2.5.[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]],
		seq(1,length(slopeS$Mean[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]])),
		slopeS$X97.5.[slopeS$parmID==i&slopeS$vegeclass==vegMI$vegeclass[j]], code=0, lwd=4)
	box(which="plot")
	mtext(paste(vegMI$vegename[vegMI$vegeclass==vegMI$vegeclass[j]]), side=1, line=10, cex=5)
	if(j==1){
		axis(2, seq(yls[i],yhs[i], by=yis[i]), rep(" ", length(seq(yls[i],yhs[i], by=yis[i]))),lwd.ticks=5)
		mtext(seq(yls[i],yhs[i], by=yis[i]),at=seq(yls[i],yhs[i], by=yis[i]), las=2, cex=5, line=5, side=2)
		mtext(paste(parmName[i]), side=2,line=20,cex=7)
		}
	}
	dev.off()
}
