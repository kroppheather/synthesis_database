##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
##########################################################
##########################################################
##########################################################
### This script uses vegetation classes updated by     ###
### coauthors to look at patterns in                   ###
### air and shallow soil temperature coupling          ###
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
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\thaw\\plots\\model"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\thaw\\model\\run3"
Nrun <-2


#join vegeclass to data
ThawParm2 <- join(ThawParm,datV, by="siteid",type="left")
ThawParm2 <- ThawParm2[-which(ThawParm2$wyear == 1996 & ThawParm2$siteid == 13 & ThawParm2$depth ==2.5),]
vegeID <- data.frame(vegeclass=unique(ThawParm2$vegeclass))
vegeIDc <- join(vegeID,datVI,by="vegeclass",type="left")
vegeIDc <- vegeIDc[order(vegeIDc$vegeclass),]
#######################################
#####prepare model run            ##### 
#######################################
datalist <- list(Nobs=dim(ThawParm2)[1],
				SoilP = ThawParm2$Mean,
				Vege=ThawParm2$vegeclass,
				depth=ThawParm2$depth,
				sigMod=ThawParm2$SD,
				NVege=dim(vegeIDc)[1])
				
#paramters of interest
parms <- c("beta0","beta1","sigSoilV","repSoilP",
		"mu.beta0","mu.beta1",
		"sig.beta0","sig.beta1","n.20")	
		
Xcomp <- round(0.05/((9*2)-1),3)


#start model 
vege.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\thaw\\thaw_length_model.r",
						data=datalist,
						n.adapt=10000,
						n.chains=3)

vege.sample <- coda.samples(vege.modI,variable.names=parms,
                       n.iter=60000, thin=30)	
					
#model history
mcmcplot(vege.sample, parms=c("beta0","beta1","sigSoilV",
		"mu.beta0","mu.beta1",
		"sig.beta0","sig.beta1"),
			dir=paste0(modDI,"\\history"))


			
#model output							   
mod.out <- summary(vege.sample,  quantiles = c(Xcomp,0.025, 0.25, 0.5, 0.75, 0.975,1-Xcomp))

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