##########################################################
########Soil patterns                          ###########
########Heather Kropp started April 2018       ###########
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

#######################################
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_pattern\\plots"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_pattern\\model\\run2"

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


vegeclassColors <- data.frame(vegeclass=seq(1,9),
					coli=c("grey50", "deepskyblue","darkgoldenrod","darkorchid4","forestgreen","tomato3","palegreen3","goldenrod1","chocolate1"))

SoilParm <- join(SoilParm,vegeclassColors,by="vegeclass",type="left")
					

SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmV)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmV[i],]
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
}
#pull out only revelevant comparisons that are meaningful

#Tmin vs Tave
#Tmin vs Tmax
#pmin vs Tmin
#Tmax vs Tave
#Tmax vs pmax
#pmin vs Tmax
#set up comparisions
#x values
xcomp <- c(4,4,3,5,2)
ycomp <- c(6,5,4,6,5)
compNameX <- parmV[xcomp]
compNameY <- parmV[ycomp]
xcent <- SoilMs[xcomp]
#######################################
#####set up model run             ##### 
#######################################
#data frame of all observations
SoilCompDF <- data.frame(xobs = c(SoilL[[xcomp[1]]]$Mean,SoilL[[xcomp[2]]]$Mean,SoilL[[xcomp[3]]]$Mean,SoilL[[xcomp[4]]]$Mean,SoilL[[xcomp[5]]]$Mean),
						yobs= c(SoilL[[ycomp[1]]]$Mean,SoilL[[ycomp[2]]]$Mean,SoilL[[ycomp[3]]]$Mean,SoilL[[ycomp[4]]]$Mean,SoilL[[ycomp[5]]]$Mean),
						xSD=c(SoilL[[xcomp[1]]]$SD,SoilL[[xcomp[2]]]$SD,SoilL[[xcomp[3]]]$SD,SoilL[[xcomp[4]]]$SD,SoilL[[xcomp[5]]]$SD),
						ySD=c(SoilL[[ycomp[1]]]$SD,SoilL[[ycomp[2]]]$SD,SoilL[[ycomp[3]]]$SD,SoilL[[ycomp[4]]]$SD,SoilL[[ycomp[5]]]$SD),
						vegeClass = c(SoilL[[xcomp[1]]]$vegeclass,SoilL[[xcomp[2]]]$vegeclass,SoilL[[xcomp[3]]]$vegeclass,
										SoilL[[xcomp[4]]]$vegeclass,SoilL[[xcomp[5]]]$vegeclass),			
						comp= rep(seq(1,length(xcomp)), each=dim(SoilL[[xcomp[1]]])[1]))

#data frame of vege class and comparision ids

vegeComp <- unique(data.frame(vegeClass=SoilCompDF$vegeClass,comp=SoilCompDF$comp))						
vegeComp <- vegeComp[order(vegeComp$comp,vegeComp$vegeClass),]						
vegeComp$vegeCompID <- seq(1,dim(vegeComp)[1])	

#join back into soildf
SoilCompDF2 <- join(SoilCompDF,vegeComp, by=c("vegeClass","comp"),type="left")
					
datalist <- list(Nobs=dim(SoilCompDF2)[1],
				yvar=SoilCompDF2$yobs,
				sig.mod=SoilCompDF2$ySD,
				xvar=SoilCompDF2$xobs,
				compVege=SoilCompDF2$vegeCompID,
				compX=SoilCompDF2$comp,
				xvarCenter=xcent,
				NcompVege=dim(vegeComp)[1],
				comp=vegeComp$comp,
				Ncomp=length(xcomp))
				
parms <- c("rep.yvar","sig.compVege","beta0","beta1","mu.beta0","mu.beta1","sig.beta0","sig.beta1")

comp.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\soil_pattern\\soil_patterns_model_code.r",
						data=datalist,
						n.adapt=50000,
						n.chains=3)

comp.sample <- coda.samples(comp.modI,variable.names=parms,
                       n.iter=150000, thin=50)	
					   
mcmcplot(comp.sample, parms=c("sig.compVege","beta0","beta1","mu.beta0","mu.beta1","sig.beta0","sig.beta1"),
			dir=paste0(modDI,"\\history"))	
					   
mod.out <- summary(comp.sample)

write.table(mod.out$statistics,paste0(modDI,"\\comp_mod_stats.csv"),
			sep=",",row.names=TRUE)
write.table(mod.out$quantiles,paste0(modDI,"\\comp_mod_quant.csv"),
			sep=",",row.names=TRUE)

chain1<-as.matrix(comp.sample [[1]])
write.table(chain1,paste0(modDI,"\\chain1_coda.csv"), sep=",")
chain2<-as.matrix(comp.sample [[2]])
write.table(chain2,paste0(modDI,"\\chain2_coda.csv"), sep=",")
chain3<-as.matrix(comp.sample [[3]])
write.table(chain3,paste0(modDI,"\\chain3_coda.csv"), sep=",")


#read in model results 

datM <- read.csv(paste0(modDI,"\\comp_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\comp_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))
#pull out numbers
datSP <- character(0)
for(i in 1:dim(datC)[1]){
	datSP[i] <- gsub("\\D","",strsplit(rownames(datC), "\\[")[[i]][2])
}

datC$parmID <- as.numeric(datSP)

#look at regression coefficients
beta0 <- datC[datC$parms=="beta0",]
beta1 <- datC[datC$parms=="beta1",]

mubeta0 <- datC[datC$parms=="mu.beta0",]
mubeta1 <- datC[datC$parms=="mu.beta1",]

#match up ids
colnames(beta0)[11] <- "vegeCompID"
colnames(beta1)[11] <- "vegeCompID"

beta0 <- join(beta0, vegeComp, by=c("vegeCompID"), type="left")
beta1 <- join(beta1, vegeComp, by=c("vegeCompID"), type="left")

mubeta0$compNameX <- compNameX
mubeta0$compNameY <- compNameY
mubeta1$compNameX <- compNameX
mubeta1$compNameY <- compNameY

yl <- c(-40,-40,-40,-1,-1)
yh <- c(1,1,1,25,25)
xseq <- seq(1,17,by=2)

#make a plot of the parms
for(i in 1:length(compNameX)){
	jpeg(paste0(plotDI,"\\model\\run1\\parms",compNameX[i],"_vs_",compNameY[i],".jpg"), width=2000, height=2000, units="px",quality=100)
		par(mfrow=c(1,2))
			plot(c(0,1),c(0,1), type="n", ylim=c(yl[i],yh[i]), xlim=c(0,20), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
				for(j in 1:9){
					
				arrows(xseq[j], beta0$X2.5.[beta0$comp==i&beta0$vegeClass==j],xseq[j], beta0$X97.5.[beta0$comp==i&beta0$vegeClass==j],
						lwd=2,code=0)
					
					polygon(c(xseq[j]-.5,xseq[j]-.5,xseq[j]+.5,xseq[j]+.5),
							c(beta0$X25.[beta0$comp==i&beta0$vegeClass==j],beta0$X75.[beta0$comp==i&beta0$vegeClass==j],
							beta0$X75.[beta0$comp==i&beta0$vegeClass==j],beta0$X25.[beta0$comp==i&beta0$vegeClass==j]),
							col="cornflowerblue")		
				
				arrows(xseq[j]-.5,beta0$Mean[beta0$comp==i&beta0$vegeClass==j],xseq[j]+.5,beta0$Mean[beta0$comp==i&beta0$vegeClass==j],
						lwd=3,code=0)
				}
		arrows(xseq[j], beta0$X2.5.[beta0$comp==i&beta0$vegeClass==j],xseq[j], beta0$X97.5.[beta0$comp==i&beta0$vegeClass==j],
						lwd=2,code=0)			
		
		arrows(19, mubeta0$X2.5.[i],19,mubeta0$X97.5.[i],
						lwd=2,code=0)
						
		polygon(c(18.5,18.5,19.5,19.5), c(mubeta0$X25.[i],	mubeta0$X75.[i],mubeta0$X75.[i],mubeta0$X25.[i]),
				col="darkorchid4")
		arrows(18.5,mubeta0$Mean[i],19.5,	mubeta0$Mean[i], lwd=3,code=0)
		
	plot(c(0,1),c(0,1), type="n", ylim=c(yl[i],yh[i]), xlim=c(0,20), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	box(which="plot")
	dev.off()
}



#######################################
#####make plot of data            ##### 
#######################################








#make figure for soil comparisons


#set up correlation panel
wd <- 50
hd <- 50
#set up axis limits
aveL <- -15
aveH <- 10
minL <- -35
minH <- 1
maxL <- -1
maxH <- 22
pminL <- .15
pminH <- 0.62
pmaxL <- 0.58
pmaxH <- 1
xlL <- 0
xlH <- 10
ylL <- 0
ylH <- 10
#sizes
px <- 10
lwt <- 10
mx <- 8
lx <- 10
#labels
aveS <- seq(-15,5,by=5)
minS <- seq(-35,0, by=5)
maxS <- seq(0,20,by=5)
pminS <- seq(.25,.55,by=.1)
pmaxS <- seq(0.65,.95,by=.1)

jpeg(paste0(plotDI,"\\soil_comp.jpg"), width=5500, height=5000, units="px",quality=100)
	layout(matrix(seq(1,6),byrow=TRUE,ncol=3), width=rep(lcm(wd),6), height=rep(lcm(hd),6))
	#ave vs min
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(aveL,aveH), ylim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[1]]]$Mean,SoilL[[ycomp[1]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[1]]]$coli),cex=px)
	axis(2,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,las=2,cex=mx,side=2,line=6)
	axis(3,aveS,rep(" ",length(aveS)),lwd.ticks=lwt)
	mtext(aveS,at=aveS,side=3,line=6,cex=mx)
	mtext("Temp Min", side=2, line=30, cex=lx)
	mtext("Temp Ave", side=3, line=25, cex=lx)
	box(which="plot")		
	#min vs max		
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(maxL,maxH), ylim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
	points(SoilL[[xcomp[2]]]$Mean,SoilL[[ycomp[2]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[1]]]$coli),cex=px)		
	axis(3,maxS,rep(" ",length(maxS)),lwd.ticks=lwt)
	mtext(maxS,at=maxS,side=3,line=6,cex=mx)
	mtext("Temp Max", side=3, line=25, cex=lx)	
	box(which="plot")		
	#min vs pmin		
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(pminL,pminH), ylim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
	points(SoilL[[xcomp[3]]]$Mean,SoilL[[ycomp[3]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[1]]]$coli),cex=px)
	axis(3,pminS,rep(" ",length(pminS)),lwd.ticks=lwt)
	mtext(pminS,at=pminS,side=3,line=6,cex=mx)	
	axis(4,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,las=2,cex=mx,side=4,line=6)
	mtext("Min time", side=3, line=25, cex=lx)
	mtext("Temp Min", side=4, line=30, cex=lx)
	box(which="plot")		
	#max vs ave
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(aveL,aveH), ylim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[4]]]$Mean,SoilL[[ycomp[4]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[1]]]$coli),cex=px)
	axis(2,maxS,rep(" ",length(maxS)),lwd.ticks=lwt)
	mtext(maxS,at=maxS,las=2,cex=mx,side=2,line=6)
	axis(1,aveS,rep(" ",length(aveS)),lwd.ticks=lwt)
	mtext("Temp Max", side=2, line=30, cex=lx)
	mtext("Temp Ave", side=1, line=25, cex=lx)
	mtext(aveS,at=aveS,side=1,line=10,cex=mx)
	
	box(which="plot")		
	#max vs pmax
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(pmaxL,pmaxH), ylim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[5]]]$Mean,SoilL[[ycomp[5]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[1]]]$coli),cex=px)
	axis(1,pmaxS,rep(" ",length(pmaxS)),lwd.ticks=lwt)
	mtext(pmaxS,at=pmaxS,cex=mx,side=1,line=10)
	mtext("Max time", side=1, line=25, cex=lx)
	box(which="plot")		
	#legend
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(xlL,xlH), ylim=c(ylL,ylH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
			
	legend(0,10,paste(datVI$vegename),pch=19,col=as.character(vegeclassColors$coli), bty="n",cex=11)	
	box(which="plot")
dev.off()
