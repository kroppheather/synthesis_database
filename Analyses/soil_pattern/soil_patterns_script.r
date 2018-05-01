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
library(plyr)

#######################################
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_pattern\\plots"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_pattern\\model\\run4"
Nrun <-4
#indicate if a model run is occuring
modRun <- 0


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

xcompDF <- data.frame(xcomp=xcomp,ycomp=ycomp,compNameY=compNameY,compNameX=compNameX,xcent=xcent,
				xmin=c(round_any(min(SoilL[[xcomp[1]]]$pc2.5),5),round_any(min(SoilL[[xcomp[2]]]$pc2.5),5),
					round_any(min(SoilL[[xcomp[3]]]$pc2.5),.1),round_any(min(SoilL[[xcomp[4]]]$pc2.5),5),round_any(min(SoilL[[xcomp[5]]]$pc2.5),.1)),
				xmax=c(round_any(max(SoilL[[xcomp[1]]]$pc97.5),5),round_any(max(SoilL[[xcomp[2]]]$pc97.5),5),
					round_any(max(SoilL[[xcomp[3]]]$pc97.5),.1),round_any(max(SoilL[[xcomp[4]]]$pc97.5),5),round_any(max(SoilL[[xcomp[5]]]$pc97.5),.1)),
				ymin=c(round_any(min(SoilL[[ycomp[1]]]$pc2.5),5),round_any(min(SoilL[[ycomp[2]]]$pc2.5),5),
					round_any(min(SoilL[[ycomp[3]]]$pc2.5),5),round_any(min(SoilL[[ycomp[4]]]$pc2.5),5),round_any(min(SoilL[[ycomp[5]]]$pc2.5),5)),
				ymax=c(round_any(max(SoilL[[ycomp[1]]]$pc97.5),5),round_any(max(SoilL[[ycomp[2]]]$pc97.5),5),
					round_any(max(SoilL[[ycomp[3]]]$pc97.5),5),round_any(max(SoilL[[ycomp[4]]]$pc97.5),5),round_any(max(SoilL[[ycomp[5]]]$pc97.5),5)))


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



#set up a matrix for plot
xplot <- matrix(rep(NA,100*dim(vegeComp)[1]),ncol=dim(vegeComp)[1])

	for(j in 1:dim(vegeComp)[1]){
		xplot[,j] <- seq(xcompDF$xmin[vegeComp$comp[j]],xcompDF$xmax[vegeComp$comp[j]],length.out=100)
	}
xplotDF <- data.frame(xplot=as.vector(xplot),vegeCompID=rep(seq(1,dim(vegeComp)[1]),each=100), comp=rep(vegeComp$comp,each=100),vegeClass=rep(vegeComp$vegeClass,each=100))
	
#multiple comparision quantile
mcQ <- round_any(0.05/(dim(vegeComp)[1]-1)	,0.001)
	
#######################################
#####set up model run             ##### 
#######################################
#data frame of all observations
if(modRun==1){					
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
				compP=xplotDF$comp)
				
parms <- c("rep.yvar","sig.compVege","beta0","beta1","mu.beta0","mu.beta1","sig.beta0","sig.beta1", "mu.plot")

comp.modI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\soil_pattern\\soil_patterns_model_code.r",
						data=datalist,
						n.adapt=50000,
						n.chains=3)

comp.sample <- coda.samples(comp.modI,variable.names=parms,
                       n.iter=150000, thin=50)	
					   
mcmcplot(comp.sample, parms=c("sig.compVege","beta0","beta1","mu.beta0","mu.beta1","sig.beta0","sig.beta1"),
			dir=paste0(modDI,"\\history"))	
					   
mod.out <- summary(comp.sample,  quantiles = c(mcQ,0.025, 0.25, 0.5, 0.75, 0.975,1-mcQ))

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
}
#######################################
#####end model run                ##### 
#######################################



#######################################
#####plot model results           ##### 
#######################################


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
colnames(beta0)[13] <- "vegeCompID"
colnames(beta1)[13] <- "vegeCompID"

beta0 <- join(beta0, vegeComp, by=c("vegeCompID"), type="left")
beta1 <- join(beta1, vegeComp, by=c("vegeCompID"), type="left")

#choose lower interval
mubeta0$pc.l <- mubeta0$X0.1.
mubeta1$pc.l <- mubeta1$X0.1.
beta0$pc.l <- beta0$X0.1.
beta1$pc.l <- beta1$X0.1.


#choose upper interval
mubeta0$pc.h <- mubeta0$X99.9.
mubeta1$pc.h <- mubeta1$X99.9.
beta0$pc.h <- beta0$X99.9.
beta1$pc.h <- beta1$X99.9.




mubeta0$compNameX <- compNameX
mubeta0$compNameY <- compNameY
mubeta1$compNameX <- compNameX
mubeta1$compNameY <- compNameY

yl <- xcompDF$ymin
yh <- xcompDF$ymax

yl2 <- c(-.1,-1,-100,-1,-100)
yh2 <- c(.6,1,100,2,100)

xseq <- seq(1,17,by=2)

VIlab <- c("herb bare", "nontussock T","tussock T","short shrub T", "tall shrub T","wetland T", "evergreen B","deciduous B", "mixed B")

#make a plot of the parms
for(i in 1:length(compNameX)){
	jpeg(paste0(plotDI,"\\model\\run",Nrun,"\\parms",compNameX[i],"_vs_",compNameY[i],".jpg"), width=3000, height=1000, units="px",quality=100)
		par(mfrow=c(1,2), mai=c(2,2,2,2))
			plot(c(0,1),c(0,1), type="n", ylim=c(yl[i],yh[i]), xlim=c(0,20), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
			
			abline(h=mubeta0$Mean[i], lwd=3)	
			polygon(c(0,0,20,20), c(mubeta0$pc.l[i],	mubeta0$pc.h[i],mubeta0$pc.h[i],mubeta0$pc.l[i]),
				col=rgb(153/255,50/255,204/255,.3),border=NA)

				for(j in 1:9){
					
				arrows(xseq[j], beta0$pc.l[beta0$comp==i&beta0$vegeClass==j],xseq[j], beta0$pc.h[beta0$comp==i&beta0$vegeClass==j],
						lwd=2,code=0)
					
					polygon(c(xseq[j]-.5,xseq[j]-.5,xseq[j]+.5,xseq[j]+.5),
							c(beta0$X25.[beta0$comp==i&beta0$vegeClass==j],beta0$X75.[beta0$comp==i&beta0$vegeClass==j],
							beta0$X75.[beta0$comp==i&beta0$vegeClass==j],beta0$X25.[beta0$comp==i&beta0$vegeClass==j]),
							col="cornflowerblue")		
				
				arrows(xseq[j]-.5,beta0$Mean[beta0$comp==i&beta0$vegeClass==j],xseq[j]+.5,beta0$Mean[beta0$comp==i&beta0$vegeClass==j],
						lwd=3,code=0)
				}
		arrows(xseq[j], beta0$pc.l[beta0$comp==i&beta0$vegeClass==j],xseq[j], beta0$pc.h[beta0$comp==i&beta0$vegeClass==j],
						lwd=2,code=0)			
		

		axis(2,seq(yl[i],yh[i],by=5),las=2, cex.axis=1.5)
		axis(1, xseq, VIlab, cex.axis=1.5)
		mtext(paste(compNameY[i]), side=2, cex=2,line=3)
		mtext(paste("Intercept at average", compNameX[i]), side=1, cex=2,line=3)
	plot(c(0,1),c(0,1), type="n", ylim=c(yl2[i],yh2[i]), xlim=c(0,20), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	
	
	
		abline(h=mubeta1$Mean[i], lwd=3)	
		polygon(c(0,0,20,20), c(mubeta1$pc.l[i],	mubeta1$pc.h[i],mubeta1$pc.h[i],mubeta1$pc.l[i]),
		col=rgb(153/255,50/255,204/255,.3),border=NA)
		abline(h=0, lwd=4, lty=3, col="grey35")
		for(j in 1:9){
					
				arrows(xseq[j], beta1$pc.l[beta1$comp==i&beta1$vegeClass==j],xseq[j], beta1$pc.h[beta1$comp==i&beta1$vegeClass==j],
						lwd=2,code=0)
					
					polygon(c(xseq[j]-.5,xseq[j]-.5,xseq[j]+.5,xseq[j]+.5),
							c(beta1$X25.[beta1$comp==i&beta1$vegeClass==j],beta1$X75.[beta1$comp==i&beta1$vegeClass==j],
							beta1$X75.[beta1$comp==i&beta1$vegeClass==j],beta1$X25.[beta1$comp==i&beta1$vegeClass==j]),
							col="cornflowerblue")		
				
				arrows(xseq[j]-.5,beta1$Mean[beta1$comp==i&beta1$vegeClass==j],xseq[j]+.5,beta1$Mean[beta1$comp==i&beta1$vegeClass==j],
						lwd=3,code=0)
				}
	
		axis(2,round(seq(yl2[i],yh2[i],length.out=5),2),las=2, cex.axis=1.5)
		axis(1, xseq, VIlab, cex.axis=1.5)
		mtext(paste(compNameY[i]), side=2, cex=2,line=3)
		mtext(paste("slope with", compNameX[i]), side=1, cex=2,line=3)
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
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(minL,minH), ylim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[2]]]$Mean,SoilL[[ycomp[2]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[2]]]$coli),cex=px)

	
	
	axis(2,maxS,rep(" ",length(maxS)),lwd.ticks=lwt)
	mtext(maxS,at=maxS,las=2,cex=mx,side=2,line=6)
	axis(3,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,side=3,line=6,cex=mx)
	mtext("Temp Max", side=2, line=30, cex=lx)
	mtext("Temp Min", side=3, line=25, cex=lx)
	box(which="plot")		
	#min vs max		
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(maxL,maxH), xlim=c(pmaxL,pmaxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
	points(SoilL[[xcomp[5]]]$Mean,SoilL[[ycomp[5]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[5]]]$coli),cex=px)		
	
	axis(3,pmaxS,rep(" ",length(pmaxS)),lwd.ticks=lwt)
	mtext(pmaxS,at=pmaxS,side=3,line=6,cex=mx)
	mtext("Time Temp Max", side=3, line=25, cex=lx)	
	box(which="plot")		
	#min vs pmin		
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(pminL,pminH), ylim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
	points(SoilL[[xcomp[3]]]$Mean,SoilL[[ycomp[3]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[3]]]$coli),cex=px)
	
	
	axis(3,pminS,rep(" ",length(pminS)),lwd.ticks=lwt)
	mtext(pminS,at=pminS,side=3,line=6,cex=mx)	
	axis(4,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,las=2,cex=mx,side=4,line=6)
	mtext("Min time", side=3, line=25, cex=lx)
	mtext("Temp Min", side=4, line=30, cex=lx)
	box(which="plot")		
	#max vs ave
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(aveL,aveH), xlim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[4]]]$Mean,SoilL[[ycomp[4]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[4]]]$coli),cex=px)
	
	
	axis(1,maxS,rep(" ",length(maxS)),lwd.ticks=lwt)
	mtext(maxS,at=maxS,las=1,cex=mx,side=1,line=6)
	axis(2,aveS,rep(" ",length(aveS)),lwd.ticks=lwt)
	mtext("Temp Max", side=1, line=30, cex=lx)
	mtext("Temp Ave", side=2, line=25, cex=lx)
	mtext(aveS,at=aveS,side=2,line=10,cex=mx)
	
	box(which="plot")		
	#max vs pmax
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(minL,pminH), ylim=c(aveL,aveH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[1]]]$Mean,SoilL[[ycomp[1]]]$Mean, pch=19,col=as.character(SoilL[[xcomp[1]]]$coli),cex=px)

	
	axis(1,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,cex=mx,side=1,line=10)
	mtext("Temp Min", side=1, line=25, cex=lx)
	box(which="plot")		
	#legend
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(xlL,xlH), ylim=c(ylL,ylH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
			
	legend(0,10,paste(datVI$vegename),pch=19,col=as.character(vegeclassColors$coli), bty="n",cex=11)	
	box(which="plot")
dev.off()



#######################################
##### plot of regression result   ##### 
#######################################


#add regression lines from model into  xplotdf

xplotDF$Mean <- datC$Mean[datC$parms=="mu.plot"]
xplotDF$pc.l <- datC$X0.1.[datC$parms=="mu.plot"]
xplotDF$pc.h <- datC$X99.9.[datC$parms=="mu.plot"]


beta0$sig <- ifelse(beta0$pc.l<0&beta0$pc.h<0,1,
				ifelse(beta0$pc.l>0&beta0$pc.h>0,1,0))

beta1$sig <- ifelse(beta1$pc.l<0&beta1$pc.h<0,1,
				ifelse(beta1$pc.l>0&beta1$pc.h>0,1,0))
				
mubeta0$sig <- ifelse(mubeta0$pc.l<0&mubeta0$pc.h<0,1,
				ifelse(mubeta0$pc.l>0&mubeta0$pc.h>0,1,0))

mubeta1$sig <- ifelse(mubeta1$pc.l<0&mubeta1$pc.h<0,1,
				ifelse(mubeta1$pc.l>0&mubeta1$pc.h>0,1,0))				
				

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

regF <- function(b0,b1,x,xcent){
	b0+(b1*(x-xcent))

}

jpeg(paste0(plotDI,"\\model\\run",Nrun,"\\soil_comp_reg.jpg"), width=5500, height=5000, units="px",quality=100)
	layout(matrix(seq(1,6),byrow=TRUE,ncol=3), width=rep(lcm(wd),6), height=rep(lcm(hd),6))
	#ave vs min
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(minL,minH), ylim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[2]]]$Mean,SoilL[[ycomp[2]]]$Mean, pch=19,col="grey25",cex=px)
	
	#global mean
	if(mubeta1$sig[2]==1){
		points(xplotDF$xplot[xplotDF$comp==1],xplotDF$Mean, lwd=2)
		
		
		
		polygon(c(xplotDF$xplot,rev(xplotDF$xplot)),
				c(regF(mubeta0$pc.h[2],mubeta1$pc.h[2],seq(minL,minH,length.out=100),xcent[2]),
					rev(regF(mubeta0$pc.l[2],mubeta1$pc.l[2],seq(minL,minH,length.out=100),xcent[2]))), border=NA,col=rgb(.75,.75,.75,.5))
	}else{
		abline(h=mubeta0$Mean[2],lwd=2,lty=2)
		polygon(c(seq(minL,minH,length.out=100),rev(seq(minL,minH,length.out=100))),
				c(regF(mubeta0$pc.h[2],0,seq(minL,minH,length.out=100),xcent[2]),
				rev(regF(mubeta0$pc.l[2],0,seq(minL,minH,length.out=100),xcent[2]))),border=NA,col=rgb(.75,.75,.75,.5))
	}
	
	
	axis(2,maxS,rep(" ",length(maxS)),lwd.ticks=lwt)
	mtext(maxS,at=maxS,las=2,cex=mx,side=2,line=6)
	axis(3,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,side=3,line=6,cex=mx)
	mtext("Temp Max", side=2, line=30, cex=lx)
	mtext("Temp Min", side=3, line=25, cex=lx)
	box(which="plot")		
	#min vs max		
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(maxL,maxH), xlim=c(pmaxL,pmaxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
	points(SoilL[[xcomp[5]]]$Mean,SoilL[[ycomp[5]]]$Mean, pch=19,col="grey25",cex=px)		
		#global mean
	if(mubeta1$sig[5]==1){
		points(seq(pmaxL,pmaxH,length.out=100),regF(mubeta0$Mean[5],mubeta1$Mean[5],seq(pmaxL,pmaxH,length.out=100),xcent[5]), lwd=2)
		
		
		
		polygon(c(seq(pmaxL,pmaxH,length.out=100),rev(seq(pmaxL,pmaxH,length.out=100))),
				c(regF(mubeta0$pc.h[5],mubeta1$pc.h[5],seq(pmaxL,pmaxH,length.out=100),xcent[5]),
					rev(regF(mubeta0$pc.l[5],mubeta1$pc.l[5],seq(pmaxL,pmaxH,length.out=100),xcent[5]))), border=NA,col=rgb(.75,.75,.75,.5))
	}else{
		abline(h=mubeta0$Mean[5],lwd=2,lty=2)
		polygon(c(seq(pmaxL,pmaxH,length.out=100),rev(seq(pmaxL,pmaxH,length.out=100))),
				c(regF(mubeta0$pc.h[5],0,seq(pmaxL,pmaxH,length.out=100),xcent[5]),
				rev(regF(mubeta0$pc.l[5],0,seq(pmaxL,pmaxH,length.out=100),xcent[5]))),border=NA,col=rgb(.75,.75,.75,.5))
	}
	
	
	
	axis(3,pmaxS,rep(" ",length(pmaxS)),lwd.ticks=lwt)
	mtext(pmaxS,at=pmaxS,side=3,line=6,cex=mx)
	mtext("Time Temp Max", side=3, line=25, cex=lx)	
	box(which="plot")		
	#min vs pmin		
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(pminL,pminH), ylim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
	points(SoilL[[xcomp[3]]]$Mean,SoilL[[ycomp[3]]]$Mean, pch=19,col="grey25",cex=px)
	
		#global mean
	if(mubeta1$sig[3]==1){
		points(seq(pminL,pminH,length.out=100),regF(mubeta0$Mean[3],mubeta1$Mean[3],seq(pminL,pminH,length.out=100),xcent[3]), lwd=2)
		
		
		
		polygon(c(seq(pminL,pminH,length.out=100),rev(seq(pminL,pminH,length.out=100))),
				c(regF(mubeta0$pc.h[3],mubeta1$pc.h[3],seq(pminL,pminH,length.out=100),xcent[3]),
					rev(regF(mubeta0$pc.l[3],mubeta1$pc.l[3],seq(pminL,pminH,length.out=100),xcent[3]))), border=NA,col=rgb(.75,.75,.75,.5))
	}else{
		abline(h=mubeta0$Mean[3],lwd=2,lty=2)
		polygon(c(seq(pminL,pminH,length.out=100),rev(seq(pminL,pminH,length.out=100))),
				c(regF(mubeta0$pc.h[3],0,seq(pminL,pminH,length.out=100),xcent[3]),
				rev(regF(mubeta0$pc.l[3],0,seq(pminL,pminH,length.out=100),xcent[3]))),border=NA,col=rgb(.75,.75,.75,.5))
	}
	
	
	axis(3,pminS,rep(" ",length(pminS)),lwd.ticks=lwt)
	mtext(pminS,at=pminS,side=3,line=6,cex=mx)	
	axis(4,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,las=2,cex=mx,side=4,line=6)
	mtext("Min time", side=3, line=25, cex=lx)
	mtext("Temp Min", side=4, line=30, cex=lx)
	box(which="plot")		
	#max vs ave
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(aveL,aveH), xlim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[4]]]$Mean,SoilL[[ycomp[4]]]$Mean, pch=19,col="grey25",cex=px)
	
	#global mean
	if(mubeta1$sig[4]==1){
		points(seq(maxL,maxH,length.out=100),regF(mubeta0$Mean[4],mubeta1$Mean[4],seq(maxL,maxH,length.out=100),xcent[4]), lwd=2)
		
		
		
		polygon(c(seq(maxL,maxH,length.out=100),rev(seq(maxL,maxH,length.out=100))),
				c(regF(mubeta0$pc.h[4],mubeta1$pc.h[4],seq(maxL,maxH,length.out=100),xcent[4]),
					rev(regF(mubeta0$pc.l[4],mubeta1$pc.l[4],seq(maxL,maxH,length.out=100),xcent[4]))), border=NA,col=rgb(.75,.75,.75,.5))
	}else{
		abline(h=mubeta0$Mean[4],lwd=2,lty=2)
		polygon(c(seq(maxL,maxH,length.out=100),rev(seq(maxL,maxH,length.out=100))),
				c(regF(mubeta0$pc.h[4],0,seq(maxL,maxH,length.out=100),xcent[4]),
				rev(regF(mubeta0$pc.l[4],0,seq(maxL,maxH,length.out=100),xcent[4]))),border=NA,col=rgb(.75,.75,.75,.5))
	}
	
	
	
	
	axis(1,maxS,rep(" ",length(maxS)),lwd.ticks=lwt)
	mtext(maxS,at=maxS,las=1,cex=mx,side=1,line=6)
	axis(2,aveS,rep(" ",length(aveS)),lwd.ticks=lwt)
	mtext("Temp Max", side=1, line=30, cex=lx)
	mtext("Temp Ave", side=2, line=25, cex=lx)
	mtext(aveS,at=aveS,side=2,line=10,cex=mx)
	
	box(which="plot")		
	#max vs pmax
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(minL,pminH), ylim=c(aveL,aveH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	points(SoilL[[xcomp[1]]]$Mean,SoilL[[ycomp[1]]]$Mean, pch=19,col="grey25",cex=px)
		#global mean
	if(mubeta1$sig[1]==1){
		points(seq(minL,minH,length.out=100),regF(mubeta0$Mean[1],mubeta1$Mean[1],seq(minL,minH,length.out=100),xcent[1]), lwd=2)
		
		
		
		polygon(c(seq(minL,minH,length.out=100),rev(seq(minL,minH,length.out=100))),
				c(regF(mubeta0$pc.h[1],mubeta1$pc.h[1],seq(minL,minH,length.out=100),xcent[1]),
					rev(regF(mubeta0$pc.l[1],mubeta1$pc.l[1],seq(minL,minH,length.out=100),xcent[1]))), border=NA,col=rgb(.75,.75,.75,.5))
	}else{
		abline(h=mubeta0$Mean[1],lwd=2,lty=2)
		polygon(c(seq(minL,minH,length.out=100),rev(seq(minL,minH,length.out=100))),
				c(regF(mubeta0$pc.h[1],0,seq(minL,minH,length.out=100),xcent[1]),
				rev(regF(mubeta0$pc.l[1],0,seq(minL,minH,length.out=100),xcent[1]))),border=NA,col=rgb(.75,.75,.75,.5))
	}
	
	
	
	
	axis(1,minS,rep(" ",length(minS)),lwd.ticks=lwt)
	mtext(minS,at=minS,cex=mx,side=1,line=10)
	mtext("Temp Min", side=1, line=25, cex=lx)
	box(which="plot")		
	#legend
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(xlL,xlH), ylim=c(ylL,ylH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)	
			
	legend(0,10,paste(datVI$vegename),pch=19,col=as.character(vegeclassColors$coli), bty="n",cex=11)	
	box(which="plot")
dev.off()





