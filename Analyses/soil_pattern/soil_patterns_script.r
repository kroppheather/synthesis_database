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
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_pattern\\plots"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_pattern\\model\\run1"

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

for(i in 1:length(parmV)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmV[i],]

}



#######################################
#####set up model run             ##### 
#######################################







#######################################
#####make plot of data            ##### 
#######################################

#make figure for soil comparisons

#pull out only revelevant comparisons that are meaningful

#Tmin vs Tave
#Tmin vs Tmax
#pmin vs Tmin
#Tmax vs Tave
#Tmax vs pmax
#pmin vs Tmax

#set up comparisions
#x values
xcomp <- c(6,5,3,6,2)
ycomp <- c(4,4,4,5,5)




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
