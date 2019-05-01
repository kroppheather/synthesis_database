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
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\thaw\\model\\run1"
Nrun <-1


#join vegeclass to data
ThawParm2 <- join(ThawParm,datV, by="siteid",type="left")
vegeID <- data.frame(vegeclass=unique(ThawParm2$vegeclass))
vegeIDc <- join(vegeID,datVI,by="vegeclass",type="left")
vegeIDc <- vegeIDc[order(vegeIDc$vegeclass),]


#######################################
#####set up colors                ##### 
#######################################

vegeclassColors <- data.frame(vegeclass=seq(1,9),
						coli=c(rgb(77/255,77/255,77/255),
								rgb(0/255,110/255,130/255),
								rgb(160/255,130/255,180/255),
								rgb(130/255,160/255,190/255),
								rgb(250/255,120/255,80/255),
								rgb(250/255,230/255,140/255),
								rgb(50/255,80/255,10/255),
								rgb(170/255,190/255,140/255),
								rgb(240/255,240/255,50/255)))

								
#######################################
#####look at model                ##### 
#######################################


#read in model results 

datM <- read.csv(paste0(modDI,"\\vege_mod_stats.csv"))
datQ <- read.csv(paste0(modDI,"\\vege_mod_quant.csv"))

datC <- cbind(datM,datQ)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
datC$parms <- gsub(dexps,"", rownames(datC))

#isolate betas

beta0 <- datC[datC$parms=="beta0",]
beta1 <- datC[datC$parms=="beta1",]
n20 <- datC[datC$parms=="n.20",]
repN <-datC[datC$parms=="repSoilP",]

beta0 <- cbind(beta0,vegeIDc)
beta1 <- cbind(beta1,vegeIDc)


#add in sig test
beta1$sigID <- ifelse(beta1$X0.3.<0&beta1$X99.7.<0,1,
				ifelse(beta1$X0.3.>0&beta1$X99.7.>0,1,0))

				
				#######################################
#####plot just intercept          ##### 
#######################################
datVI$name2 <- c("herb barren", "graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra",
					"wetland","evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")
					
datVI$namel1 <- c("herb", "graminoid","tussock","short","tall",
					"wetland","evergreen","deciduous","mixed")	
datVI$namel2 <- c("barren", "tundra","tundra","shrub","shrub",
					" ","needleleaf","needleleaf","boreal")	
datVI$namel3 <- c(" ", " "," ","tundra","tundra",
					" ","boreal","boreal"," ")		




					
wd <- 80
hd <- 80



xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(100)
yhi <- c(210)
yii <- c(25)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 6
#size of x labels
axc <- 6
#line for label num
xll <- 2

#line for units
yll1 <- 20
#line for name
yll2 <- 15
#cex of axis label
mcx <- 8
#one line width
zlw <- 9



png(paste0(plotDI,"\\run",Nrun,"\\intercepts.png"), width=2500,height=2500,
			units="px")
	#layout(matrix(seq(1,3),ncol=1), width=rep(lcm(wd),3),height=c(rep(lcm(hd),2),lcm(hd2)))
	layout(matrix(seq(1),ncol=1), width=rep(lcm(wd),1),height=rep(lcm(hd),1))
		#plot intercept
	par(mai=c(10,5,5,5))
		
		
			plot(c(0,1),c(0,1), ylim=c(yli,yhi), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			points(c(xl,xh),c(1,1),type="l",lwd=zlw, col="grey75",lty=3)
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$vegeclass==j],beta0$X75.[beta0$vegeclass==j],
							beta0$X75.[beta0$vegeclass==j],beta0$X25.[beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.3.[beta0$vegeclass==j],
						xseq[j],beta0$X99.7.[beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			mtext("Thaw days",side=2,line=yll2,cex=mcx)
			text(xseq,rep(98.1,length(xseq)),datVI$name2,srt=35, adj=1,cex=axc,xpd=TRUE)
dev.off()			