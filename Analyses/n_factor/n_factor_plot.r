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
#####set directories              ##### 
#######################################

#set up a plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\n_factor\\plots\\model"
#model directory
modDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\n_factor\\model\\run2"
Nrun <-2
#indicate if a model run is occuring
modRun <- 1


#join vegeclass to data
Nfactor2 <- join(Nfactor,datV, by="siteid",type="left")
#create regression id
Nfactor2$regID <- ifelse(Nfactor2$parm=="Fn",1,2)


#create dataframe

regvegeID <- unique(data.frame(vegeclass=Nfactor2$vegeclass,regID=Nfactor2$regID))
regvegeID <- regvegeID[order(regvegeID$regID,regvegeID$vegeclass),]
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#join back into Nfactors
Nfactor2 <- join(Nfactor2,regvegeID, by=c("regID","vegeclass"), type="left")


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

beta0 <- cbind(beta0,regvegeID)
beta1 <- cbind(beta1,regvegeID)

#add in sig test
beta1$sigID <- ifelse(beta1$X0.3.<0&beta1$X99.7.<0,1,
				ifelse(beta1$X0.3.>0&beta1$X99.7.>0,1,0))
				
plot( Nfactor2$Mean[Nfactor2$regID==1],repN$Mean[Nfactor2$regID==1])	
abline(0,1)	
fit1 <- 	lm(repN$Mean[Nfactor2$regID==1]~Nfactor2$Mean[Nfactor2$regID==1])
summary(fit1)

plot( Nfactor2$Mean[Nfactor2$regID==2],repN$Mean[Nfactor2$regID==2])	
abline(0,1)	
fit2 <- 	lm(repN$Mean[Nfactor2$regID==2]~Nfactor2$Mean[Nfactor2$regID==2])
summary(fit2)


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
hd <- 73



#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(0,0)
yhi <- c(1.5,1.7)
yii <- c(.5,.5)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 8
#size of x labels
axc <- 10
#line for label num
xll <- 2

#line for units
yll1 <- 20
#line for name
yll2 <- 15
#cex of axis label
mcx <- 13
#one line width
zlw <- 9


png(paste0(plotDI,"\\run",Nrun,"\\intercepts2.png"), width=3000,height=4200,
			units="px")
	#layout(matrix(seq(1,3),ncol=1), width=rep(lcm(wd),3),height=c(rep(lcm(hd),2),lcm(hd2)))
	layout(matrix(seq(1,2),ncol=1), width=rep(lcm(wd),2),height=rep(lcm(hd),2))
		#plot intercept
	
		par(mai=c(1,6,15,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[1],yhi[1]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			points(c(xl,xh),c(1,1),type="l",lwd=zlw, col="grey75",lty=3)
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==1&beta0$vegeclass==j],beta0$X75.[beta0$regID==1&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==1&beta0$vegeclass==j],beta0$X25.[beta0$regID==1&beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.3.[beta0$regID==1&beta0$vegeclass==j],
						xseq[j],beta0$X99.7.[beta0$regID==1&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			#mtext(expression(paste("(N"[freeze],")")),side=2,line=yll1,cex=mcx)
			mtext("Freeze n-factor",side=2,line=yll2,cex=mcx)
			
		par(mai=c(15,6,1,0),xpd=TRUE)
		
			plot(c(0,1),c(0,1), ylim=c(yli[2],yhi[2]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		
			points(c(xl,xh),c(1,1),type="l",lwd=zlw, col="grey75",lty=3)
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(beta0$X25.[beta0$regID==2&beta0$vegeclass==j],beta0$X75.[beta0$regID==2&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==2&beta0$vegeclass==j],beta0$X25.[beta0$regID==2&beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[j]-1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],
						xseq[j]+1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],beta0$X0.3.[beta0$regID==2&beta0$vegeclass==j],
						xseq[j],beta0$X99.7.[beta0$regID==2&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[2],yhi[2], by=yii[2]), rep(" ",length(seq(yli[2],yhi[2], by=yii[2]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[2],yhi[2], by=yii[2]),at=seq(yli[2],yhi[2], by=yii[2]), side=2, line=xll,cex=axc,las=2)	
			#mtext(expression(paste("(N"[thaw],")")),side=2,line=yll1,cex=mcx)			
			mtext("Thaw n-factor",side=2,line=yll2,cex=mcx)
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			text(xseq,rep(-.1,length(xseq)),datVI$name2,srt=35, adj=1,cex=axc,xpd=TRUE)
			#labels
			#par(mai=c(0,0,1,0),xpd=TRUE)
			#plot(c(0,1),c(0,1), ylim=c(0,1), xlim=c(xl,xh),
			#	xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
						
			

				
			
dev.off()	

				

				
#######
	plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\figures"			
plot( Nfactor2$Mean[Nfactor2$regID==1],repN$Mean[Nfactor2$regID==1])	
abline(0,1)	
fit1 <- 	lm(repN$Mean[Nfactor2$regID==1]~Nfactor2$Mean[Nfactor2$regID==1])
summary(fit1)

plot( Nfactor2$Mean[Nfactor2$regID==2],repN$Mean[Nfactor2$regID==2])	
abline(0,1)	
fit2 <- 	lm(repN$Mean[Nfactor2$regID==2]~Nfactor2$Mean[Nfactor2$regID==2])
summary(fit2)



hd <- 30
wd <- 30
#soil range
tl1 <-0
th1 <- 1.3
tl2 <-0
th2 <- 1.5

lws <- 4
sseq <- seq(0,1.2, by=.2)
sseq2 <- seq(0,1.4, by=.2)
png(paste0(plotDI,"\\supp_n_fit.png"), width=2000,height=1000,
			units="px")
	layout(matrix(c(1,2),ncol=2,byrow=TRUE), width=c(lcm(wd),lcm(wd)),height=lcm(hd))
	par(mai=c(1,1,1,1))
		
			plot(c(0,1),c(0,1), ylim=c(tl1,th1), xlim=c(tl1,th1),
				xlab="Observed freezing n-factor ", 
				ylab="Predicted freezing n-factor ",xaxs="i",yaxs="i",axes=FALSE, cex.lab=2.5)
			points(Nfactor2$Mean[Nfactor2$regID==1],repN$Mean[Nfactor2$regID==1], pch=19, col=rgb(.5,.5,.5,.15), cex=1.5	)
	text(.3,1.15, paste("R squared =", round(summary(fit1)$r.squared,2)), cex=2)
	text(.4,1.1, paste("Predicted =", round(summary(fit1)$coefficients[1,1],2),
		"+",round(summary(fit1)$coefficients[2,1],2),"observed"), cex=2)
	
	abline(0,1,col="red", lwd=lws)
	abline(fit1,col="cornflowerblue", lty=3, lwd= lws)
	axis(1, sseq, cex.axis=1.5)
	axis(2, sseq, cex.axis=1.5, las=2)
	par(mai=c(1,1,1,1))
		
			plot(c(0,1),c(0,1), ylim=c(tl2,th2), xlim=c(tl2,th2),
				xlab="Observed thawing n-factor ", 
				ylab="Predicted thawing n-factor ",xaxs="i",yaxs="i",axes=FALSE, cex.lab=2.5)
			points(Nfactor2$Mean[Nfactor2$regID==2],repN$Mean[Nfactor2$regID==2], pch=19, col=rgb(.5,.5,.5,.15), cex=1.5	)
	text(.3,1.35, paste("R squared =", round(summary(fit2)$r.squared,2)), cex=2)
	text(.4,1.3, paste("Predicted =", round(summary(fit2)$coefficients[1,1],2),
		"+",round(summary(fit2)$coefficients[2,1],2),"observed"), cex=2)
	
	abline(0,1,col="red", lwd=lws)
	abline(fit2,col="cornflowerblue", lty=3, lwd= lws)
	axis(1, sseq2, cex.axis=1.5)
	axis(2, sseq2, cex.axis=1.5, las=2)
	
dev.off()	