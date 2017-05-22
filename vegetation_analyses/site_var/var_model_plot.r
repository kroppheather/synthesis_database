#######################################################
########this script reads in output from model 10 #####
########and plots data and empirical mdoel output #####
#######################################################

library(plyr)

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

SIcheck<-aggregate(datS1$Mean[datS1$parm=="TminS"], 
				by=list(datS1$region[datS1$parm=="TminS"],
						datS1$class[datS1$parm=="TminS"]),
				FUN="length")
colnames(SIcheck)<-c("region","class","count")
#now exclude anything with a sample size of less or equal to 3
SIcheckS<-SIcheck[SIcheck$count>3,]
#now add a unique ID
SIcheckS$svID<-seq(1, dim(SIcheckS)[1])

#now count how many regions in each class
regI<-aggregate(SIcheckS$region, by=list(SIcheckS$class), FUN="length")
colnames(regI)<-c("class", "count")
#now match air temp min and max to N factor

datNF<-datN1[datN1$parm=="Fn",]
datNF<-datN1[datN1$parm=="Tn",]
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

#vege class Name
className<-c("herb barren",	"gramminoid tundra", "tussock tundra",	
			"shrub tundra",	"gramminoid wetland", "needleleaf deciduous",
				"needleleaf evergreen", "mixed conifer decidous")

region.name<-unique(data.frame(region=datR$region,name=datR$region_name))
region.name$nameF<-c("Alaska","Canada", "E Russia", "W Russia", "Islands")
#start by looking at how these parameters plot out

#get vegeclass/region info out
regInClass<-list()
for(i in 1:length(className)){

	regInClass[[i]]<-SIcheckS[SIcheckS$class==i,]
}


region.name$rcolor<-c("mediumseagreen",
						"lightsteelblue",
						"thistle4",
						"coral4",
						"royalblue4")

##################################################
##################################################
#plot zero curtain model days

wb<-40
hb<-40
nlowT=0
nhighT=240


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\Zero_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datDZ$depth[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
				datDZ$Mean[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datDZ$depth[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
			datDZ$pc2.5[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
			datDZ$depth[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 	
			datDZ$pc97.5[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
		mtext("Days in Zero Mean", side=2, cex=10, line=25 )	
		axis(2, seq(0,200, by=50), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(0,240, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(min(datDZ$pc2.5A[datDZ$class==i]-1),max(datDZ$pc97.5A[datDZ$class==i]+1)), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datDZ$MeanA[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
				datDZ$Mean[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datDZ$MeanA[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
			datDZ$pc2.5[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
			datDZ$MeanA[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 	
			datDZ$pc97.5[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datDZ$pc2.5A[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
			datDZ$Mean[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 
			datDZ$pc97.5A[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]], 	
			datDZ$Mean[datDZ$class==regInClass[[i]]$class[j]&datDZ$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
		mtext("Days in Zero Mean", side=2, cex=10, line=25 )	
		axis(2, seq(0,200, by=50), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8,padj=1) 
		
		legend(min(datDZ$pc2.5A[datDZ$class==i]-1),240, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Minimum Air Temperature (Tmin, C)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	

dev.off()




#################################################
#############thawing n factor

##################################################
##################################################


wb<-40
hb<-40
nlowT=0
nhighT=1.7


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\ThawN_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datNT$depth[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
				datNT$Mean[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datNT$depth[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
			datNT$pc2.5[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
			datNT$depth[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 	
			datNT$pc97.5[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
		mtext("Thawing n factor", side=2, cex=10, line=25 )	
		axis(2, seq(0,1.5, by=.50), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(10,1.7, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(min(datNT$pc2.5A[datNT$class==i]-1),max(datNT$pc97.5A[datNT$class==i]+1)), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datNT$MeanA[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
				datNT$Mean[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datNT$MeanA[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
			datNT$pc2.5[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
			datNT$MeanA[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 	
			datNT$pc97.5[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datNT$pc2.5A[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
			datNT$Mean[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 
			datNT$pc97.5A[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]], 	
			datNT$Mean[datNT$class==regInClass[[i]]$class[j]&datNT$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
		mtext("Thawing n factor", side=2, cex=10, line=25 )	
		axis(2, seq(0,1.5, by=.550), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(0,35, by=5),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Maximum Air Temperature (Tmin, C)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	

dev.off()


#################################################
#############freezing n factor

##################################################
##################################################


wb<-40
hb<-40
nlowT=0
nhighT=1.7


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\freezeN_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datNF$depth[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
				datNF$Mean[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datNF$depth[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
			datNF$pc2.5[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
			datNF$depth[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 	
			datNF$pc97.5[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
		mtext("Freezing  factor", side=2, cex=10, line=25 )	
		axis(2, seq(0,1.5, by=.50), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(10,1.7, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(min(datNF$pc2.5A[datNF$class==i]-1),max(datNF$pc97.5A[datNF$class==i]+1)), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datNF$MeanA[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
				datNF$Mean[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datNF$MeanA[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
			datNF$pc2.5[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
			datNF$MeanA[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 	
			datNF$pc97.5[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datNF$pc2.5A[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
			datNF$Mean[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 
			datNF$pc97.5A[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]], 	
			datNF$Mean[datNF$class==regInClass[[i]]$class[j]&datNF$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
		mtext("Freezing n factor", side=2, cex=10, line=25 )	
		axis(2, seq(0,1.5, by=.550), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(-50,0, by=5),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Minimum Air Temperature (Tmin, C)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	

dev.off()


#################################################
#############minimum T

##################################################
##################################################


wb<-40
hb<-40
nlowT=-45
nhighT=0


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\Tmin_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datTmin$depth[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
				datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datTmin$depth[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$pc2.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$depth[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 	
			datTmin$pc97.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
			
		axis(2, seq(-40,0, by=5), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(10,-30, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(min(datTmin$pc2.5A[datTmin$class==i]-1),max(datTmin$pc97.5A[datTmin$class==i]+1)), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datTmin$MeanA[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
				datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datTmin$MeanA[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$pc2.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$MeanA[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 	
			datTmin$pc97.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datTmin$pc2.5A[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$pc97.5A[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 	
			datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
			
		axis(2, seq(-40,0, by=5), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(-50,0, by=5),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Minimum Air Temperature (Tmin, C)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	
	mtext("Minimum Soil Temperature (Tmin, C)", outer=TRUE, line=-20, cex=10, side=2 )
dev.off()



#################################################
#############minimum T

##################################################
##################################################


wb<-40
hb<-40
nlowT=0
nhighT=25


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\Tmax_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datTmax$depth[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
				datTmax$Mean[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datTmax$depth[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
			datTmax$pc2.5[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
			datTmax$depth[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 	
			datTmax$pc97.5[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
			
		axis(2, seq(0,30, by=5), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,30, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(0,25, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(min(datTmax$pc2.5A[datTmax$class==i]-1),max(datTmax$pc97.5A[datTmax$class==i]+1)), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datTmax$MeanA[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
				datTmax$Mean[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datTmax$MeanA[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
			datTmax$pc2.5[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
			datTmax$MeanA[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 	
			datTmax$pc97.5[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datTmax$pc2.5A[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
			datTmax$Mean[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 
			datTmax$pc97.5A[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]], 	
			datTmax$Mean[datTmax$class==regInClass[[i]]$class[j]&datTmax$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
			
		axis(2, seq(0,30, by=5), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(0,30, by=5),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Maximum Air Temperature (Tmin, C)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	
	mtext("Maximum Soil Temperature (Tmin, C)", outer=TRUE, line=-20, cex=10, side=2 )
dev.off()

#################################################
#############minimum T timing

##################################################
##################################################


wb<-40
hb<-40
nlowT=.2
nhighT=.6


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\TminPeak_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datPW$depth[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
				datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datPW$depth[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$pc2.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$depth[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 	
			datPW$pc97.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
			
		axis(2, seq(0,.5, by=.1), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,20, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(10,.37, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(.2,.6), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datPW$MeanA[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
				datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datPW$MeanA[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$pc2.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$MeanA[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 	
			datPW$pc97.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datPW$pc2.5A[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$pc97.5A[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 	
			datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
			
		axis(2, seq(.2,.5, by=.1), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(.25,.55, by=.1),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Minimum Air Temperature Time (pmin)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	
	mtext("Minimum Soil Temperature (pmin)", outer=TRUE, line=-20, cex=10, side=2 )
dev.off()


#################################################
#############maximum T timing

##################################################
##################################################


wb<-40
hb<-40
nlowT=.6
nhighT=1


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\TmaxPeak_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datPS$depth[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
				datPS$Mean[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datPS$depth[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
			datPS$pc2.5[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
			datPS$depth[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 	
			datPS$pc97.5[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
			
		axis(2, seq(.6,1, by=.1), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,20, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(12,1, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(.6,1), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datPS$MeanA[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
				datPS$Mean[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], pch=19,
					col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
			#arrows for y		
			arrows(datPS$MeanA[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
			datPS$pc2.5[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
			datPS$MeanA[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 	
			datPS$pc97.5[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datPS$pc2.5A[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
			datPS$Mean[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 
			datPS$pc97.5A[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]], 	
			datPS$Mean[datPS$class==regInClass[[i]]$class[j]&datPS$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
			
		axis(2, seq(.6,.9, by=.1), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(.65,.95, by=.1),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Maximum Air Temperature Time (pmax)", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	
	mtext("Maximum Air Temperature (pmin)", outer=TRUE, line=-20, cex=10, side=2 )
dev.off()



#################################################
#############see how zero curtain might 
#############affect min and timing of min
##################################################
##################################################


wb<-40
hb<-40
nlowT=.2
nhighT=.6


jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\zeromin.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,240), ylim=c(nlowT,nhighT), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datDZ$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
				datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datDZ$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$pc2.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datDZ$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 	
			datPW$pc97.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			
			arrows(datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datDZ$pc2.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 
			datPW$Mean[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]], 	
			datDZ$pc97.5[datPW$class==regInClass[[i]]$class[j]&datPW$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
		mtext("Peak Tmin (pmin)", line=20, cex=10, side=2 )	
		axis(2, seq(.2,.6, by=.1), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,250, by=50),cex.axis=12, lwd.ticks=8) 
		
		legend(150,.4, paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,240), ylim=c(min(datTmin$pc2.5[datTmax$class==i]-1),max(datTmin$pc97.5[datTmax$class==i]+1)), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datDZ$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
				datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datDZ$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$pc2.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datDZ$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 	
			datTmin$pc97.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			
			arrows(datDZ$pc2.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 
			datDZ$pc97.5[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]], 	
			datTmin$Mean[datTmin$class==regInClass[[i]]$class[j]&datTmin$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
		mtext("Min Air Temperature (Tmin)", line=20, cex=10, side=2 )
		axis(2, seq(-55,0, by=5), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(0,240, by=50),cex.axis=12, lwd.ticks=8,padj=1) 
		

		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext("Days in Zero mean", outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Days in Zero mean", outer=TRUE, line=-100, cex=10, side=3 )	
	
dev.off()
