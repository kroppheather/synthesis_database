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

#vege class Name
className<-c("herb barren",	"gramminoid tundra", "tussock tundra",	
			"shrub tundra",	"gramminoid wetland", "needleleaf deciduous",
				"needleleaf evergreen", "mixed conifer decidous")

region.name<-unique(data.frame(region=datR$region,name=datR$region_name))

#start by looking at how these parameters plot out

##################################################
####N factor plots by vegetation and region across depth
#make a panel for each vegetation class and show regions in them
#total of 8 panels

{
wb<-40
hb<-40
nlowT=0
nhighT=1.7

jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\nthaw_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

###thawing
#baren
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$depth[datNT$class==1&datNT$region==1], 
			datNT$Mean[datNT$class==1&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$depth[datNT$class==1&datNT$region==5], 
			datNT$Mean[datNT$class==1&datNT$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNT$depth[datNT$class==1&datNT$region==1], 
			datNT$pc2.5[datNT$class==1&datNT$region==1], 
			datNT$depth[datNT$class==1&datNT$region==1], 	
			datNT$pc97.5[datNT$class==1&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$depth[datNT$class==1&datNT$region==5], 
			datNT$pc2.5[datNT$class==1&datNT$region==5], 
			datNT$depth[datNT$class==1&datNT$region==5], 	
			datNT$pc97.5[datNT$class==1&datNT$region==5],lwd=5, code=0)	

axis(2, seq(nlowT,1.5, by=.5), cex.axis=12, lwd.ticks=8, las=2)
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
mtext("Thawing n-factor", side=2, cex=10, line=25 )			
legend(10,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")

box(which="plot")				
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
	points(datNT$depth[datNT$class==2&datNT$region==1], 
			datNT$Mean[datNT$class==2&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$depth[datNT$class==2&datNT$region==2], 
			datNT$Mean[datNT$class==2&datNT$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datNT$depth[datNT$class==2&datNT$region==1], 
			datNT$pc2.5[datNT$class==2&datNT$region==1], 
			datNT$depth[datNT$class==2&datNT$region==1], 	
			datNT$pc97.5[datNT$class==2&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$depth[datNT$class==2&datNT$region==2], 
			datNT$pc2.5[datNT$class==2&datNT$region==2], 
			datNT$depth[datNT$class==2&datNT$region==2], 	
			datNT$pc97.5[datNT$class==2&datNT$region==2],lwd=5, code=0)	
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
box(which="plot")			
#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		points(datNT$depth[datNT$class==3&datNT$region==1], 
			datNT$Mean[datNT$class==3&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$depth[datNT$class==3&datNT$region==1], 
			datNT$pc2.5[datNT$class==3&datNT$region==1], 
			datNT$depth[datNT$class==3&datNT$region==1], 	
			datNT$pc97.5[datNT$class==3&datNT$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
			points(datNT$depth[datNT$class==4&datNT$region==1], 
			datNT$Mean[datNT$class==4&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$depth[datNT$class==4&datNT$region==2], 
			datNT$Mean[datNT$class==4&datNT$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datNT$depth[datNT$class==4&datNT$region==1], 
			datNT$pc2.5[datNT$class==4&datNT$region==1], 
			datNT$depth[datNT$class==4&datNT$region==1], 	
			datNT$pc97.5[datNT$class==4&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$depth[datNT$class==4&datNT$region==2], 
			datNT$pc2.5[datNT$class==4&datNT$region==2], 
			datNT$depth[datNT$class==4&datNT$region==2], 	
			datNT$pc97.5[datNT$class==4&datNT$region==2],lwd=5, code=0)
			
			points(datNT$depth[datNT$class==4&datNT$region==5], 
			datNT$Mean[datNT$class==4&datNT$region==5], pch=19,
				col="dodgerblue4", cex=15)
	points(datNT$depth[datNT$class==4&datNT$region==4], 
			datNT$Mean[datNT$class==4&datNT$region==4], pch=19,
				col="slategray4", cex=15)	
	arrows(datNT$depth[datNT$class==4&datNT$region==4], 
			datNT$pc2.5[datNT$class==4&datNT$region==4], 
			datNT$depth[datNT$class==4&datNT$region==4], 	
			datNT$pc97.5[datNT$class==4&datNT$region==4],lwd=5, code=0)			
	arrows(datNT$depth[datNT$class==4&datNT$region==5], 
			datNT$pc2.5[datNT$class==4&datNT$region==5], 
			datNT$depth[datNT$class==4&datNT$region==5], 	
			datNT$pc97.5[datNT$class==4&datNT$region==5],lwd=5, code=0)				
			
			
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),
					pch=19, cex=10, bty="n")
		
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		points(datNT$depth[datNT$class==5&datNT$region==1], 
			datNT$Mean[datNT$class==5&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$depth[datNT$class==5&datNT$region==5], 
			datNT$Mean[datNT$class==5&datNT$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNT$depth[datNT$class==5&datNT$region==1], 
			datNT$pc2.5[datNT$class==5&datNT$region==1], 
			datNT$depth[datNT$class==5&datNT$region==1], 	
			datNT$pc97.5[datNT$class==5&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$depth[datNT$class==5&datNT$region==5], 
			datNT$pc2.5[datNT$class==5&datNT$region==5], 
			datNT$depth[datNT$class==5&datNT$region==5], 	
			datNT$pc97.5[datNT$class==5&datNT$region==5],lwd=5, code=0)	

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 		
legend(10,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$depth[datNT$class==6&datNT$region==3], 
			datNT$Mean[datNT$class==6&datNT$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datNT$depth[datNT$class==6&datNT$region==3], 
			datNT$pc2.5[datNT$class==6&datNT$region==3], 
			datNT$depth[datNT$class==6&datNT$region==3], 	
			datNT$pc97.5[datNT$class==6&datNT$region==3],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$depth[datNT$class==7&datNT$region==1], 
			datNT$Mean[datNT$class==7&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$depth[datNT$class==7&datNT$region==1], 
			datNT$pc2.5[datNT$class==7&datNT$region==1], 
			datNT$depth[datNT$class==7&datNT$region==1], 	
			datNT$pc97.5[datNT$class==7&datNT$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		
	points(datNT$depth[datNT$class==8&datNT$region==1], 
			datNT$Mean[datNT$class==8&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$depth[datNT$class==8&datNT$region==1], 
			datNT$pc2.5[datNT$class==8&datNT$region==1], 
			datNT$depth[datNT$class==8&datNT$region==1], 	
			datNT$pc97.5[datNT$class==8&datNT$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")			
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$MeanA[datNT$class==1&datNT$region==1], 
			datNT$Mean[datNT$class==1&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$MeanA[datNT$class==1&datNT$region==5], 
			datNT$Mean[datNT$class==1&datNT$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNT$MeanA[datNT$class==1&datNT$region==1], 
			datNT$pc2.5[datNT$class==1&datNT$region==1], 
			datNT$MeanA[datNT$class==1&datNT$region==1], 	
			datNT$pc97.5[datNT$class==1&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$MeanA[datNT$class==1&datNT$region==5], 
			datNT$pc2.5[datNT$class==1&datNT$region==5], 
			datNT$MeanA[datNT$class==1&datNT$region==5], 	
			datNT$pc97.5[datNT$class==1&datNT$region==5],lwd=5, code=0)	
	arrows(datNT$pc2.5A[datNT$class==1&datNT$region==1], 
			datNT$Mean[datNT$class==1&datNT$region==1], 
			datNT$pc97.5A[datNT$class==1&datNT$region==1], 	
			datNT$Mean[datNT$class==1&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$pc2.5A[datNT$class==1&datNT$region==5], 
			datNT$Mean[datNT$class==1&datNT$region==5], 
			datNT$pc97.5A[datNT$class==1&datNT$region==5], 	
			datNT$Mean[datNT$class==1&datNT$region==5],lwd=5, code=0)				
			
			
			
mtext(paste(className[1]), cex=8, line=22, side=1)
axis(2, seq(nlowT,1.5, by=.5), cex.axis=12, las=2, lwd.ticks=8)
axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
mtext("Thawing n-factor", side=2, cex=10, line=25 )			
legend(10,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=12, bty="n")		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$MeanA[datNT$class==2&datNT$region==1], 
			datNT$Mean[datNT$class==2&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$MeanA[datNT$class==2&datNT$region==2], 
			datNT$Mean[datNT$class==2&datNT$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datNT$MeanA[datNT$class==2&datNT$region==1], 
			datNT$pc2.5[datNT$class==2&datNT$region==1], 
			datNT$MeanA[datNT$class==2&datNT$region==1], 	
			datNT$pc97.5[datNT$class==2&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$MeanA[datNT$class==2&datNT$region==2], 
			datNT$pc2.5[datNT$class==2&datNT$region==2], 
			datNT$MeanA[datNT$class==2&datNT$region==2], 	
			datNT$pc97.5[datNT$class==2&datNT$region==2],lwd=5, code=0)	
	arrows(datNT$pc2.5A[datNT$class==2&datNT$region==1], 
			datNT$Mean[datNT$class==2&datNT$region==1], 
			datNT$pc97.5A[datNT$class==2&datNT$region==1], 	
			datNT$Mean[datNT$class==2&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$pc2.5A[datNT$class==2&datNT$region==2], 
			datNT$Mean[datNT$class==2&datNT$region==2], 
			datNT$pc97.5A[datNT$class==2&datNT$region==2], 	
			datNT$Mean[datNT$class==2&datNT$region==2],lwd=5, code=0)	
axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,1.7, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=12, bty="n")
mtext(paste(className[2]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$MeanA[datNT$class==3&datNT$region==1], 
			datNT$Mean[datNT$class==3&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$MeanA[datNT$class==3&datNT$region==1], 
			datNT$pc2.5[datNT$class==3&datNT$region==1], 
			datNT$MeanA[datNT$class==3&datNT$region==1], 	
			datNT$pc97.5[datNT$class==3&datNT$region==1],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==3&datNT$region==1], 
			datNT$Mean[datNT$class==3&datNT$region==1], 
			datNT$pc97.5A[datNT$class==3&datNT$region==1], 	
			datNT$Mean[datNT$class==3&datNT$region==1],lwd=5, code=0)			

axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=12, bty="n")	
		
mtext(paste(className[3]), cex=8, line=22, side=1)		
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			
	points(datNT$MeanA[datNT$class==4&datNT$region==1], 
			datNT$Mean[datNT$class==4&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$MeanA[datNT$class==4&datNT$region==1], 
			datNT$pc2.5[datNT$class==4&datNT$region==1], 
			datNT$MeanA[datNT$class==4&datNT$region==1], 	
			datNT$pc97.5[datNT$class==4&datNT$region==1],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==4&datNT$region==1], 
			datNT$Mean[datNT$class==4&datNT$region==1], 
			datNT$pc97.5A[datNT$class==4&datNT$region==1], 	
			datNT$Mean[datNT$class==4&datNT$region==1],lwd=5, code=0)		
		
		points(datNT$MeanA[datNT$class==4&datNT$region==2], 
			datNT$Mean[datNT$class==4&datNT$region==2], pch=19,
				col="tomato3", cex=15)

	arrows(datNT$MeanA[datNT$class==4&datNT$region==2], 
			datNT$pc2.5[datNT$class==4&datNT$region==2], 
			datNT$MeanA[datNT$class==4&datNT$region==2], 	
			datNT$pc97.5[datNT$class==4&datNT$region==2],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==4&datNT$region==2], 
			datNT$Mean[datNT$class==4&datNT$region==2], 
			datNT$pc97.5A[datNT$class==4&datNT$region==2], 	
			datNT$Mean[datNT$class==4&datNT$region==2],lwd=5, code=0)
			
		points(datNT$MeanA[datNT$class==4&datNT$region==4], 
			datNT$Mean[datNT$class==4&datNT$region==4], pch=19,
				col="slategray4", cex=15)

	arrows(datNT$MeanA[datNT$class==4&datNT$region==4], 
			datNT$pc2.5[datNT$class==4&datNT$region==4], 
			datNT$MeanA[datNT$class==4&datNT$region==4], 	
			datNT$pc97.5[datNT$class==4&datNT$region==4],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==4&datNT$region==4], 
			datNT$Mean[datNT$class==4&datNT$region==4], 
			datNT$pc97.5A[datNT$class==4&datNT$region==4], 	
			datNT$Mean[datNT$class==4&datNT$region==4],lwd=5, code=0)
			
		points(datNT$MeanA[datNT$class==4&datNT$region==5], 
			datNT$Mean[datNT$class==4&datNT$region==5], pch=19,
				col="dodgerblue4", cex=15)

	arrows(datNT$MeanA[datNT$class==4&datNT$region==5], 
			datNT$pc2.5[datNT$class==4&datNT$region==5], 
			datNT$MeanA[datNT$class==4&datNT$region==5], 	
			datNT$pc97.5[datNT$class==4&datNT$region==5],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==4&datNT$region==5], 
			datNT$Mean[datNT$class==4&datNT$region==5], 
			datNT$pc97.5A[datNT$class==4&datNT$region==5], 	
			datNT$Mean[datNT$class==4&datNT$region==5],lwd=5, code=0)				
axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(16,1.7, 		c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),pch=19,bty="n", cex=12)
		
mtext(paste(className[4]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datNT$MeanA[datNT$class==5&datNT$region==1], 
			datNT$Mean[datNT$class==5&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNT$MeanA[datNT$class==5&datNT$region==5], 
			datNT$Mean[datNT$class==5&datNT$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNT$MeanA[datNT$class==5&datNT$region==1], 
			datNT$pc2.5[datNT$class==5&datNT$region==1], 
			datNT$MeanA[datNT$class==5&datNT$region==1], 	
			datNT$pc97.5[datNT$class==5&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$MeanA[datNT$class==5&datNT$region==5], 
			datNT$pc2.5[datNT$class==5&datNT$region==5], 
			datNT$MeanA[datNT$class==5&datNT$region==5], 	
			datNT$pc97.5[datNT$class==5&datNT$region==5],lwd=5, code=0)	
	arrows(datNT$pc2.5A[datNT$class==5&datNT$region==1], 
			datNT$Mean[datNT$class==5&datNT$region==1], 
			datNT$pc97.5A[datNT$class==5&datNT$region==1], 	
			datNT$Mean[datNT$class==5&datNT$region==1],lwd=5, code=0)			
	arrows(datNT$pc2.5A[datNT$class==5&datNT$region==5], 
			datNT$Mean[datNT$class==5&datNT$region==5], 
			datNT$pc97.5A[datNT$class==5&datNT$region==5], 	
			datNT$Mean[datNT$class==5&datNT$region==5],lwd=5, code=0)				
			
axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
	
legend(10,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=12, bty="n")	
mtext(paste(className[5]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNT$MeanA[datNT$class==6&datNT$region==3], 
			datNT$Mean[datNT$class==6&datNT$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datNT$MeanA[datNT$class==6&datNT$region==3], 
			datNT$pc2.5[datNT$class==6&datNT$region==3], 
			datNT$MeanA[datNT$class==6&datNT$region==3], 	
			datNT$pc97.5[datNT$class==6&datNT$region==3],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==6&datNT$region==3], 
			datNT$Mean[datNT$class==6&datNT$region==3], 
			datNT$pc97.5A[datNT$class==6&datNT$region==3], 	
			datNT$Mean[datNT$class==6&datNT$region==3],lwd=5, code=0)			

axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,1.7, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=12, bty="n")	
		
mtext(paste(className[6]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		points(datNT$MeanA[datNT$class==7&datNT$region==1], 
			datNT$Mean[datNT$class==7&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$MeanA[datNT$class==7&datNT$region==1], 
			datNT$pc2.5[datNT$class==7&datNT$region==1], 
			datNT$MeanA[datNT$class==7&datNT$region==1], 	
			datNT$pc97.5[datNT$class==7&datNT$region==1],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==7&datNT$region==1], 
			datNT$Mean[datNT$class==7&datNT$region==1], 
			datNT$pc97.5A[datNT$class==7&datNT$region==1], 	
			datNT$Mean[datNT$class==7&datNT$region==1],lwd=5, code=0)			

axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=12, bty="n")	

mtext(paste(className[7]), cex=8, line=22, side=1)
					
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,30), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")

		points(datNT$MeanA[datNT$class==8&datNT$region==1], 
			datNT$Mean[datNT$class==8&datNT$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNT$MeanA[datNT$class==8&datNT$region==1], 
			datNT$pc2.5[datNT$class==8&datNT$region==1], 
			datNT$MeanA[datNT$class==8&datNT$region==1], 	
			datNT$pc97.5[datNT$class==8&datNT$region==1],lwd=5, code=0)			

	arrows(datNT$pc2.5A[datNT$class==8&datNT$region==1], 
			datNT$Mean[datNT$class==8&datNT$region==1], 
			datNT$pc97.5A[datNT$class==8&datNT$region==1], 	
			datNT$Mean[datNT$class==8&datNT$region==1],lwd=5, code=0)			

axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=12, bty="n")	
mtext(paste(className[8]), cex=8, line=22, side=1)			
mtext("Maximum Air Temperature (Tmax, C)", outer=TRUE, line=-100, cex=10, side=1 )		
mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )		
dev.off()

}

###Freezing


{
wb<-40
hb<-40
nlowT=0
nhighT=1.7

jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\nfreeze_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

###thawing
#baren
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$depth[datNF$class==1&datNF$region==1], 
			datNF$Mean[datNF$class==1&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$depth[datNF$class==1&datNF$region==5], 
			datNF$Mean[datNF$class==1&datNF$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNF$depth[datNF$class==1&datNF$region==1], 
			datNF$pc2.5[datNF$class==1&datNF$region==1], 
			datNF$depth[datNF$class==1&datNF$region==1], 	
			datNF$pc97.5[datNF$class==1&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$depth[datNF$class==1&datNF$region==5], 
			datNF$pc2.5[datNF$class==1&datNF$region==5], 
			datNF$depth[datNF$class==1&datNF$region==5], 	
			datNF$pc97.5[datNF$class==1&datNF$region==5],lwd=5, code=0)	

axis(2, seq(nlowT,1.5, by=.5), cex.axis=12, lwd.ticks=8, las=2)
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
mtext("Thawing n-factor", side=2, cex=10, line=25 )			
legend(10,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")

box(which="plot")				
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
	points(datNF$depth[datNF$class==2&datNF$region==1], 
			datNF$Mean[datNF$class==2&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$depth[datNF$class==2&datNF$region==2], 
			datNF$Mean[datNF$class==2&datNF$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datNF$depth[datNF$class==2&datNF$region==1], 
			datNF$pc2.5[datNF$class==2&datNF$region==1], 
			datNF$depth[datNF$class==2&datNF$region==1], 	
			datNF$pc97.5[datNF$class==2&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$depth[datNF$class==2&datNF$region==2], 
			datNF$pc2.5[datNF$class==2&datNF$region==2], 
			datNF$depth[datNF$class==2&datNF$region==2], 	
			datNF$pc97.5[datNF$class==2&datNF$region==2],lwd=5, code=0)	
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
box(which="plot")			
#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		points(datNF$depth[datNF$class==3&datNF$region==1], 
			datNF$Mean[datNF$class==3&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$depth[datNF$class==3&datNF$region==1], 
			datNF$pc2.5[datNF$class==3&datNF$region==1], 
			datNF$depth[datNF$class==3&datNF$region==1], 	
			datNF$pc97.5[datNF$class==3&datNF$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
			points(datNF$depth[datNF$class==4&datNF$region==1], 
			datNF$Mean[datNF$class==4&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$depth[datNF$class==4&datNF$region==2], 
			datNF$Mean[datNF$class==4&datNF$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datNF$depth[datNF$class==4&datNF$region==1], 
			datNF$pc2.5[datNF$class==4&datNF$region==1], 
			datNF$depth[datNF$class==4&datNF$region==1], 	
			datNF$pc97.5[datNF$class==4&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$depth[datNF$class==4&datNF$region==2], 
			datNF$pc2.5[datNF$class==4&datNF$region==2], 
			datNF$depth[datNF$class==4&datNF$region==2], 	
			datNF$pc97.5[datNF$class==4&datNF$region==2],lwd=5, code=0)
			
			points(datNF$depth[datNF$class==4&datNF$region==5], 
			datNF$Mean[datNF$class==4&datNF$region==5], pch=19,
				col="dodgerblue4", cex=15)
	points(datNF$depth[datNF$class==4&datNF$region==4], 
			datNF$Mean[datNF$class==4&datNF$region==4], pch=19,
				col="slategray4", cex=15)	
	arrows(datNF$depth[datNF$class==4&datNF$region==4], 
			datNF$pc2.5[datNF$class==4&datNF$region==4], 
			datNF$depth[datNF$class==4&datNF$region==4], 	
			datNF$pc97.5[datNF$class==4&datNF$region==4],lwd=5, code=0)			
	arrows(datNF$depth[datNF$class==4&datNF$region==5], 
			datNF$pc2.5[datNF$class==4&datNF$region==5], 
			datNF$depth[datNF$class==4&datNF$region==5], 	
			datNF$pc97.5[datNF$class==4&datNF$region==5],lwd=5, code=0)				
			
			
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),
					pch=19, cex=10, bty="n")
		
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		points(datNF$depth[datNF$class==5&datNF$region==1], 
			datNF$Mean[datNF$class==5&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$depth[datNF$class==5&datNF$region==5], 
			datNF$Mean[datNF$class==5&datNF$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNF$depth[datNF$class==5&datNF$region==1], 
			datNF$pc2.5[datNF$class==5&datNF$region==1], 
			datNF$depth[datNF$class==5&datNF$region==1], 	
			datNF$pc97.5[datNF$class==5&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$depth[datNF$class==5&datNF$region==5], 
			datNF$pc2.5[datNF$class==5&datNF$region==5], 
			datNF$depth[datNF$class==5&datNF$region==5], 	
			datNF$pc97.5[datNF$class==5&datNF$region==5],lwd=5, code=0)	

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 		
legend(10,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$depth[datNF$class==6&datNF$region==3], 
			datNF$Mean[datNF$class==6&datNF$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datNF$depth[datNF$class==6&datNF$region==3], 
			datNF$pc2.5[datNF$class==6&datNF$region==3], 
			datNF$depth[datNF$class==6&datNF$region==3], 	
			datNF$pc97.5[datNF$class==6&datNF$region==3],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$depth[datNF$class==7&datNF$region==1], 
			datNF$Mean[datNF$class==7&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$depth[datNF$class==7&datNF$region==1], 
			datNF$pc2.5[datNF$class==7&datNF$region==1], 
			datNF$depth[datNF$class==7&datNF$region==1], 	
			datNF$pc97.5[datNF$class==7&datNF$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		
	points(datNF$depth[datNF$class==8&datNF$region==1], 
			datNF$Mean[datNF$class==8&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$depth[datNF$class==8&datNF$region==1], 
			datNF$pc2.5[datNF$class==8&datNF$region==1], 
			datNF$depth[datNF$class==8&datNF$region==1], 	
			datNF$pc97.5[datNF$class==8&datNF$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")			
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$MeanA[datNF$class==1&datNF$region==1], 
			datNF$Mean[datNF$class==1&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$MeanA[datNF$class==1&datNF$region==5], 
			datNF$Mean[datNF$class==1&datNF$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNF$MeanA[datNF$class==1&datNF$region==1], 
			datNF$pc2.5[datNF$class==1&datNF$region==1], 
			datNF$MeanA[datNF$class==1&datNF$region==1], 	
			datNF$pc97.5[datNF$class==1&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$MeanA[datNF$class==1&datNF$region==5], 
			datNF$pc2.5[datNF$class==1&datNF$region==5], 
			datNF$MeanA[datNF$class==1&datNF$region==5], 	
			datNF$pc97.5[datNF$class==1&datNF$region==5],lwd=5, code=0)	
	arrows(datNF$pc2.5A[datNF$class==1&datNF$region==1], 
			datNF$Mean[datNF$class==1&datNF$region==1], 
			datNF$pc97.5A[datNF$class==1&datNF$region==1], 	
			datNF$Mean[datNF$class==1&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$pc2.5A[datNF$class==1&datNF$region==5], 
			datNF$Mean[datNF$class==1&datNF$region==5], 
			datNF$pc97.5A[datNF$class==1&datNF$region==5], 	
			datNF$Mean[datNF$class==1&datNF$region==5],lwd=5, code=0)				
			
			
			
mtext(paste(className[1]), cex=8, line=22, side=1)
axis(2, seq(nlowT,1.5, by=.5), cex.axis=12, las=2, lwd.ticks=8)
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
mtext("Thawing n-factor", side=2, cex=10, line=25 )			
legend(-20,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$MeanA[datNF$class==2&datNF$region==1], 
			datNF$Mean[datNF$class==2&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$MeanA[datNF$class==2&datNF$region==2], 
			datNF$Mean[datNF$class==2&datNF$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datNF$MeanA[datNF$class==2&datNF$region==1], 
			datNF$pc2.5[datNF$class==2&datNF$region==1], 
			datNF$MeanA[datNF$class==2&datNF$region==1], 	
			datNF$pc97.5[datNF$class==2&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$MeanA[datNF$class==2&datNF$region==2], 
			datNF$pc2.5[datNF$class==2&datNF$region==2], 
			datNF$MeanA[datNF$class==2&datNF$region==2], 	
			datNF$pc97.5[datNF$class==2&datNF$region==2],lwd=5, code=0)	
	arrows(datNF$pc2.5A[datNF$class==2&datNF$region==1], 
			datNF$Mean[datNF$class==2&datNF$region==1], 
			datNF$pc97.5A[datNF$class==2&datNF$region==1], 	
			datNF$Mean[datNF$class==2&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$pc2.5A[datNF$class==2&datNF$region==2], 
			datNF$Mean[datNF$class==2&datNF$region==2], 
			datNF$pc97.5A[datNF$class==2&datNF$region==2], 	
			datNF$Mean[datNF$class==2&datNF$region==2],lwd=5, code=0)	
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,1.7, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
mtext(paste(className[2]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$MeanA[datNF$class==3&datNF$region==1], 
			datNF$Mean[datNF$class==3&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$MeanA[datNF$class==3&datNF$region==1], 
			datNF$pc2.5[datNF$class==3&datNF$region==1], 
			datNF$MeanA[datNF$class==3&datNF$region==1], 	
			datNF$pc97.5[datNF$class==3&datNF$region==1],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==3&datNF$region==1], 
			datNF$Mean[datNF$class==3&datNF$region==1], 
			datNF$pc97.5A[datNF$class==3&datNF$region==1], 	
			datNF$Mean[datNF$class==3&datNF$region==1],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[3]), cex=8, line=22, side=1)		
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			
	points(datNF$MeanA[datNF$class==4&datNF$region==1], 
			datNF$Mean[datNF$class==4&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$MeanA[datNF$class==4&datNF$region==1], 
			datNF$pc2.5[datNF$class==4&datNF$region==1], 
			datNF$MeanA[datNF$class==4&datNF$region==1], 	
			datNF$pc97.5[datNF$class==4&datNF$region==1],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==4&datNF$region==1], 
			datNF$Mean[datNF$class==4&datNF$region==1], 
			datNF$pc97.5A[datNF$class==4&datNF$region==1], 	
			datNF$Mean[datNF$class==4&datNF$region==1],lwd=5, code=0)		
		
		points(datNF$MeanA[datNF$class==4&datNF$region==2], 
			datNF$Mean[datNF$class==4&datNF$region==2], pch=19,
				col="tomato3", cex=15)

	arrows(datNF$MeanA[datNF$class==4&datNF$region==2], 
			datNF$pc2.5[datNF$class==4&datNF$region==2], 
			datNF$MeanA[datNF$class==4&datNF$region==2], 	
			datNF$pc97.5[datNF$class==4&datNF$region==2],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==4&datNF$region==2], 
			datNF$Mean[datNF$class==4&datNF$region==2], 
			datNF$pc97.5A[datNF$class==4&datNF$region==2], 	
			datNF$Mean[datNF$class==4&datNF$region==2],lwd=5, code=0)
			
		points(datNF$MeanA[datNF$class==4&datNF$region==4], 
			datNF$Mean[datNF$class==4&datNF$region==4], pch=19,
				col="slategray4", cex=15)

	arrows(datNF$MeanA[datNF$class==4&datNF$region==4], 
			datNF$pc2.5[datNF$class==4&datNF$region==4], 
			datNF$MeanA[datNF$class==4&datNF$region==4], 	
			datNF$pc97.5[datNF$class==4&datNF$region==4],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==4&datNF$region==4], 
			datNF$Mean[datNF$class==4&datNF$region==4], 
			datNF$pc97.5A[datNF$class==4&datNF$region==4], 	
			datNF$Mean[datNF$class==4&datNF$region==4],lwd=5, code=0)
			
		points(datNF$MeanA[datNF$class==4&datNF$region==5], 
			datNF$Mean[datNF$class==4&datNF$region==5], pch=19,
				col="dodgerblue4", cex=15)

	arrows(datNF$MeanA[datNF$class==4&datNF$region==5], 
			datNF$pc2.5[datNF$class==4&datNF$region==5], 
			datNF$MeanA[datNF$class==4&datNF$region==5], 	
			datNF$pc97.5[datNF$class==4&datNF$region==5],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==4&datNF$region==5], 
			datNF$Mean[datNF$class==4&datNF$region==5], 
			datNF$pc97.5A[datNF$class==4&datNF$region==5], 	
			datNF$Mean[datNF$class==4&datNF$region==5],lwd=5, code=0)				
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-16,1.7, 		c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),pch=19,bty="n", cex=10)
		
mtext(paste(className[4]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datNF$MeanA[datNF$class==5&datNF$region==1], 
			datNF$Mean[datNF$class==5&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datNF$MeanA[datNF$class==5&datNF$region==5], 
			datNF$Mean[datNF$class==5&datNF$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datNF$MeanA[datNF$class==5&datNF$region==1], 
			datNF$pc2.5[datNF$class==5&datNF$region==1], 
			datNF$MeanA[datNF$class==5&datNF$region==1], 	
			datNF$pc97.5[datNF$class==5&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$MeanA[datNF$class==5&datNF$region==5], 
			datNF$pc2.5[datNF$class==5&datNF$region==5], 
			datNF$MeanA[datNF$class==5&datNF$region==5], 	
			datNF$pc97.5[datNF$class==5&datNF$region==5],lwd=5, code=0)	
	arrows(datNF$pc2.5A[datNF$class==5&datNF$region==1], 
			datNF$Mean[datNF$class==5&datNF$region==1], 
			datNF$pc97.5A[datNF$class==5&datNF$region==1], 	
			datNF$Mean[datNF$class==5&datNF$region==1],lwd=5, code=0)			
	arrows(datNF$pc2.5A[datNF$class==5&datNF$region==5], 
			datNF$Mean[datNF$class==5&datNF$region==5], 
			datNF$pc97.5A[datNF$class==5&datNF$region==5], 	
			datNF$Mean[datNF$class==5&datNF$region==5],lwd=5, code=0)				
			
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
	
legend(-20,1.7, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[5]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datNF$MeanA[datNF$class==6&datNF$region==3], 
			datNF$Mean[datNF$class==6&datNF$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datNF$MeanA[datNF$class==6&datNF$region==3], 
			datNF$pc2.5[datNF$class==6&datNF$region==3], 
			datNF$MeanA[datNF$class==6&datNF$region==3], 	
			datNF$pc97.5[datNF$class==6&datNF$region==3],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==6&datNF$region==3], 
			datNF$Mean[datNF$class==6&datNF$region==3], 
			datNF$pc97.5A[datNF$class==6&datNF$region==3], 	
			datNF$Mean[datNF$class==6&datNF$region==3],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,1.7, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[6]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		points(datNF$MeanA[datNF$class==7&datNF$region==1], 
			datNF$Mean[datNF$class==7&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$MeanA[datNF$class==7&datNF$region==1], 
			datNF$pc2.5[datNF$class==7&datNF$region==1], 
			datNF$MeanA[datNF$class==7&datNF$region==1], 	
			datNF$pc97.5[datNF$class==7&datNF$region==1],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==7&datNF$region==1], 
			datNF$Mean[datNF$class==7&datNF$region==1], 
			datNF$pc97.5A[datNF$class==7&datNF$region==1], 	
			datNF$Mean[datNF$class==7&datNF$region==1],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	

mtext(paste(className[7]), cex=8, line=22, side=1)
					
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-40,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")

		points(datNF$MeanA[datNF$class==8&datNF$region==1], 
			datNF$Mean[datNF$class==8&datNF$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datNF$MeanA[datNF$class==8&datNF$region==1], 
			datNF$pc2.5[datNF$class==8&datNF$region==1], 
			datNF$MeanA[datNF$class==8&datNF$region==1], 	
			datNF$pc97.5[datNF$class==8&datNF$region==1],lwd=5, code=0)			

	arrows(datNF$pc2.5A[datNF$class==8&datNF$region==1], 
			datNF$Mean[datNF$class==8&datNF$region==1], 
			datNF$pc97.5A[datNF$class==8&datNF$region==1], 	
			datNF$Mean[datNF$class==8&datNF$region==1],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,1.7, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[8]), cex=8, line=22, side=1)			
mtext("Minimum Air Temperature (Tmax, C)", outer=TRUE, line=-100, cex=10, side=1 )		
mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )		
dev.off()

}

##Minimum temperature
{
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

###thawing
#baren
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$depth[datTmin$class==1&datTmin$region==1], 
			datTmin$Mean[datTmin$class==1&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$depth[datTmin$class==1&datTmin$region==5], 
			datTmin$Mean[datTmin$class==1&datTmin$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmin$depth[datTmin$class==1&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==1&datTmin$region==1], 
			datTmin$depth[datTmin$class==1&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==1&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$depth[datTmin$class==1&datTmin$region==5], 
			datTmin$pc2.5[datTmin$class==1&datTmin$region==5], 
			datTmin$depth[datTmin$class==1&datTmin$region==5], 	
			datTmin$pc97.5[datTmin$class==1&datTmin$region==5],lwd=5, code=0)	

axis(2, seq(-40,0, by=5), cex.axis=12, lwd.ticks=8, las=2)
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
mtext("Thawing n-factor", side=2, cex=10, line=25 )			
legend(10,-30, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")

box(which="plot")				
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
	points(datTmin$depth[datTmin$class==2&datTmin$region==1], 
			datTmin$Mean[datTmin$class==2&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$depth[datTmin$class==2&datTmin$region==2], 
			datTmin$Mean[datTmin$class==2&datTmin$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datTmin$depth[datTmin$class==2&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==2&datTmin$region==1], 
			datTmin$depth[datTmin$class==2&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==2&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$depth[datTmin$class==2&datTmin$region==2], 
			datTmin$pc2.5[datTmin$class==2&datTmin$region==2], 
			datTmin$depth[datTmin$class==2&datTmin$region==2], 	
			datTmin$pc97.5[datTmin$class==2&datTmin$region==2],lwd=5, code=0)	
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,-30, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
box(which="plot")			
#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		points(datTmin$depth[datTmin$class==3&datTmin$region==1], 
			datTmin$Mean[datTmin$class==3&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$depth[datTmin$class==3&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==3&datTmin$region==1], 
			datTmin$depth[datTmin$class==3&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==3&datTmin$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,-30, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
			points(datTmin$depth[datTmin$class==4&datTmin$region==1], 
			datTmin$Mean[datTmin$class==4&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$depth[datTmin$class==4&datTmin$region==2], 
			datTmin$Mean[datTmin$class==4&datTmin$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datTmin$depth[datTmin$class==4&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==1], 
			datTmin$depth[datTmin$class==4&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$depth[datTmin$class==4&datTmin$region==2], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==2], 
			datTmin$depth[datTmin$class==4&datTmin$region==2], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==2],lwd=5, code=0)
			
			points(datTmin$depth[datTmin$class==4&datTmin$region==5], 
			datTmin$Mean[datTmin$class==4&datTmin$region==5], pch=19,
				col="dodgerblue4", cex=15)
	points(datTmin$depth[datTmin$class==4&datTmin$region==4], 
			datTmin$Mean[datTmin$class==4&datTmin$region==4], pch=19,
				col="slategray4", cex=15)	
	arrows(datTmin$depth[datTmin$class==4&datTmin$region==4], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==4], 
			datTmin$depth[datTmin$class==4&datTmin$region==4], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==4],lwd=5, code=0)			
	arrows(datTmin$depth[datTmin$class==4&datTmin$region==5], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==5], 
			datTmin$depth[datTmin$class==4&datTmin$region==5], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==5],lwd=5, code=0)				
			
			
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,-20, c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),
					pch=19, cex=10, bty="n")
		
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		points(datTmin$depth[datTmin$class==5&datTmin$region==1], 
			datTmin$Mean[datTmin$class==5&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$depth[datTmin$class==5&datTmin$region==5], 
			datTmin$Mean[datTmin$class==5&datTmin$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmin$depth[datTmin$class==5&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==5&datTmin$region==1], 
			datTmin$depth[datTmin$class==5&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==5&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$depth[datTmin$class==5&datTmin$region==5], 
			datTmin$pc2.5[datTmin$class==5&datTmin$region==5], 
			datTmin$depth[datTmin$class==5&datTmin$region==5], 	
			datTmin$pc97.5[datTmin$class==5&datTmin$region==5],lwd=5, code=0)	

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 		
legend(10,-30, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$depth[datTmin$class==6&datTmin$region==3], 
			datTmin$Mean[datTmin$class==6&datTmin$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datTmin$depth[datTmin$class==6&datTmin$region==3], 
			datTmin$pc2.5[datTmin$class==6&datTmin$region==3], 
			datTmin$depth[datTmin$class==6&datTmin$region==3], 	
			datTmin$pc97.5[datTmin$class==6&datTmin$region==3],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,-30, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$depth[datTmin$class==7&datTmin$region==1], 
			datTmin$Mean[datTmin$class==7&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$depth[datTmin$class==7&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==7&datTmin$region==1], 
			datTmin$depth[datTmin$class==7&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==7&datTmin$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,-30, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		
	points(datTmin$depth[datTmin$class==8&datTmin$region==1], 
			datTmin$Mean[datTmin$class==8&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$depth[datTmin$class==8&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==8&datTmin$region==1], 
			datTmin$depth[datTmin$class==8&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==8&datTmin$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend10,-30, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")			
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$MeanA[datTmin$class==1&datTmin$region==1], 
			datTmin$Mean[datTmin$class==1&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$MeanA[datTmin$class==1&datTmin$region==5], 
			datTmin$Mean[datTmin$class==1&datTmin$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmin$MeanA[datTmin$class==1&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==1&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==1&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==1&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$MeanA[datTmin$class==1&datTmin$region==5], 
			datTmin$pc2.5[datTmin$class==1&datTmin$region==5], 
			datTmin$MeanA[datTmin$class==1&datTmin$region==5], 	
			datTmin$pc97.5[datTmin$class==1&datTmin$region==5],lwd=5, code=0)	
	arrows(datTmin$pc2.5A[datTmin$class==1&datTmin$region==1], 
			datTmin$Mean[datTmin$class==1&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==1&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==1&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$pc2.5A[datTmin$class==1&datTmin$region==5], 
			datTmin$Mean[datTmin$class==1&datTmin$region==5], 
			datTmin$pc97.5A[datTmin$class==1&datTmin$region==5], 	
			datTmin$Mean[datTmin$class==1&datTmin$region==5],lwd=5, code=0)				
			
			
			
mtext(paste(className[1]), cex=8, line=22, side=1)
axis(2, seq(-40,0, by=5), cex.axis=12, las=2, lwd.ticks=8)
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
mtext("Thawing n-factor", side=2, cex=10, line=25 )			
legend(-20,-30, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$MeanA[datTmin$class==2&datTmin$region==1], 
			datTmin$Mean[datTmin$class==2&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$MeanA[datTmin$class==2&datTmin$region==2], 
			datTmin$Mean[datTmin$class==2&datTmin$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datTmin$MeanA[datTmin$class==2&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==2&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==2&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==2&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$MeanA[datTmin$class==2&datTmin$region==2], 
			datTmin$pc2.5[datTmin$class==2&datTmin$region==2], 
			datTmin$MeanA[datTmin$class==2&datTmin$region==2], 	
			datTmin$pc97.5[datTmin$class==2&datTmin$region==2],lwd=5, code=0)	
	arrows(datTmin$pc2.5A[datTmin$class==2&datTmin$region==1], 
			datTmin$Mean[datTmin$class==2&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==2&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==2&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$pc2.5A[datTmin$class==2&datTmin$region==2], 
			datTmin$Mean[datTmin$class==2&datTmin$region==2], 
			datTmin$pc97.5A[datTmin$class==2&datTmin$region==2], 	
			datTmin$Mean[datTmin$class==2&datTmin$region==2],lwd=5, code=0)	
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,-30, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
mtext(paste(className[2]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$MeanA[datTmin$class==3&datTmin$region==1], 
			datTmin$Mean[datTmin$class==3&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$MeanA[datTmin$class==3&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==3&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==3&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==3&datTmin$region==1],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==3&datTmin$region==1], 
			datTmin$Mean[datTmin$class==3&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==3&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==3&datTmin$region==1],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,-30, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[3]), cex=8, line=22, side=1)		
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			
	points(datTmin$MeanA[datTmin$class==4&datTmin$region==1], 
			datTmin$Mean[datTmin$class==4&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$MeanA[datTmin$class==4&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==4&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==1],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==4&datTmin$region==1], 
			datTmin$Mean[datTmin$class==4&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==4&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==4&datTmin$region==1],lwd=5, code=0)		
		
		points(datTmin$MeanA[datTmin$class==4&datTmin$region==2], 
			datTmin$Mean[datTmin$class==4&datTmin$region==2], pch=19,
				col="tomato3", cex=15)

	arrows(datTmin$MeanA[datTmin$class==4&datTmin$region==2], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==2], 
			datTmin$MeanA[datTmin$class==4&datTmin$region==2], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==2],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==4&datTmin$region==2], 
			datTmin$Mean[datTmin$class==4&datTmin$region==2], 
			datTmin$pc97.5A[datTmin$class==4&datTmin$region==2], 	
			datTmin$Mean[datTmin$class==4&datTmin$region==2],lwd=5, code=0)
			
		points(datTmin$MeanA[datTmin$class==4&datTmin$region==4], 
			datTmin$Mean[datTmin$class==4&datTmin$region==4], pch=19,
				col="slategray4", cex=15)

	arrows(datTmin$MeanA[datTmin$class==4&datTmin$region==4], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==4], 
			datTmin$MeanA[datTmin$class==4&datTmin$region==4], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==4],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==4&datTmin$region==4], 
			datTmin$Mean[datTmin$class==4&datTmin$region==4], 
			datTmin$pc97.5A[datTmin$class==4&datTmin$region==4], 	
			datTmin$Mean[datTmin$class==4&datTmin$region==4],lwd=5, code=0)
			
		points(datTmin$MeanA[datTmin$class==4&datTmin$region==5], 
			datTmin$Mean[datTmin$class==4&datTmin$region==5], pch=19,
				col="dodgerblue4", cex=15)

	arrows(datTmin$MeanA[datTmin$class==4&datTmin$region==5], 
			datTmin$pc2.5[datTmin$class==4&datTmin$region==5], 
			datTmin$MeanA[datTmin$class==4&datTmin$region==5], 	
			datTmin$pc97.5[datTmin$class==4&datTmin$region==5],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==4&datTmin$region==5], 
			datTmin$Mean[datTmin$class==4&datTmin$region==5], 
			datTmin$pc97.5A[datTmin$class==4&datTmin$region==5], 	
			datTmin$Mean[datTmin$class==4&datTmin$region==5],lwd=5, code=0)				
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,-20, 		c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),pch=19,bty="n", cex=10)
		
mtext(paste(className[4]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datTmin$MeanA[datTmin$class==5&datTmin$region==1], 
			datTmin$Mean[datTmin$class==5&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmin$MeanA[datTmin$class==5&datTmin$region==5], 
			datTmin$Mean[datTmin$class==5&datTmin$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmin$MeanA[datTmin$class==5&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==5&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==5&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==5&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$MeanA[datTmin$class==5&datTmin$region==5], 
			datTmin$pc2.5[datTmin$class==5&datTmin$region==5], 
			datTmin$MeanA[datTmin$class==5&datTmin$region==5], 	
			datTmin$pc97.5[datTmin$class==5&datTmin$region==5],lwd=5, code=0)	
	arrows(datTmin$pc2.5A[datTmin$class==5&datTmin$region==1], 
			datTmin$Mean[datTmin$class==5&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==5&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==5&datTmin$region==1],lwd=5, code=0)			
	arrows(datTmin$pc2.5A[datTmin$class==5&datTmin$region==5], 
			datTmin$Mean[datTmin$class==5&datTmin$region==5], 
			datTmin$pc97.5A[datTmin$class==5&datTmin$region==5], 	
			datTmin$Mean[datTmin$class==5&datTmin$region==5],lwd=5, code=0)				
			
axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
	
legend(-20,-30, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[5]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmin$MeanA[datTmin$class==6&datTmin$region==3], 
			datTmin$Mean[datTmin$class==6&datTmin$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datTmin$MeanA[datTmin$class==6&datTmin$region==3], 
			datTmin$pc2.5[datTmin$class==6&datTmin$region==3], 
			datTmin$MeanA[datTmin$class==6&datTmin$region==3], 	
			datTmin$pc97.5[datTmin$class==6&datTmin$region==3],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==6&datTmin$region==3], 
			datTmin$Mean[datTmin$class==6&datTmin$region==3], 
			datTmin$pc97.5A[datTmin$class==6&datTmin$region==3], 	
			datTmin$Mean[datTmin$class==6&datTmin$region==3],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,-30, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[6]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-45,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		points(datTmin$MeanA[datTmin$class==7&datTmin$region==1], 
			datTmin$Mean[datTmin$class==7&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$MeanA[datTmin$class==7&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==7&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==7&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==7&datTmin$region==1],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==7&datTmin$region==1], 
			datTmin$Mean[datTmin$class==7&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==7&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==7&datTmin$region==1],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,-30, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	

mtext(paste(className[7]), cex=8, line=22, side=1)
					
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-40,0), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")

		points(datTmin$MeanA[datTmin$class==8&datTmin$region==1], 
			datTmin$Mean[datTmin$class==8&datTmin$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmin$MeanA[datTmin$class==8&datTmin$region==1], 
			datTmin$pc2.5[datTmin$class==8&datTmin$region==1], 
			datTmin$MeanA[datTmin$class==8&datTmin$region==1], 	
			datTmin$pc97.5[datTmin$class==8&datTmin$region==1],lwd=5, code=0)			

	arrows(datTmin$pc2.5A[datTmin$class==8&datTmin$region==1], 
			datTmin$Mean[datTmin$class==8&datTmin$region==1], 
			datTmin$pc97.5A[datTmin$class==8&datTmin$region==1], 	
			datTmin$Mean[datTmin$class==8&datTmin$region==1],lwd=5, code=0)			

axis(1, seq(-40,0, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(-20,-30, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[8]), cex=8, line=22, side=1)			
mtext("Minimum Air Temperature (Tmax, C)", outer=TRUE, line=-100, cex=10, side=1 )		
mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )		
dev.off()

}


##Maximum temerature


{
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


#baren
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$depth[datTmax$class==1&datTmax$region==1], 
			datTmax$Mean[datTmax$class==1&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$depth[datTmax$class==1&datTmax$region==5], 
			datTmax$Mean[datTmax$class==1&datTmax$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmax$depth[datTmax$class==1&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==1&datTmax$region==1], 
			datTmax$depth[datTmax$class==1&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==1&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$depth[datTmax$class==1&datTmax$region==5], 
			datTmax$pc2.5[datTmax$class==1&datTmax$region==5], 
			datTmax$depth[datTmax$class==1&datTmax$region==5], 	
			datTmax$pc97.5[datTmax$class==1&datTmax$region==5],lwd=5, code=0)	

axis(2, seq(0,20, by=5), cex.axis=12, lwd.ticks=8, las=2)
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
mtext("Maximum (Tmax)", side=2, cex=10, line=25 )			
legend(10,25, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")

box(which="plot")				
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
	points(datTmax$depth[datTmax$class==2&datTmax$region==1], 
			datTmax$Mean[datTmax$class==2&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$depth[datTmax$class==2&datTmax$region==2], 
			datTmax$Mean[datTmax$class==2&datTmax$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datTmax$depth[datTmax$class==2&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==2&datTmax$region==1], 
			datTmax$depth[datTmax$class==2&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==2&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$depth[datTmax$class==2&datTmax$region==2], 
			datTmax$pc2.5[datTmax$class==2&datTmax$region==2], 
			datTmax$depth[datTmax$class==2&datTmax$region==2], 	
			datTmax$pc97.5[datTmax$class==2&datTmax$region==2],lwd=5, code=0)	
axis(3, seq(0,20, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,25, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
box(which="plot")			
#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		points(datTmax$depth[datTmax$class==3&datTmax$region==1], 
			datTmax$Mean[datTmax$class==3&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$depth[datTmax$class==3&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==3&datTmax$region==1], 
			datTmax$depth[datTmax$class==3&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==3&datTmax$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,25, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
			points(datTmax$depth[datTmax$class==4&datTmax$region==1], 
			datTmax$Mean[datTmax$class==4&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$depth[datTmax$class==4&datTmax$region==2], 
			datTmax$Mean[datTmax$class==4&datTmax$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datTmax$depth[datTmax$class==4&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==1], 
			datTmax$depth[datTmax$class==4&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$depth[datTmax$class==4&datTmax$region==2], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==2], 
			datTmax$depth[datTmax$class==4&datTmax$region==2], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==2],lwd=5, code=0)
			
			points(datTmax$depth[datTmax$class==4&datTmax$region==5], 
			datTmax$Mean[datTmax$class==4&datTmax$region==5], pch=19,
				col="dodgerblue4", cex=15)
	points(datTmax$depth[datTmax$class==4&datTmax$region==4], 
			datTmax$Mean[datTmax$class==4&datTmax$region==4], pch=19,
				col="slategray4", cex=15)	
	arrows(datTmax$depth[datTmax$class==4&datTmax$region==4], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==4], 
			datTmax$depth[datTmax$class==4&datTmax$region==4], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==4],lwd=5, code=0)			
	arrows(datTmax$depth[datTmax$class==4&datTmax$region==5], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==5], 
			datTmax$depth[datTmax$class==4&datTmax$region==5], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==5],lwd=5, code=0)				
			
			
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,25, c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),
					pch=19, cex=10, bty="n")
		
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		points(datTmax$depth[datTmax$class==5&datTmax$region==1], 
			datTmax$Mean[datTmax$class==5&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$depth[datTmax$class==5&datTmax$region==5], 
			datTmax$Mean[datTmax$class==5&datTmax$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmax$depth[datTmax$class==5&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==5&datTmax$region==1], 
			datTmax$depth[datTmax$class==5&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==5&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$depth[datTmax$class==5&datTmax$region==5], 
			datTmax$pc2.5[datTmax$class==5&datTmax$region==5], 
			datTmax$depth[datTmax$class==5&datTmax$region==5], 	
			datTmax$pc97.5[datTmax$class==5&datTmax$region==5],lwd=5, code=0)	

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 		
legend(10,25, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$depth[datTmax$class==6&datTmax$region==3], 
			datTmax$Mean[datTmax$class==6&datTmax$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datTmax$depth[datTmax$class==6&datTmax$region==3], 
			datTmax$pc2.5[datTmax$class==6&datTmax$region==3], 
			datTmax$depth[datTmax$class==6&datTmax$region==3], 	
			datTmax$pc97.5[datTmax$class==6&datTmax$region==3],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,25, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$depth[datTmax$class==7&datTmax$region==1], 
			datTmax$Mean[datTmax$class==7&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$depth[datTmax$class==7&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==7&datTmax$region==1], 
			datTmax$depth[datTmax$class==7&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==7&datTmax$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,25, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		
	points(datTmax$depth[datTmax$class==8&datTmax$region==1], 
			datTmax$Mean[datTmax$class==8&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$depth[datTmax$class==8&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==8&datTmax$region==1], 
			datTmax$depth[datTmax$class==8&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==8&datTmax$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,25, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")			
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$MeanA[datTmax$class==1&datTmax$region==1], 
			datTmax$Mean[datTmax$class==1&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$MeanA[datTmax$class==1&datTmax$region==5], 
			datTmax$Mean[datTmax$class==1&datTmax$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmax$MeanA[datTmax$class==1&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==1&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==1&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==1&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$MeanA[datTmax$class==1&datTmax$region==5], 
			datTmax$pc2.5[datTmax$class==1&datTmax$region==5], 
			datTmax$MeanA[datTmax$class==1&datTmax$region==5], 	
			datTmax$pc97.5[datTmax$class==1&datTmax$region==5],lwd=5, code=0)	
	arrows(datTmax$pc2.5A[datTmax$class==1&datTmax$region==1], 
			datTmax$Mean[datTmax$class==1&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==1&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==1&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$pc2.5A[datTmax$class==1&datTmax$region==5], 
			datTmax$Mean[datTmax$class==1&datTmax$region==5], 
			datTmax$pc97.5A[datTmax$class==1&datTmax$region==5], 	
			datTmax$Mean[datTmax$class==1&datTmax$region==5],lwd=5, code=0)				
			
			
			
mtext(paste(className[1]), cex=8, line=22, side=1)
axis(2, seq(0,20, by=5), cex.axis=12, las=2, lwd.ticks=8)
axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
mtext("Maximum T (Tmax)", side=2, cex=10, line=25 )			
legend(10,25, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$MeanA[datTmax$class==2&datTmax$region==1], 
			datTmax$Mean[datTmax$class==2&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$MeanA[datTmax$class==2&datTmax$region==2], 
			datTmax$Mean[datTmax$class==2&datTmax$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datTmax$MeanA[datTmax$class==2&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==2&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==2&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==2&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$MeanA[datTmax$class==2&datTmax$region==2], 
			datTmax$pc2.5[datTmax$class==2&datTmax$region==2], 
			datTmax$MeanA[datTmax$class==2&datTmax$region==2], 	
			datTmax$pc97.5[datTmax$class==2&datTmax$region==2],lwd=5, code=0)	
	arrows(datTmax$pc2.5A[datTmax$class==2&datTmax$region==1], 
			datTmax$Mean[datTmax$class==2&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==2&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==2&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$pc2.5A[datTmax$class==2&datTmax$region==2], 
			datTmax$Mean[datTmax$class==2&datTmax$region==2], 
			datTmax$pc97.5A[datTmax$class==2&datTmax$region==2], 	
			datTmax$Mean[datTmax$class==2&datTmax$region==2],lwd=5, code=0)	
axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,25, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
mtext(paste(className[2]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$MeanA[datTmax$class==3&datTmax$region==1], 
			datTmax$Mean[datTmax$class==3&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$MeanA[datTmax$class==3&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==3&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==3&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==3&datTmax$region==1],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==3&datTmax$region==1], 
			datTmax$Mean[datTmax$class==3&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==3&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==3&datTmax$region==1],lwd=5, code=0)			

axis(1, seq(0,25, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,20, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[3]), cex=8, line=22, side=1)		
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			
	points(datTmax$MeanA[datTmax$class==4&datTmax$region==1], 
			datTmax$Mean[datTmax$class==4&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$MeanA[datTmax$class==4&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==4&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==1],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==4&datTmax$region==1], 
			datTmax$Mean[datTmax$class==4&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==4&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==4&datTmax$region==1],lwd=5, code=0)		
		
		points(datTmax$MeanA[datTmax$class==4&datTmax$region==2], 
			datTmax$Mean[datTmax$class==4&datTmax$region==2], pch=19,
				col="tomato3", cex=15)

	arrows(datTmax$MeanA[datTmax$class==4&datTmax$region==2], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==2], 
			datTmax$MeanA[datTmax$class==4&datTmax$region==2], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==2],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==4&datTmax$region==2], 
			datTmax$Mean[datTmax$class==4&datTmax$region==2], 
			datTmax$pc97.5A[datTmax$class==4&datTmax$region==2], 	
			datTmax$Mean[datTmax$class==4&datTmax$region==2],lwd=5, code=0)
			
		points(datTmax$MeanA[datTmax$class==4&datTmax$region==4], 
			datTmax$Mean[datTmax$class==4&datTmax$region==4], pch=19,
				col="slategray4", cex=15)

	arrows(datTmax$MeanA[datTmax$class==4&datTmax$region==4], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==4], 
			datTmax$MeanA[datTmax$class==4&datTmax$region==4], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==4],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==4&datTmax$region==4], 
			datTmax$Mean[datTmax$class==4&datTmax$region==4], 
			datTmax$pc97.5A[datTmax$class==4&datTmax$region==4], 	
			datTmax$Mean[datTmax$class==4&datTmax$region==4],lwd=5, code=0)
			
		points(datTmax$MeanA[datTmax$class==4&datTmax$region==5], 
			datTmax$Mean[datTmax$class==4&datTmax$region==5], pch=19,
				col="dodgerblue4", cex=15)

	arrows(datTmax$MeanA[datTmax$class==4&datTmax$region==5], 
			datTmax$pc2.5[datTmax$class==4&datTmax$region==5], 
			datTmax$MeanA[datTmax$class==4&datTmax$region==5], 	
			datTmax$pc97.5[datTmax$class==4&datTmax$region==5],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==4&datTmax$region==5], 
			datTmax$Mean[datTmax$class==4&datTmax$region==5], 
			datTmax$pc97.5A[datTmax$class==4&datTmax$region==5], 	
			datTmax$Mean[datTmax$class==4&datTmax$region==5],lwd=5, code=0)				
axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,25, 		c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),pch=19,bty="n", cex=10)
		
mtext(paste(className[4]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datTmax$MeanA[datTmax$class==5&datTmax$region==1], 
			datTmax$Mean[datTmax$class==5&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datTmax$MeanA[datTmax$class==5&datTmax$region==5], 
			datTmax$Mean[datTmax$class==5&datTmax$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datTmax$MeanA[datTmax$class==5&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==5&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==5&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==5&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$MeanA[datTmax$class==5&datTmax$region==5], 
			datTmax$pc2.5[datTmax$class==5&datTmax$region==5], 
			datTmax$MeanA[datTmax$class==5&datTmax$region==5], 	
			datTmax$pc97.5[datTmax$class==5&datTmax$region==5],lwd=5, code=0)	
	arrows(datTmax$pc2.5A[datTmax$class==5&datTmax$region==1], 
			datTmax$Mean[datTmax$class==5&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==5&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==5&datTmax$region==1],lwd=5, code=0)			
	arrows(datTmax$pc2.5A[datTmax$class==5&datTmax$region==5], 
			datTmax$Mean[datTmax$class==5&datTmax$region==5], 
			datTmax$pc97.5A[datTmax$class==5&datTmax$region==5], 	
			datTmax$Mean[datTmax$class==5&datTmax$region==5],lwd=5, code=0)				
			
axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
	
legend(10,25 c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[5]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datTmax$MeanA[datTmax$class==6&datTmax$region==3], 
			datTmax$Mean[datTmax$class==6&datTmax$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datTmax$MeanA[datTmax$class==6&datTmax$region==3], 
			datTmax$pc2.5[datTmax$class==6&datTmax$region==3], 
			datTmax$MeanA[datTmax$class==6&datTmax$region==3], 	
			datTmax$pc97.5[datTmax$class==6&datTmax$region==3],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==6&datTmax$region==3], 
			datTmax$Mean[datTmax$class==6&datTmax$region==3], 
			datTmax$pc97.5A[datTmax$class==6&datTmax$region==3], 	
			datTmax$Mean[datTmax$class==6&datTmax$region==3],lwd=5, code=0)			

axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,25, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[6]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		points(datTmax$MeanA[datTmax$class==7&datTmax$region==1], 
			datTmax$Mean[datTmax$class==7&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$MeanA[datTmax$class==7&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==7&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==7&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==7&datTmax$region==1],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==7&datTmax$region==1], 
			datTmax$Mean[datTmax$class==7&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==7&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==7&datTmax$region==1],lwd=5, code=0)			

axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,25, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	

mtext(paste(className[7]), cex=8, line=22, side=1)
					
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,25), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")

		points(datTmax$MeanA[datTmax$class==8&datTmax$region==1], 
			datTmax$Mean[datTmax$class==8&datTmax$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datTmax$MeanA[datTmax$class==8&datTmax$region==1], 
			datTmax$pc2.5[datTmax$class==8&datTmax$region==1], 
			datTmax$MeanA[datTmax$class==8&datTmax$region==1], 	
			datTmax$pc97.5[datTmax$class==8&datTmax$region==1],lwd=5, code=0)			

	arrows(datTmax$pc2.5A[datTmax$class==8&datTmax$region==1], 
			datTmax$Mean[datTmax$class==8&datTmax$region==1], 
			datTmax$pc97.5A[datTmax$class==8&datTmax$region==1], 	
			datTmax$Mean[datTmax$class==8&datTmax$region==1],lwd=5, code=0)			

axis(1, seq(0,20, by=5),cex.axis=12, lwd.ticks=8, padj=1) 
legend(10,25, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[8]), cex=8, line=22, side=1)			
mtext("Maximum Air Temperature (Tmax, C)", outer=TRUE, line=-100, cex=10, side=1 )		
mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )		
dev.off()

}

#Maximum peak
{
wb<-40
hb<-40
nlowT=.5
nhighT=1

jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\PeakS_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))


#baren
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$depth[datPS$class==1&datPS$region==1], 
			datPS$Mean[datPS$class==1&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$depth[datPS$class==1&datPS$region==5], 
			datPS$Mean[datPS$class==1&datPS$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPS$depth[datPS$class==1&datPS$region==1], 
			datPS$pc2.5[datPS$class==1&datPS$region==1], 
			datPS$depth[datPS$class==1&datPS$region==1], 	
			datPS$pc97.5[datPS$class==1&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$depth[datPS$class==1&datPS$region==5], 
			datPS$pc2.5[datPS$class==1&datPS$region==5], 
			datPS$depth[datPS$class==1&datPS$region==5], 	
			datPS$pc97.5[datPS$class==1&datPS$region==5],lwd=5, code=0)	

axis(2, seq(.55,.95,by=.1), cex.axis=12, lwd.ticks=8, las=2)
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
mtext("Time of Maximum", side=2, cex=10, line=25 )			
legend(10,1, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")

box(which="plot")				
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
	points(datPS$depth[datPS$class==2&datPS$region==1], 
			datPS$Mean[datPS$class==2&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$depth[datPS$class==2&datPS$region==2], 
			datPS$Mean[datPS$class==2&datPS$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datPS$depth[datPS$class==2&datPS$region==1], 
			datPS$pc2.5[datPS$class==2&datPS$region==1], 
			datPS$depth[datPS$class==2&datPS$region==1], 	
			datPS$pc97.5[datPS$class==2&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$depth[datPS$class==2&datPS$region==2], 
			datPS$pc2.5[datPS$class==2&datPS$region==2], 
			datPS$depth[datPS$class==2&datPS$region==2], 	
			datPS$pc97.5[datPS$class==2&datPS$region==2],lwd=5, code=0)	
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
box(which="plot")			
#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		points(datPS$depth[datPS$class==3&datPS$region==1], 
			datPS$Mean[datPS$class==3&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$depth[datPS$class==3&datPS$region==1], 
			datPS$pc2.5[datPS$class==3&datPS$region==1], 
			datPS$depth[datPS$class==3&datPS$region==1], 	
			datPS$pc97.5[datPS$class==3&datPS$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
			points(datPS$depth[datPS$class==4&datPS$region==1], 
			datPS$Mean[datPS$class==4&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$depth[datPS$class==4&datPS$region==2], 
			datPS$Mean[datPS$class==4&datPS$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datPS$depth[datPS$class==4&datPS$region==1], 
			datPS$pc2.5[datPS$class==4&datPS$region==1], 
			datPS$depth[datPS$class==4&datPS$region==1], 	
			datPS$pc97.5[datPS$class==4&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$depth[datPS$class==4&datPS$region==2], 
			datPS$pc2.5[datPS$class==4&datPS$region==2], 
			datPS$depth[datPS$class==4&datPS$region==2], 	
			datPS$pc97.5[datPS$class==4&datPS$region==2],lwd=5, code=0)
			
			points(datPS$depth[datPS$class==4&datPS$region==5], 
			datPS$Mean[datPS$class==4&datPS$region==5], pch=19,
				col="dodgerblue4", cex=15)
	points(datPS$depth[datPS$class==4&datPS$region==4], 
			datPS$Mean[datPS$class==4&datPS$region==4], pch=19,
				col="slategray4", cex=15)	
	arrows(datPS$depth[datPS$class==4&datPS$region==4], 
			datPS$pc2.5[datPS$class==4&datPS$region==4], 
			datPS$depth[datPS$class==4&datPS$region==4], 	
			datPS$pc97.5[datPS$class==4&datPS$region==4],lwd=5, code=0)			
	arrows(datPS$depth[datPS$class==4&datPS$region==5], 
			datPS$pc2.5[datPS$class==4&datPS$region==5], 
			datPS$depth[datPS$class==4&datPS$region==5], 	
			datPS$pc97.5[datPS$class==4&datPS$region==5],lwd=5, code=0)				
			
			
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1, c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),
					pch=19, cex=10, bty="n")
		
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		points(datPS$depth[datPS$class==5&datPS$region==1], 
			datPS$Mean[datPS$class==5&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$depth[datPS$class==5&datPS$region==5], 
			datPS$Mean[datPS$class==5&datPS$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPS$depth[datPS$class==5&datPS$region==1], 
			datPS$pc2.5[datPS$class==5&datPS$region==1], 
			datPS$depth[datPS$class==5&datPS$region==1], 	
			datPS$pc97.5[datPS$class==5&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$depth[datPS$class==5&datPS$region==5], 
			datPS$pc2.5[datPS$class==5&datPS$region==5], 
			datPS$depth[datPS$class==5&datPS$region==5], 	
			datPS$pc97.5[datPS$class==5&datPS$region==5],lwd=5, code=0)	

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 		
legend(10,1, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$depth[datPS$class==6&datPS$region==3], 
			datPS$Mean[datPS$class==6&datPS$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datPS$depth[datPS$class==6&datPS$region==3], 
			datPS$pc2.5[datPS$class==6&datPS$region==3], 
			datPS$depth[datPS$class==6&datPS$region==3], 	
			datPS$pc97.5[datPS$class==6&datPS$region==3],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$depth[datPS$class==7&datPS$region==1], 
			datPS$Mean[datPS$class==7&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$depth[datPS$class==7&datPS$region==1], 
			datPS$pc2.5[datPS$class==7&datPS$region==1], 
			datPS$depth[datPS$class==7&datPS$region==1], 	
			datPS$pc97.5[datPS$class==7&datPS$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		
	points(datPS$depth[datPS$class==8&datPS$region==1], 
			datPS$Mean[datPS$class==8&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$depth[datPS$class==8&datPS$region==1], 
			datPS$pc2.5[datPS$class==8&datPS$region==1], 
			datPS$depth[datPS$class==8&datPS$region==1], 	
			datPS$pc97.5[datPS$class==8&datPS$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(10,1, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")			
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$MeanA[datPS$class==1&datPS$region==1], 
			datPS$Mean[datPS$class==1&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$MeanA[datPS$class==1&datPS$region==5], 
			datPS$Mean[datPS$class==1&datPS$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPS$MeanA[datPS$class==1&datPS$region==1], 
			datPS$pc2.5[datPS$class==1&datPS$region==1], 
			datPS$MeanA[datPS$class==1&datPS$region==1], 	
			datPS$pc97.5[datPS$class==1&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$MeanA[datPS$class==1&datPS$region==5], 
			datPS$pc2.5[datPS$class==1&datPS$region==5], 
			datPS$MeanA[datPS$class==1&datPS$region==5], 	
			datPS$pc97.5[datPS$class==1&datPS$region==5],lwd=5, code=0)	
	arrows(datPS$pc2.5A[datPS$class==1&datPS$region==1], 
			datPS$Mean[datPS$class==1&datPS$region==1], 
			datPS$pc97.5A[datPS$class==1&datPS$region==1], 	
			datPS$Mean[datPS$class==1&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$pc2.5A[datPS$class==1&datPS$region==5], 
			datPS$Mean[datPS$class==1&datPS$region==5], 
			datPS$pc97.5A[datPS$class==1&datPS$region==5], 	
			datPS$Mean[datPS$class==1&datPS$region==5],lwd=5, code=0)				
			
			
			
mtext(paste(className[1]), cex=8, line=22, side=1)
axis(2, seq(.55,.95,by=.1), cex.axis=12, las=2, lwd.ticks=8)
axis(1, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) ),cex.axis=12, lwd.ticks=8, padj=1) 
mtext("Time of Maximum T", side=2, cex=10, line=25 )			
legend(0.55,1, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$MeanA[datPS$class==2&datPS$region==1], 
			datPS$Mean[datPS$class==2&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$MeanA[datPS$class==2&datPS$region==2], 
			datPS$Mean[datPS$class==2&datPS$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datPS$MeanA[datPS$class==2&datPS$region==1], 
			datPS$pc2.5[datPS$class==2&datPS$region==1], 
			datPS$MeanA[datPS$class==2&datPS$region==1], 	
			datPS$pc97.5[datPS$class==2&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$MeanA[datPS$class==2&datPS$region==2], 
			datPS$pc2.5[datPS$class==2&datPS$region==2], 
			datPS$MeanA[datPS$class==2&datPS$region==2], 	
			datPS$pc97.5[datPS$class==2&datPS$region==2],lwd=5, code=0)	
	arrows(datPS$pc2.5A[datPS$class==2&datPS$region==1], 
			datPS$Mean[datPS$class==2&datPS$region==1], 
			datPS$pc97.5A[datPS$class==2&datPS$region==1], 	
			datPS$Mean[datPS$class==2&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$pc2.5A[datPS$class==2&datPS$region==2], 
			datPS$Mean[datPS$class==2&datPS$region==2], 
			datPS$pc97.5A[datPS$class==2&datPS$region==2], 	
			datPS$Mean[datPS$class==2&datPS$region==2],lwd=5, code=0)	
axis(1, seq(.55,.95,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(.55,1, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
mtext(paste(className[2]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$MeanA[datPS$class==3&datPS$region==1], 
			datPS$Mean[datPS$class==3&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$MeanA[datPS$class==3&datPS$region==1], 
			datPS$pc2.5[datPS$class==3&datPS$region==1], 
			datPS$MeanA[datPS$class==3&datPS$region==1], 	
			datPS$pc97.5[datPS$class==3&datPS$region==1],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==3&datPS$region==1], 
			datPS$Mean[datPS$class==3&datPS$region==1], 
			datPS$pc97.5A[datPS$class==3&datPS$region==1], 	
			datPS$Mean[datPS$class==3&datPS$region==1],lwd=5, code=0)			

axis(1, seq(.55,.95,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(.55,1, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[3]), cex=8, line=22, side=1)		
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			
	points(datPS$MeanA[datPS$class==4&datPS$region==1], 
			datPS$Mean[datPS$class==4&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$MeanA[datPS$class==4&datPS$region==1], 
			datPS$pc2.5[datPS$class==4&datPS$region==1], 
			datPS$MeanA[datPS$class==4&datPS$region==1], 	
			datPS$pc97.5[datPS$class==4&datPS$region==1],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==4&datPS$region==1], 
			datPS$Mean[datPS$class==4&datPS$region==1], 
			datPS$pc97.5A[datPS$class==4&datPS$region==1], 	
			datPS$Mean[datPS$class==4&datPS$region==1],lwd=5, code=0)		
		
		points(datPS$MeanA[datPS$class==4&datPS$region==2], 
			datPS$Mean[datPS$class==4&datPS$region==2], pch=19,
				col="tomato3", cex=15)

	arrows(datPS$MeanA[datPS$class==4&datPS$region==2], 
			datPS$pc2.5[datPS$class==4&datPS$region==2], 
			datPS$MeanA[datPS$class==4&datPS$region==2], 	
			datPS$pc97.5[datPS$class==4&datPS$region==2],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==4&datPS$region==2], 
			datPS$Mean[datPS$class==4&datPS$region==2], 
			datPS$pc97.5A[datPS$class==4&datPS$region==2], 	
			datPS$Mean[datPS$class==4&datPS$region==2],lwd=5, code=0)
			
		points(datPS$MeanA[datPS$class==4&datPS$region==4], 
			datPS$Mean[datPS$class==4&datPS$region==4], pch=19,
				col="slategray4", cex=15)

	arrows(datPS$MeanA[datPS$class==4&datPS$region==4], 
			datPS$pc2.5[datPS$class==4&datPS$region==4], 
			datPS$MeanA[datPS$class==4&datPS$region==4], 	
			datPS$pc97.5[datPS$class==4&datPS$region==4],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==4&datPS$region==4], 
			datPS$Mean[datPS$class==4&datPS$region==4], 
			datPS$pc97.5A[datPS$class==4&datPS$region==4], 	
			datPS$Mean[datPS$class==4&datPS$region==4],lwd=5, code=0)
			
		points(datPS$MeanA[datPS$class==4&datPS$region==5], 
			datPS$Mean[datPS$class==4&datPS$region==5], pch=19,
				col="dodgerblue4", cex=15)

	arrows(datPS$MeanA[datPS$class==4&datPS$region==5], 
			datPS$pc2.5[datPS$class==4&datPS$region==5], 
			datPS$MeanA[datPS$class==4&datPS$region==5], 	
			datPS$pc97.5[datPS$class==4&datPS$region==5],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==4&datPS$region==5], 
			datPS$Mean[datPS$class==4&datPS$region==5], 
			datPS$pc97.5A[datPS$class==4&datPS$region==5], 	
			datPS$Mean[datPS$class==4&datPS$region==5],lwd=5, code=0)				
axis(1, seq(.55,.95,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0.55,1, 		c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),pch=19,bty="n", cex=10)
		
mtext(paste(className[4]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datPS$MeanA[datPS$class==5&datPS$region==1], 
			datPS$Mean[datPS$class==5&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPS$MeanA[datPS$class==5&datPS$region==5], 
			datPS$Mean[datPS$class==5&datPS$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPS$MeanA[datPS$class==5&datPS$region==1], 
			datPS$pc2.5[datPS$class==5&datPS$region==1], 
			datPS$MeanA[datPS$class==5&datPS$region==1], 	
			datPS$pc97.5[datPS$class==5&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$MeanA[datPS$class==5&datPS$region==5], 
			datPS$pc2.5[datPS$class==5&datPS$region==5], 
			datPS$MeanA[datPS$class==5&datPS$region==5], 	
			datPS$pc97.5[datPS$class==5&datPS$region==5],lwd=5, code=0)	
	arrows(datPS$pc2.5A[datPS$class==5&datPS$region==1], 
			datPS$Mean[datPS$class==5&datPS$region==1], 
			datPS$pc97.5A[datPS$class==5&datPS$region==1], 	
			datPS$Mean[datPS$class==5&datPS$region==1],lwd=5, code=0)			
	arrows(datPS$pc2.5A[datPS$class==5&datPS$region==5], 
			datPS$Mean[datPS$class==5&datPS$region==5], 
			datPS$pc97.5A[datPS$class==5&datPS$region==5], 	
			datPS$Mean[datPS$class==5&datPS$region==5],lwd=5, code=0)				
			
axis(1, seq(.55,.95,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
	
legend(.55,1 c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[5]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPS$MeanA[datPS$class==6&datPS$region==3], 
			datPS$Mean[datPS$class==6&datPS$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datPS$MeanA[datPS$class==6&datPS$region==3], 
			datPS$pc2.5[datPS$class==6&datPS$region==3], 
			datPS$MeanA[datPS$class==6&datPS$region==3], 	
			datPS$pc97.5[datPS$class==6&datPS$region==3],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==6&datPS$region==3], 
			datPS$Mean[datPS$class==6&datPS$region==3], 
			datPS$pc97.5A[datPS$class==6&datPS$region==3], 	
			datPS$Mean[datPS$class==6&datPS$region==3],lwd=5, code=0)			

axis(1, seq(.55,.95,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(.55,1, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[6]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		points(datPS$MeanA[datPS$class==7&datPS$region==1], 
			datPS$Mean[datPS$class==7&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$MeanA[datPS$class==7&datPS$region==1], 
			datPS$pc2.5[datPS$class==7&datPS$region==1], 
			datPS$MeanA[datPS$class==7&datPS$region==1], 	
			datPS$pc97.5[datPS$class==7&datPS$region==1],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==7&datPS$region==1], 
			datPS$Mean[datPS$class==7&datPS$region==1], 
			datPS$pc97.5A[datPS$class==7&datPS$region==1], 	
			datPS$Mean[datPS$class==7&datPS$region==1],lwd=5, code=0)			

axis(1, seq(.55,.95,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(.55,1, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	

mtext(paste(className[7]), cex=8, line=22, side=1)
					
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")

		points(datPS$MeanA[datPS$class==8&datPS$region==1], 
			datPS$Mean[datPS$class==8&datPS$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPS$MeanA[datPS$class==8&datPS$region==1], 
			datPS$pc2.5[datPS$class==8&datPS$region==1], 
			datPS$MeanA[datPS$class==8&datPS$region==1], 	
			datPS$pc97.5[datPS$class==8&datPS$region==1],lwd=5, code=0)			

	arrows(datPS$pc2.5A[datPS$class==8&datPS$region==1], 
			datPS$Mean[datPS$class==8&datPS$region==1], 
			datPS$pc97.5A[datPS$class==8&datPS$region==1], 	
			datPS$Mean[datPS$class==8&datPS$region==1],lwd=5, code=0)			

axis(1, seq(.55,.95, by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(.55,1, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[8]), cex=8, line=22, side=1)			
mtext("Maximum Air Temperature Peak Time", outer=TRUE, line=-100, cex=10, side=1 )		
mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )		
dev.off()

}


#Minimum peak
{
wb<-40
hb<-40
nlowT=0
nhighT=6

jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\PeakW_depth.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))


#baren
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$depth[datPW$class==1&datPW$region==1], 
			datPW$Mean[datPW$class==1&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$depth[datPW$class==1&datPW$region==5], 
			datPW$Mean[datPW$class==1&datPW$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPW$depth[datPW$class==1&datPW$region==1], 
			datPW$pc2.5[datPW$class==1&datPW$region==1], 
			datPW$depth[datPW$class==1&datPW$region==1], 	
			datPW$pc97.5[datPW$class==1&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$depth[datPW$class==1&datPW$region==5], 
			datPW$pc2.5[datPW$class==1&datPW$region==5], 
			datPW$depth[datPW$class==1&datPW$region==5], 	
			datPW$pc97.5[datPW$class==1&datPW$region==5],lwd=5, code=0)	

axis(2, seq(.05,.55,by=.1), cex.axis=12, lwd.ticks=8, las=2)
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
mtext("Time of Minimum", side=2, cex=10, line=25 )			
legend(0,.15, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")

box(which="plot")				
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
	points(datPW$depth[datPW$class==2&datPW$region==1], 
			datPW$Mean[datPW$class==2&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$depth[datPW$class==2&datPW$region==2], 
			datPW$Mean[datPW$class==2&datPW$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datPW$depth[datPW$class==2&datPW$region==1], 
			datPW$pc2.5[datPW$class==2&datPW$region==1], 
			datPW$depth[datPW$class==2&datPW$region==1], 	
			datPW$pc97.5[datPW$class==2&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$depth[datPW$class==2&datPW$region==2], 
			datPW$pc2.5[datPW$class==2&datPW$region==2], 
			datPW$depth[datPW$class==2&datPW$region==2], 	
			datPW$pc97.5[datPW$class==2&datPW$region==2],lwd=5, code=0)	
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(0,.15, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
box(which="plot")			
#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		points(datPW$depth[datPW$class==3&datPW$region==1], 
			datPW$Mean[datPW$class==3&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$depth[datPW$class==3&datPW$region==1], 
			datPW$pc2.5[datPW$class==3&datPW$region==1], 
			datPW$depth[datPW$class==3&datPW$region==1], 	
			datPW$pc97.5[datPW$class==3&datPW$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(0,.15, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
			points(datPW$depth[datPW$class==4&datPW$region==1], 
			datPW$Mean[datPW$class==4&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$depth[datPW$class==4&datPW$region==2], 
			datPW$Mean[datPW$class==4&datPW$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datPW$depth[datPW$class==4&datPW$region==1], 
			datPW$pc2.5[datPW$class==4&datPW$region==1], 
			datPW$depth[datPW$class==4&datPW$region==1], 	
			datPW$pc97.5[datPW$class==4&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$depth[datPW$class==4&datPW$region==2], 
			datPW$pc2.5[datPW$class==4&datPW$region==2], 
			datPW$depth[datPW$class==4&datPW$region==2], 	
			datPW$pc97.5[datPW$class==4&datPW$region==2],lwd=5, code=0)
			
			points(datPW$depth[datPW$class==4&datPW$region==5], 
			datPW$Mean[datPW$class==4&datPW$region==5], pch=19,
				col="dodgerblue4", cex=15)
	points(datPW$depth[datPW$class==4&datPW$region==4], 
			datPW$Mean[datPW$class==4&datPW$region==4], pch=19,
				col="slategray4", cex=15)	
	arrows(datPW$depth[datPW$class==4&datPW$region==4], 
			datPW$pc2.5[datPW$class==4&datPW$region==4], 
			datPW$depth[datPW$class==4&datPW$region==4], 	
			datPW$pc97.5[datPW$class==4&datPW$region==4],lwd=5, code=0)			
	arrows(datPW$depth[datPW$class==4&datPW$region==5], 
			datPW$pc2.5[datPW$class==4&datPW$region==5], 
			datPW$depth[datPW$class==4&datPW$region==5], 	
			datPW$pc97.5[datPW$class==4&datPW$region==5],lwd=5, code=0)				
			
			
axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(0,.15, c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),
					pch=19, cex=10, bty="n")
		
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		points(datPW$depth[datPW$class==5&datPW$region==1], 
			datPW$Mean[datPW$class==5&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$depth[datPW$class==5&datPW$region==5], 
			datPW$Mean[datPW$class==5&datPW$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPW$depth[datPW$class==5&datPW$region==1], 
			datPW$pc2.5[datPW$class==5&datPW$region==1], 
			datPW$depth[datPW$class==5&datPW$region==1], 	
			datPW$pc97.5[datPW$class==5&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$depth[datPW$class==5&datPW$region==5], 
			datPW$pc2.5[datPW$class==5&datPW$region==5], 
			datPW$depth[datPW$class==5&datPW$region==5], 	
			datPW$pc97.5[datPW$class==5&datPW$region==5],lwd=5, code=0)	

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 		
legend(0,.15, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$depth[datPW$class==6&datPW$region==3], 
			datPW$Mean[datPW$class==6&datPW$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datPW$depth[datPW$class==6&datPW$region==3], 
			datPW$pc2.5[datPW$class==6&datPW$region==3], 
			datPW$depth[datPW$class==6&datPW$region==3], 	
			datPW$pc97.5[datPW$class==6&datPW$region==3],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(0,.15, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
		
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$depth[datPW$class==7&datPW$region==1], 
			datPW$Mean[datPW$class==7&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$depth[datPW$class==7&datPW$region==1], 
			datPW$pc2.5[datPW$class==7&datPW$region==1], 
			datPW$depth[datPW$class==7&datPW$region==1], 	
			datPW$pc97.5[datPW$class==7&datPW$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(0,.15, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
box(which="plot")
		#		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(-2,21), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		
	points(datPW$depth[datPW$class==8&datPW$region==1], 
			datPW$Mean[datPW$class==8&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$depth[datPW$class==8&datPW$region==1], 
			datPW$pc2.5[datPW$class==8&datPW$region==1], 
			datPW$depth[datPW$class==8&datPW$region==1], 	
			datPW$pc97.5[datPW$class==8&datPW$region==1],lwd=5, code=0)			

axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
legend(0,.15, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")			
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$MeanA[datPW$class==1&datPW$region==1], 
			datPW$Mean[datPW$class==1&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$MeanA[datPW$class==1&datPW$region==5], 
			datPW$Mean[datPW$class==1&datPW$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPW$MeanA[datPW$class==1&datPW$region==1], 
			datPW$pc2.5[datPW$class==1&datPW$region==1], 
			datPW$MeanA[datPW$class==1&datPW$region==1], 	
			datPW$pc97.5[datPW$class==1&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$MeanA[datPW$class==1&datPW$region==5], 
			datPW$pc2.5[datPW$class==1&datPW$region==5], 
			datPW$MeanA[datPW$class==1&datPW$region==5], 	
			datPW$pc97.5[datPW$class==1&datPW$region==5],lwd=5, code=0)	
	arrows(datPW$pc2.5A[datPW$class==1&datPW$region==1], 
			datPW$Mean[datPW$class==1&datPW$region==1], 
			datPW$pc97.5A[datPW$class==1&datPW$region==1], 	
			datPW$Mean[datPW$class==1&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$pc2.5A[datPW$class==1&datPW$region==5], 
			datPW$Mean[datPW$class==1&datPW$region==5], 
			datPW$pc97.5A[datPW$class==1&datPW$region==5], 	
			datPW$Mean[datPW$class==1&datPW$region==5],lwd=5, code=0)				
			
			
			
mtext(paste(className[1]), cex=8, line=22, side=1)
axis(2, seq(.05,.55,by=.1), cex.axis=12, las=2, lwd.ticks=8)
axis(1, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) ),cex.axis=12, lwd.ticks=8, padj=1) 
mtext("Time of Minimum T", side=2, cex=10, line=25 )			
legend(0,.15, c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$MeanA[datPW$class==2&datPW$region==1], 
			datPW$Mean[datPW$class==2&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$MeanA[datPW$class==2&datPW$region==2], 
			datPW$Mean[datPW$class==2&datPW$region==2], pch=19,
				col="tomato3", cex=15)	
	arrows(datPW$MeanA[datPW$class==2&datPW$region==1], 
			datPW$pc2.5[datPW$class==2&datPW$region==1], 
			datPW$MeanA[datPW$class==2&datPW$region==1], 	
			datPW$pc97.5[datPW$class==2&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$MeanA[datPW$class==2&datPW$region==2], 
			datPW$pc2.5[datPW$class==2&datPW$region==2], 
			datPW$MeanA[datPW$class==2&datPW$region==2], 	
			datPW$pc97.5[datPW$class==2&datPW$region==2],lwd=5, code=0)	
	arrows(datPW$pc2.5A[datPW$class==2&datPW$region==1], 
			datPW$Mean[datPW$class==2&datPW$region==1], 
			datPW$pc97.5A[datPW$class==2&datPW$region==1], 	
			datPW$Mean[datPW$class==2&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$pc2.5A[datPW$class==2&datPW$region==2], 
			datPW$Mean[datPW$class==2&datPW$region==2], 
			datPW$pc97.5A[datPW$class==2&datPW$region==2], 	
			datPW$Mean[datPW$class==2&datPW$region==2],lwd=5, code=0)	
axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,.15, c("AK","Canada"), col=c("darkolivegreen4","tomato3"),
					pch=19, cex=10, bty="n")
mtext(paste(className[2]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$MeanA[datPW$class==3&datPW$region==1], 
			datPW$Mean[datPW$class==3&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$MeanA[datPW$class==3&datPW$region==1], 
			datPW$pc2.5[datPW$class==3&datPW$region==1], 
			datPW$MeanA[datPW$class==3&datPW$region==1], 	
			datPW$pc97.5[datPW$class==3&datPW$region==1],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==3&datPW$region==1], 
			datPW$Mean[datPW$class==3&datPW$region==1], 
			datPW$pc97.5A[datPW$class==3&datPW$region==1], 	
			datPW$Mean[datPW$class==3&datPW$region==1],lwd=5, code=0)			

axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,.15, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[3]), cex=8, line=22, side=1)		
		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			
	points(datPW$MeanA[datPW$class==4&datPW$region==1], 
			datPW$Mean[datPW$class==4&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$MeanA[datPW$class==4&datPW$region==1], 
			datPW$pc2.5[datPW$class==4&datPW$region==1], 
			datPW$MeanA[datPW$class==4&datPW$region==1], 	
			datPW$pc97.5[datPW$class==4&datPW$region==1],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==4&datPW$region==1], 
			datPW$Mean[datPW$class==4&datPW$region==1], 
			datPW$pc97.5A[datPW$class==4&datPW$region==1], 	
			datPW$Mean[datPW$class==4&datPW$region==1],lwd=5, code=0)		
		
		points(datPW$MeanA[datPW$class==4&datPW$region==2], 
			datPW$Mean[datPW$class==4&datPW$region==2], pch=19,
				col="tomato3", cex=15)

	arrows(datPW$MeanA[datPW$class==4&datPW$region==2], 
			datPW$pc2.5[datPW$class==4&datPW$region==2], 
			datPW$MeanA[datPW$class==4&datPW$region==2], 	
			datPW$pc97.5[datPW$class==4&datPW$region==2],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==4&datPW$region==2], 
			datPW$Mean[datPW$class==4&datPW$region==2], 
			datPW$pc97.5A[datPW$class==4&datPW$region==2], 	
			datPW$Mean[datPW$class==4&datPW$region==2],lwd=5, code=0)
			
		points(datPW$MeanA[datPW$class==4&datPW$region==4], 
			datPW$Mean[datPW$class==4&datPW$region==4], pch=19,
				col="slategray4", cex=15)

	arrows(datPW$MeanA[datPW$class==4&datPW$region==4], 
			datPW$pc2.5[datPW$class==4&datPW$region==4], 
			datPW$MeanA[datPW$class==4&datPW$region==4], 	
			datPW$pc97.5[datPW$class==4&datPW$region==4],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==4&datPW$region==4], 
			datPW$Mean[datPW$class==4&datPW$region==4], 
			datPW$pc97.5A[datPW$class==4&datPW$region==4], 	
			datPW$Mean[datPW$class==4&datPW$region==4],lwd=5, code=0)
			
		points(datPW$MeanA[datPW$class==4&datPW$region==5], 
			datPW$Mean[datPW$class==4&datPW$region==5], pch=19,
				col="dodgerblue4", cex=15)

	arrows(datPW$MeanA[datPW$class==4&datPW$region==5], 
			datPW$pc2.5[datPW$class==4&datPW$region==5], 
			datPW$MeanA[datPW$class==4&datPW$region==5], 	
			datPW$pc97.5[datPW$class==4&datPW$region==5],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==4&datPW$region==5], 
			datPW$Mean[datPW$class==4&datPW$region==5], 
			datPW$pc97.5A[datPW$class==4&datPW$region==5], 	
			datPW$Mean[datPW$class==4&datPW$region==5],lwd=5, code=0)				
axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,0.2, 		c("AK","Canada","W russia", "island"), 
		col=c("darkolivegreen4","tomato3","slategray4","dodgerblue4"),pch=19,bty="n", cex=10)
		
mtext(paste(className[4]), cex=8, line=22, side=1)		
box(which="plot")		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datPW$MeanA[datPW$class==5&datPW$region==1], 
			datPW$Mean[datPW$class==5&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)
	points(datPW$MeanA[datPW$class==5&datPW$region==5], 
			datPW$Mean[datPW$class==5&datPW$region==5], pch=19,
				col="dodgerblue4", cex=15)	
	arrows(datPW$MeanA[datPW$class==5&datPW$region==1], 
			datPW$pc2.5[datPW$class==5&datPW$region==1], 
			datPW$MeanA[datPW$class==5&datPW$region==1], 	
			datPW$pc97.5[datPW$class==5&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$MeanA[datPW$class==5&datPW$region==5], 
			datPW$pc2.5[datPW$class==5&datPW$region==5], 
			datPW$MeanA[datPW$class==5&datPW$region==5], 	
			datPW$pc97.5[datPW$class==5&datPW$region==5],lwd=5, code=0)	
	arrows(datPW$pc2.5A[datPW$class==5&datPW$region==1], 
			datPW$Mean[datPW$class==5&datPW$region==1], 
			datPW$pc97.5A[datPW$class==5&datPW$region==1], 	
			datPW$Mean[datPW$class==5&datPW$region==1],lwd=5, code=0)			
	arrows(datPW$pc2.5A[datPW$class==5&datPW$region==5], 
			datPW$Mean[datPW$class==5&datPW$region==5], 
			datPW$pc97.5A[datPW$class==5&datPW$region==5], 	
			datPW$Mean[datPW$class==5&datPW$region==5],lwd=5, code=0)				
			
axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
	
legend(0,.15 c("AK","Island"), col=c("darkolivegreen4","dodgerblue4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[5]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
	points(datPW$MeanA[datPW$class==6&datPW$region==3], 
			datPW$Mean[datPW$class==6&datPW$region==3], pch=19,
				col="mistyrose3", cex=15)

	arrows(datPW$MeanA[datPW$class==6&datPW$region==3], 
			datPW$pc2.5[datPW$class==6&datPW$region==3], 
			datPW$MeanA[datPW$class==6&datPW$region==3], 	
			datPW$pc97.5[datPW$class==6&datPW$region==3],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==6&datPW$region==3], 
			datPW$Mean[datPW$class==6&datPW$region==3], 
			datPW$pc97.5A[datPW$class==6&datPW$region==3], 	
			datPW$Mean[datPW$class==6&datPW$region==3],lwd=5, code=0)			

axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,.15, c("E Russia"), col=c("mistyrose3"),
					pch=19, cex=10, bty="n")	
		
mtext(paste(className[6]), cex=8, line=22, side=1)		
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
		
		points(datPW$MeanA[datPW$class==7&datPW$region==1], 
			datPW$Mean[datPW$class==7&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$MeanA[datPW$class==7&datPW$region==1], 
			datPW$pc2.5[datPW$class==7&datPW$region==1], 
			datPW$MeanA[datPW$class==7&datPW$region==1], 	
			datPW$pc97.5[datPW$class==7&datPW$region==1],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==7&datPW$region==1], 
			datPW$Mean[datPW$class==7&datPW$region==1], 
			datPW$pc97.5A[datPW$class==7&datPW$region==1], 	
			datPW$Mean[datPW$class==7&datPW$region==1],lwd=5, code=0)			

axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,.15, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	

mtext(paste(className[7]), cex=8, line=22, side=1)
					
box(which="plot")
		
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(nlowT,nhighT), ylim=c(nlowT,nhighT), axes=FALSE,
		xlab=" ",ylab=" ", xaxs="i", yaxs="i")

		points(datPW$MeanA[datPW$class==8&datPW$region==1], 
			datPW$Mean[datPW$class==8&datPW$region==1], pch=19,
				col="darkolivegreen4", cex=15)

	arrows(datPW$MeanA[datPW$class==8&datPW$region==1], 
			datPW$pc2.5[datPW$class==8&datPW$region==1], 
			datPW$MeanA[datPW$class==8&datPW$region==1], 	
			datPW$pc97.5[datPW$class==8&datPW$region==1],lwd=5, code=0)			

	arrows(datPW$pc2.5A[datPW$class==8&datPW$region==1], 
			datPW$Mean[datPW$class==8&datPW$region==1], 
			datPW$pc97.5A[datPW$class==8&datPW$region==1], 	
			datPW$Mean[datPW$class==8&datPW$region==1],lwd=5, code=0)			

axis(1, seq(.05,.55,by=.1),cex.axis=12, lwd.ticks=8, padj=1) 
legend(0,.15, c("AK"), col=c("darkolivegreen4"),
					pch=19, cex=10, bty="n")	
mtext(paste(className[8]), cex=8, line=22, side=1)			
mtext("Minimum Air Temperature Peak Time", outer=TRUE, line=-100, cex=10, side=1 )		
mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )		
dev.off()

}