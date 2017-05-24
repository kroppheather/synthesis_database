#######################################################
########this script reads in output from model 10 #####
########and plots data and empirical mdoel output #####
########that looks at variation across vegetation #####
########community types                           #####
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
datNT<-datN1[datN1$parm=="Tn",]
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

						
#turn data into a list

datAll<-list(datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ)						
#get the average Air value for covariate centering
roundI<-c(0,0,0,0,2,2,0)
TexM<-numeric(0)
for(i in 1:7){
	TexM[i]<-round(mean(datAll[[k]]$MeanA),roundI[i])

}
data.name<-c("nfreeze","nthaw","Tmax","Tmin","Peakmax","Peakmin","DayZero")						
						
##################################################
##################################################
#plot each vege community with region shown



wb<-40
hb<-40
nlow=c(0,0,0,-40,.6,0,0)
nhigh=c(1.7,1.7,25,0,1,.6,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","peakMax","peakMin", "zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum","Soil temperature maximum time",
			"Soil temperature minimum time","Days in zero mean")
Xlabel<-c("Air temperature minimum","Air temperature maximum",
		"Air temperature maximum","Air temperature minimum",
		"Air temperature maximum timing","Air temperature minimum timing","Air temperature minimum")			
axisL<-c(0,0,0,-40,.6,.1,0)
axisH<-c(1.5,1.5,20,-10,.9,.6,220)
axisI<-c(.5,.5,5,10,.1,.1,20)
axisXL<-c(-40,0,0,-40,.6,.1,-40)
axisXH<-c(-10,25,25,-10,.9,.6,-10)
axisXI<-c(10,5,5,10,.1,.1,10)
nhighX<-c(-40,0,0,-40,.5,0,-40)
nlowX<-c(-8,30,30,-8,1,.6,-8)

for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\",nameV[k],".jpg"), width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot depth
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlow[k],nhigh[k]), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
			points(datAll[[k]]$depth[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
				datAll[[k]]$Mean[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], pch=19,
					col=paste0(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)	
			arrows(datAll[[k]]$depth[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$pc2.5[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$depth[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 	
			datAll[[k]]$pc97.5[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]],lwd=5, code=0)
		
		}
		
		if(i==1){
		mtext(paste(labelV[k]), side=2, cex=10, line=-25, outer=TRUE )	
		axis(2, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
		
		legend(10,nhigh[k], paste0(region.name$nameF[regInClass[[i]]$region]),
		col=paste0(region.name$rcolor[regInClass[[i]]$region]),
					pch=19, cex=10, bty="n")

		box(which="plot")
		
					
	}
	
	


#now plot air temp	
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(nlowX[k],nhighX[k]), 
			ylim=c(nlow[k],nhigh[k]), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		for(j in 1:dim(regInClass[[i]])[1]){		
		points(datAll[[k]]$MeanA[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$Mean[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], pch=19,
				col=paste(region.name$rcolor[regInClass[[i]]$region[j]]), cex=15)
		#arrows for y		
		arrows(datAll[[k]]$MeanA[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$pc2.5[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$MeanA[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 	
			datAll[[k]]$pc97.5[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#arrows for x
			arrows(datAll[[k]]$pc2.5A[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$Mean[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 
			datAll[[k]]$pc97.5A[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]], 	
			datAll[[k]]$Mean[datAll[[k]]$class==regInClass[[i]]$class[j]&datAll[[k]]$region==regInClass[[i]]$region[j]],lwd=5, code=0)
			#	
		
		}
		
		if(i==1){
		axis(2, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(1, seq(axisXL[k],axisXH[k], by=axisXI[k]),cex.axis=12, lwd.ticks=8,padj=1) 
		
		box(which="plot")
		mtext(paste(className[i]), cex=8, line=22, side=1)
	}	
	
	mtext(paste(Xlabel[k]), outer=TRUE, line=-100, cex=10, side=1 )		
	mtext("Depth (cm)", outer=TRUE, line=-100, cex=10, side=3 )	

dev.off()


}


#see if Tmin and Tpeak and Zero day correspond:

jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\var\\TminComp.jpg", width=10000,height=5000)	
ab<-layout(matrix(seq(1,16), ncol=8, byrow=TRUE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#plot peak
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(nlow[6],nhigh[6]), ylim=c(nlow[4],nhigh[4]), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datAll[[6]]$Mean[datAll[[6]]$class==i], 
				datAll[[4]]$Mean[datAll[[4]]$class==i], pch=19,
					col="wheat4", cex=15)	
			arrows(datAll[[6]]$Mean[datAll[[6]]$class==i], 
			datAll[[4]]$pc2.5[datAll[[4]]$class==i], 
			datAll[[6]]$Mean[datAll[[6]]$class==i], 	
			datAll[[4]]$pc97.5[datAll[[4]]$class==i],lwd=5, code=0)
			
			arrows(datAll[[6]]$pc2.5[datAll[[6]]$class==i], 
			datAll[[4]]$Mean[datAll[[4]]$class==i], 
			datAll[[6]]$pc97.5[datAll[[6]]$class==i], 	
			datAll[[4]]$Mean[datAll[[4]]$class==i],lwd=5, code=0)		

		mtext(paste(className[i]), cex=8, line=22, side=1)
		if(i==1){
		mtext(paste(labelV[4]), side=2, cex=10, line=-25, outer=TRUE )	
		axis(2, seq(axisL[4],axisH[4], by=axisI[4]), cex.axis=12, lwd.ticks=8, las=2)
		}
		axis(3, seq(axisL[6],axisH[6], by=axisI[6]),cex.axis=12, lwd.ticks=8) 

		box(which="plot")
		
					
	}
	
#plot zero days
	for(i in 1:length(className)){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(nlow[7],nhigh[7]), ylim=c(nlow[4],nhigh[4]), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		
			points(datAll[[7]]$Mean[datAll[[7]]$class==i], 
				datAll[[4]]$Mean[datAll[[4]]$class==i], pch=19,
					col="wheat4", cex=15)	
			arrows(datAll[[7]]$Mean[datAll[[7]]$class==i], 
			datAll[[4]]$pc2.5[datAll[[4]]$class==i], 
			datAll[[7]]$Mean[datAll[[7]]$class==i], 	
			datAll[[4]]$pc97.5[datAll[[4]]$class==i],lwd=5, code=0)
			
			arrows(datAll[[7]]$pc2.5[datAll[[7]]$class==i], 
			datAll[[4]]$Mean[datAll[[4]]$class==i], 
			datAll[[7]]$pc97.5[datAll[[7]]$class==i], 	
			datAll[[4]]$Mean[datAll[[4]]$class==i],lwd=5, code=0)		

		
		if(i==1){
		axis(2, seq(axisL[4],axisH[4], by=axisI[4]), cex.axis=12, lwd.ticks=8, las=2)
		mtext(paste(Xlabel[6]), outer=TRUE, line=-100, cex=10, side=3 )	
		mtext(paste(Xlabel[7]), outer=TRUE, line=-100, cex=10, side=1 )	
		}
		axis(1, seq(axisL[7],axisH[7], by=axisI[7]),cex.axis=12, lwd.ticks=8,padj=1) 

		box(which="plot")
		
					
	}
dev.off()	


