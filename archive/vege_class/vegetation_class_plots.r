##########################################################
########Vegetation classification analysis     ###########
########Heather Kropp started August 2017      ###########
##########################################################
##########################################################
### This script plots analyses for vegetation class    ###
### simple model to look at patterns in                ###
### air and shallow soil temperature coupling          ###
##########################################################
##########################################################


##########################################
##first grab soil temperature parameters##
##########################################

#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")


#read in vegetation class data
#note sites 199-222 not confirmed yet

datVC <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\vege_class\\new_class.csv")

#make sub table
newC <- data.frame(siteid=datVC$siteid, Vclass=datVC$new.class)

#join vegetation class to each parameter table
NfactorV <- join(Nfactor, newC, by="siteid", type="left")
SoilParmV <- join(SoilParm, newC, by="siteid", type="left")

#check to see how many observations in each vege class
ParmsCount <- aggregate(SoilParmV$Mean[SoilParmV$parm=="TmaxS"],
						by=list(SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]), FUN="length")
#see how many sites are unique
SiteCountL <-unique(data.frame(siteid=SoilParmV$siteid[SoilParmV$parm=="TmaxS"],
								Vclass=SoilParmV$Vclass[SoilParmV$parm=="TmaxS"]))
#count how many sites in each class
SiteCount <- aggregate(SiteCountL$siteid, by=list(SiteCountL$Vclass), FUN="length")

#make seperate data frames fore each soil parameter

datNF<-NfactorV [NfactorV $parm=="Fn",]
datNT<-NfactorV [NfactorV $parm=="Tn",]
datTmax<-SoilParmV[SoilParmV$parm=="TmaxS",]
datTmin<-SoilParmV[SoilParmV$parm=="TminS",]
datDZ<-SoilParmV[SoilParmV$parm=="DayZero",]
datPS<-SoilParmV[SoilParmV$parm=="peakSS",]
datPW<-SoilParmV[SoilParmV$parm=="peakWS",]
datTaveS <-SoilParmV[SoilParmV$parm=="T.aveS1",]

#now seperate out air to match
datTmaxA<-AirParm[AirParm$parm=="TmaxA",]
colnames(datTmaxA)[1:4]<-paste0(colnames(datTmaxA)[1:4],"A")
datTminA<-AirParm[AirParm$parm=="TminA",]
colnames(datTminA)[1:4]<-paste0(colnames(datTminA)[1:4],"A")
datPSA<-AirParm[AirParm$parm=="peakSA",]
colnames(datPSA)[1:4]<-paste0(colnames(datPSA)[1:4],"A")
datPWA<-AirParm[AirParm$parm=="peakWA",]
colnames(datPWA)[1:4]<-paste0(colnames(datPWA)[1:4],"A")
datTaveA<-AirParm[AirParm$parm=="T.aveA1",]
colnames(datTaveA)[1:4]<-paste0(colnames(datTaveA)[1:4],"A")


#now combine air measure with matching parm
datNF<-join(datNF, datTminA, by=c("siteid","height","wyear"), type="left")
datNT<-join(datNT, datTmaxA, by=c("siteid","height","wyear"), type="left")

datTmax<-join(datTmax, datTmaxA, by=c("siteid","wyear"), type="left")
datTmin<-join(datTmin, datTminA, by=c("siteid","wyear"), type="left")

datPS<-join(datPS,datPSA,by=c("siteid","wyear"), type="left")
datPW<-join(datPW,datPWA,by=c("siteid","wyear"), type="left")

datDZ<-join(datDZ,datTminA, by=c("siteid","wyear"), type="left")

datAve <- join(datTaveS,datTaveA, by=c("siteid","wyear"), type="left")

datAll<-list(datNF,datNT,datTmax,datTmin,datPS,datPW,datDZ, datAve)

#get the average Air value for covariate centering
roundI<-c(0,0,0,0,2,2,0,0)
TexM<-numeric(0)
for(i in 1:8){
	TexM[i]<-round(mean(datAll[[i]]$MeanA),roundI[i])

}

data.name<-c("nfreeze","nthaw","Tmax","Tmin","Peakmax","Peakmin","DayZero", "Tave")

#sequences for plotting mu

depthseq<-seq(0,20,length.out=100)


airseq<-list(seq(-40,-10,length.out=100),
			seq(0,25,length.out=100),
			seq(0,25,length.out=100),
			seq(-40,-10,length.out=100),
			seq(.6,1,length.out=100),
			seq(.1,.6,length.out=100),
			seq(-40,-10,length.out=100))





##########################################
##first grab soil temperature parameters##
##########################################
data.name<-c("nfreeze","nthaw","Tmax","Tmin","Peakmax","Peakmin","DayZero")
dexps<-"\\[*[[:digit:]]*\\]"
dexps2<-"[^[:alpha:]]"
datSt<-list()
datQ<-list()
datC<-list()
b1p<-list()
b2p<-list()
b3p<-list()
repX<-list()
b2Sig<-list()
b3Sig<-list()
muD<-list()
muA<-list()
#read in model output
for(i in 1:length(data.name)){
	datSt[[i]]<-read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\",data.name[i],"Temp_mod_stats.csv"))
	datQ[[i]]<-read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\",data.name[i],"Temp_mod_quant.csv"))
	datC[[i]]<-cbind(datSt[[i]],datQ[[i]])
	#make parms vectors
	datC[[i]]$parms<-gsub(dexps,"",rownames(datC[[i]]))
	datC[[i]]$parms2<-gsub(dexps2,"",rownames(datC[[i]]))
	b1p[[i]]<-datC[[i]][datC[[i]]$parms=="b1",]
	b2p[[i]]<-datC[[i]][datC[[i]]$parms=="b2",]
	b3p[[i]]<-datC[[i]][datC[[i]]$parms=="b3",]
	repX[[i]]<-datC[[i]][datC[[i]]$parms=="rep.Xobs",]
	#create a flag to indicate significance
	b2Sig[[i]]<-ifelse(b2p[[i]]$X2.5.<0&b2p[[i]]$X97.5.>0,0,
				ifelse(b2p[[i]]$X2.5.>0&b2p[[i]]$X97.5.<0,0,1))
	b3Sig[[i]]<-ifelse(b3p[[i]]$X2.5.<0&b3p[[i]]$X97.5.>0,0,
				ifelse(b3p[[i]]$X2.5.>0&b3p[[i]]$X97.5.<0,0,1))	
				
				
	muD[[i]]<-datC[[i]][datC[[i]]$parms2=="mudepth",]	
	muD[[i]]$vegeC<-rep(seq(1,9),each=100)
	muA[[i]]<-datC[[i]][datC[[i]]$parms2=="muair",]
	muA[[i]]$vegeC<-rep(seq(1,9),each=100)
	}
	
	
###########################################################
##plot regressions across each vegetation type	
wb<-35
hb<-35
nlow=c(0,0,0,-40,.6,0,0)
nhigh=c(1.7,1.7,25,0,1,.6,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","peakMax","peakMin", "zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum (C)", 
			"Soil temperature minimum (C)","Soil temperature maximum time",
			"Soil temperature minimum time","Days in zero mean")
Xlabel<-c("Air temperature minimum (C)","Air temperature maximum (C)",
		"Air temperature maximum (C)","Air temperature minimum (C)",
		"Air temperature maximum timing","Air temperature minimum timing","Air temperature minimum (C)")			
Nclass=9
VclassNames <- c("herb-barren", "gramminoid tundra", "tussock tundra", "short shrub tundra",
					"tall shrub tundra", "wetlands", "evergreen needleaf", 
					"deciduous needleleaf", "mixed boreal")
		
axisL<-c(0,0,0,-40,.6,.1,0)
axisH<-c(1.5,1.5,20,-10,.9,.6,220)
axisI<-c(.5,.5,5,10,.1,.1,40)
axisXL<-c(-40,0,0,-40,.6,.1,-40)
axisXH<-c(-10,25,25,-10,.9,.6,-10)
axisXLP<-c(-35,5,5,-35,.65,.15,-35)
axisXHP<-c(-15,25,25,-15,.95,.55,-15)

axisXI<-c(5,5,5,5,.1,.1,10)



nhighX<-c(-8,0,0,-8,.5,1,-8)
nlowX<-c(-40,30,30,-40,0,.6,-40)	
txoff<- c(.1,.1,1,1,.05,.05,2)
	
for(i in 1:length(data.name)){
	jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\analysis_plot\\",nameV[i], ".jpg"),
			, width=10000,height=5000)	
ab<-layout(matrix(seq(1,Nclass*2), ncol=Nclass, byrow=TRUE), width=rep(lcm(wb), Nclass*2),
				height=rep(lcm(hb), Nclass*2))
	#plot the depth first
	for(j in 1:Nclass){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1),type="n", xlim=c(-1,21), ylim=c(nlow[i],nhigh[i]), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		if(b2Sig[[i]][j]==1){
			polygon(c(depthseq, rev(depthseq)),
					c(muD[[i]]$X2.5.[muD[[i]]$vegeC==j], rev(muD[[i]]$X97.5.[muD[[i]]$vegeC==j])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(depthseq, rev(depthseq)),
					c(rep(b1p[[i]]$X2.5.[j],length(depthseq)),
					rep( b1p[[i]]$X97.5.[j],length(depthseq))),col="grey85",border=FALSE)
			
			}		
		
		points(datAll[[i]]$depth[datAll[[i]]$Vclass==j],datAll[[i]]$Mean[datAll[[i]]$Vclass==j],
				pch=19, cex=10, col="tomato3")
		arrows(datAll[[i]]$depth[datAll[[i]]$Vclass==j],datAll[[i]]$pc2.5[datAll[[i]]$Vclass==j],
				datAll[[i]]$depth[datAll[[i]]$Vclass==j],datAll[[i]]$pc97.5[datAll[[i]]$Vclass==j],
				lwd=5, code=0)	

			if(b2Sig[[i]][j]==1){
		points(depthseq,muD[[i]]$Mean[muD[[i]]$vegeC==j], type="l", lwd=5)
		}else{
			points(depthseq,rep(b1p[[i]]$Mean[j], length(depthseq)), type="l", lwd=5, lty=2)
			}

			if(j==1){
		mtext(paste(labelV[i]), side=2, cex=12, line=-25, outer=TRUE )	
		axis(2, seq(axisL[i],axisH[i], by=axisI[i]), cex.axis=12, lwd.ticks=8, las=2)
		}
	axis(3, seq(0,15, by=5),cex.axis=12, lwd.ticks=8) 
		mtext("Depth (cm)", outer=TRUE, line=-120, cex=12, side=3 )	
		mtext(paste(VclassNames[j]),  line=20, cex=8, side=3 )		
		box(which="plot")
	
	
	}
		for(j in 1:Nclass){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1),type="n", xlim=c(axisXL[i],axisXH[i]), ylim=c(nlow[i],nhigh[i]), axes=FALSE,
			xlab=" ",ylab=" ", xaxs="i", yaxs="i")
		if(b3Sig[[i]][j]==1){
			polygon(c(airseq[[i]], rev(airseq[[i]])),
					c(muA[[i]]$X2.5.[muA[[i]]$vegeC==j], rev(muA[[i]]$X97.5.[muA[[i]]$vegeC==j])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(airseq[[i]], rev(airseq[[i]])),
					c(rep(b1p[[i]]$X2.5.[j],length(airseq[[i]])),
					rep(b1p[[i]]$X97.5.[j],length(airseq[[i]]))),col="grey85",border=FALSE)
			
			}	
			abline(v=TexM[i], lwd=5, lty=3, col="royalblue2")
			
			points(datAll[[i]]$MeanA[datAll[[i]]$Vclass==j],datAll[[i]]$Mean[datAll[[i]]$Vclass==j],
				pch=19, cex=10, col="tomato3")
			arrows(datAll[[i]]$MeanA[datAll[[i]]$Vclass==j],datAll[[i]]$pc2.5[datAll[[i]]$Vclass==j],
				datAll[[i]]$MeanA[datAll[[i]]$Vclass==j],datAll[[i]]$pc97.5[datAll[[i]]$Vclass==j],
				lwd=5, code=0)
			arrows(datAll[[i]]$pc2.5A[datAll[[i]]$Vclass==j],datAll[[i]]$Mean[datAll[[i]]$Vclass==j],
				datAll[[i]]$pc97.5A[datAll[[i]]$Vclass==j],datAll[[i]]$Mean[datAll[[i]]$Vclass==j],
				lwd=5, code=0)
				
			if(b3Sig[[i]][j]==1){
		points(airseq[[i]],muA[[i]]$Mean[muA[[i]]$vegeC==j], type="l", lwd=5)
		}else{
			points(airseq[[i]],rep(b1p[[i]]$Mean[j], length(airseq[[i]])), type="l", lwd=5, lty=2)
			}
				
		box(which="plot")
	if(j==1){
		axis(2, seq(axisL[i],axisH[i], by=axisI[i]), cex.axis=12, lwd.ticks=8, las=2)
		mtext(paste(Xlabel[i]), cex=12, outer=TRUE,line=-120, side=1)
		}
		axis(1, seq(axisXLP[i],axisXHP[i], by=axisXI[i]),cex.axis=12, lwd.ticks=8,padj=1) 
		

		
	}	
		


	dev.off()
	}
	
	
#######################################################################
## calculate the R2:
#now make a goodness of fit plot
nlow=c(0,0,0,-40,.6,0,0)
nhigh=c(1.7,1.7,25,0,1,.6,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","peakMax","peakMin", "zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum","Soil temperature maximum time",
			"Soil temperature minimum time","Days in zero mean")			
axisL<-c(0,0,0,-40,.5,0,0)
axisH<-c(1.5,1.5,20,0,1,.6,220)
axisI<-c(.5,.5,5,10,.1,.1,20)
ADD<-c(.4,.4,7,9,.12,.12,50)
for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\shallow\\vege_class\\model1\\analysis_plot\\fit",nameV[k],".jpg"), width=1000,height=1000)
	par(mai=c(3,3,3,3))
	plot(c(0,1),c(0,1), type="n", xlim=c(nlow[k],nhigh[k]), ylim=c(nlow[k],nhigh[k]),
		xlab=" ",ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
	points(datAll[[k]]$Mean,repX[[k]]$Mean, pch=19, cex=2)
	axis(1, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=2)
	axis(2, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=2, las=2)
	mtext(paste("Observed", labelV[k]), side=1,line=5, cex=2)
	mtext(paste("Predicted", labelV[k]), side=2,line=5, cex=2)
	fit<-lm(repX[[k]]$Mean~datAll[[k]]$Mean)
	abline(fit, lwd=2, lty=3)
	abline(0,1, lwd=2, col="red")
	text(nlow[k]+ADD[k],nhigh[k]-ADD[k], paste("y=",round(fit$coefficients[1],2),"+",round(fit$coefficients[2],2),"Observed"), cex=1.5)
	text(nlow[k]+ADD[k],nhigh[k]-(ADD[k]+(ADD[k]*.2)), paste("R2=",round(summary(fit)$r.squared,2)), cex=1.5)
dev.off()
}
	
	

#####################################################################
#### do a simple comparison to see if cedible intervals overlap for significance
#### pairwise differences need to be calculated in code but don't have time to 
#### rerun before ATM
	
	b2Sig<-list()
	for(k in 1:3){
	Sig <- matrix(rep(NA,9*9), ncol=9)
	colnames(Sig)<-VclassNames 
	rownames(Sig)<-VclassNames 
	#create pair wise matrix
	for(i in 1:9){
		for(j in 1:9){
		Sig[i,j]<- ifelse(b2p[[k]]$Mean[i]<b2p[[k]]$X2.5[j]|b2p[[k]]$Mean[i]>b2p[[k]]$X97.5[j],1,0)
		
		
		}
	}
	b2Sig[[k]]<-Sig
	}
	
	
	
#make a legend plot

plot(c(0,10), c(0,10), axes=FALSE, xlab=" ", ylab=" ", type="n")

legend(1,10, c("significant multiple regression slope", "non-sigificant multiple regression slope ", "regression mean credible interval", "intercept for centered model"),
			col=c("black","black","grey85", "royalblue2"), pch=c(NA,NA,15,NA), lty=c(1,2,NA,3), lwd=2, bty="n", cex=2)
			
			
			
#####################################################################
#########look at patterns in shallow surface temperature
plot()

			
			