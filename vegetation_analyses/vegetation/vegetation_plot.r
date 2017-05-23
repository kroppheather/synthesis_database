#######################################################
########this script reads in output from model 10 #####
########and plots data and empirical mdoel output #####
########that looks at variation across vegetation #####
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


##############################################
#########vegetation data ####################
##############################################
#siteinfo
datI<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\siteinfo.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))

#species cover
datC<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\spcov.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))
#species biomass
datB<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\spec_bio.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
#read in soil organic layer data
datS<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\soil.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				

#make some data frames with only specific info

Biome<-data.frame(biome=datI$vege_z, siteid=datI$site_id)

#distrubance
Dist<-data.frame(siteid=datI$site_id, Dtype=datI$dist_type, Dtime=datI$dist_hist )
Dist<-Dist[!is.na(Dist$Dtype)|Dist$Dtype!="undisturbed",]
				
#################################################
######### Aggregate         data ################
#################################################

#just focus on functional type right now
#get rid of any case issues
funcLC<-tolower(datC$func_type)
#get the functional type names
Funct.all<-unique(funcLC)

#set up a vector to make names consistent 


#rename to have more consistent names
#moss here is defined as any bryophyte
funcT<- ifelse(funcLC=="Tall sedges", "sedge",
		ifelse(funcLC=="tussock sedge","tussock",
		ifelse(funcLC=="organic","bare ground",
		ifelse(funcLC=="soil","bare ground",
		ifelse(funcLC=="dryas integrifolia", "evergreen shrub",
		ifelse(funcLC=="herb","forb",
		ifelse(funcLC=="sedges","sedge",
		ifelse(funcLC=="forbs","forb",
		ifelse(funcLC=="mosses","moss",
		ifelse(funcLC=="heath","evergreen shrub",
		ifelse(funcLC=="grasses","graminoid",
		ifelse(funcLC=="gramminoid","graminoid",
		ifelse(funcLC=="bare soil","bare ground",funcLC
		)))))))))))))
		
#now check unique names again
Funct.allRN<-unique(funcT)

#now create an even simple classification that is
#just tree shrub ect
simpFT<-ifelse(funcT=="moss","moss",
		ifelse(funcT=="shrub","shrub",
		ifelse(funcT=="evergreen shrub", "shrub",
		ifelse(funcT=="deciduous shrub", "shrub",
		ifelse(funcT=="litter", "litter",
		ifelse(funcT=="bare ground", "bare ground",
		ifelse(funcT=="forb", "forb",
		ifelse(funcT=="sedge", "graminoid",
		ifelse(funcT=="tussock", "graminoid",
		ifelse(funcT=="graminoid","graminoid",
		ifelse(funcT=="deciduous tree", "tree",
		ifelse(funcT=="evergreen tree", "tree",
		ifelse(funcT=="deciduous coniferous tree", "tree","other"
				)))))))))))))
				
				
perc.ftm<-aggregate(datC$perc_cover, by=list(funcT,
											datC$site_id),
					FUN="sum")
perc.sm<-aggregate(datC$perc_cover, by=list(simpFT,
											datC$site_id),
					FUN="sum")
colnames(perc.sm)<-c("FuncType", "siteid", "p.cover")	
colnames(perc.ftm)<-c("FuncType", "siteid", "p.cover")


#some sites appear to have done percent cover in a non-additive way and 
#did ground area cover
#thus scale perc.sm to be from 0-100
perc.smM<-aggregate(perc.sm$p.cover, by=list(perc.sm$siteid), FUN="sum")
colnames(perc.smM)<-c("siteid", "max")
perc.sm2<-join(perc.sm, perc.smM, by="siteid", type="left")
perc.sm2$pcov.cor<-ifelse(perc.sm2$max>100,(perc.sm2$p.cover/perc.sm2$max)*100,perc.sm2$p.cover)

#subset into data frames to join
Tree.c<-perc.sm2[perc.sm2$FuncType=="tree",]
Tree.c<-data.frame(FuncType=Tree.c$FuncType, siteid=Tree.c$siteid, tree.pc=Tree.c$pcov.cor)


Shrub.c<-perc.sm2[perc.sm2$FuncType=="shrub",]
Shrub.c<-data.frame(FuncType=Shrub.c$FuncType, siteid=Shrub.c$siteid, shrub.pc=Shrub.c$pcov.cor)

Moss.c<-perc.sm2[perc.sm2$FuncType=="moss",]
Moss.c<-data.frame(FuncType=Moss.c$FuncType, siteid=Moss.c$siteid, moss.pc=Moss.c$pcov.cor)

Ground.c<-perc.sm2[perc.sm2$FuncType=="bare ground",]
Ground.c<-data.frame(FuncType=Ground.c$FuncType, siteid=Ground.c$siteid, ground.pc=Ground.c$pcov.cor)

Gram.c<-perc.sm2[perc.sm2$FuncType=="graminoid",]
Gram.c<-data.frame(FuncType=Gram.c$FuncType, siteid=Gram.c$siteid, gram.pc=Gram.c$pcov.cor)

##############################################################
############## read in region data ###########################
##############################################################
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\region.csv")
colnames(datR)[1]<-"siteid"




Coverall<-list(Shrub.c,Moss.c, Gram.c, Ground.c, datR, Biome)
CoverC<-join_all(Coverall,by="siteid", type="left")

datNF<-join(datNF, CoverC, by="siteid", type="left")
datNT<-join(datNT, CoverC, by="siteid", type="left")
datTmax<-join(datTmax, CoverC, by="siteid", type="left")
datTmin<-join(datTmin, CoverC, by="siteid", type="left")
datDZ<-join(datDZ, CoverC, by="siteid", type="left")


###############################################################
###############################################################
#start by looking at patterns across:
#plant cover, 
#compare ice wedge vs non ice wedge in the vegetation type?
#look at biome vs plant cover


#start by looking at shrub cover, moss cover by region w/ depth
#need to subset to exclude NA 

NFV<-datNF[!is.na(datNF$moss.pc)&!is.na(datNF$shrub.pc),]
NTV<-datNT[!is.na(datNT$moss.pc)&!is.na(datNT$shrub.pc),]
TmaxV<-datTmax[!is.na(datTmax$moss.pc)&!is.na(datTmax$shrub.pc),]
TminV<-datTmin[!is.na(datTmin$moss.pc)&!is.na(datTmin$shrub.pc),]
DZV<-datDZ[!is.na(datDZ$moss.pc)&!is.na(datDZ$shrub.pc),]

##################################################
#################for plotting need to get unique region 

#only 1 observation from E russia so exclude region 3
regID<-unique(data.frame(region=DZV$region, region.name=DZV$region_name ))
regID<-regID[order(regID$region),]
regID<-regID[regID$region!=3,]

regID$regUID<-seq(1,dim(regID)[1])
regID$nameF<-c("Alaska","Canada", "Islands")


#plot depth, shrub, moss by region

#then by biome depth, shrub, moss by region

##############################################
########Freezing N factor


wb<-40
hb<-40
nlowT=0
nhighT=1.7




jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\nfreeze_region.jpg", width=6000,height=5000)	
ab<-layout(matrix(seq(1,12), ncol=4, byrow=FALSE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#depth 
#plot each region
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NFV$depth[NFV$region==regID$region[i]],NFV$Mean[NFV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NFV$depth[NFV$region==regID$region[i]],
				NFV$pc2.5[NFV$region==regID$region[i]],
				NFV$depth[NFV$region==regID$region[i]],
				NFV$pc97.5[NFV$region==regID$region[i]], lwd=5,code=0)
		axis(2, seq(0,1.5, by=.5),cex.axis=12, lwd.ticks=8)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,18, by=3),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Freezing n-factor", side=2, outer=TRUE, line=-50, cex=10)
		mtext("Depth (cm)", side=1,  line=20, cex=10)
		}
		


	}
	
#shrub 	
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NFV$shrub.pc[NFV$region==regID$region[i]],NFV$Mean[NFV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NFV$shrub.pc[NFV$region==regID$region[i]],
				NFV$pc2.5[NFV$region==regID$region[i]],
				NFV$shrub.pc[NFV$region==regID$region[i]],
				NFV$pc97.5[NFV$region==regID$region[i]], lwd=5,code=0)
		
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Shrub % Cover", side=1,  line=20, cex=10)
		}
		


	}

#moss
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NFV$moss.pc[NFV$region==regID$region[i]],NFV$Mean[NFV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NFV$moss.pc[NFV$region==regID$region[i]],
				NFV$pc2.5[NFV$region==regID$region[i]],
				NFV$moss.pc[NFV$region==regID$region[i]],
				NFV$pc97.5[NFV$region==regID$region[i]], lwd=5,code=0)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Moss % Cover", side=1,  line=20, cex=10)
		}
		


	}
#moss
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-35,-8), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NFV$MeanA[NFV$region==regID$region[i]],NFV$Mean[NFV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NFV$MeanA[NFV$region==regID$region[i]],
				NFV$pc2.5[NFV$region==regID$region[i]],
				NFV$MeanA[NFV$region==regID$region[i]],
				NFV$pc97.5[NFV$region==regID$region[i]], lwd=5,code=0)
				
		arrows(NFV$pc2.5A[NFV$region==regID$region[i]],
				NFV$Mean[NFV$region==regID$region[i]],
				NFV$pc97.5A[NFV$region==regID$region[i]],
				NFV$Mean[NFV$region==regID$region[i]], lwd=5,code=0)

		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(-30,-10, by=10),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Air T min", side=1,  line=20, cex=10)
		}
		


	}
dev.off()



#################################################
########thawing N factor


wb<-40
hb<-40
nlowT=0
nhighT=1.7




jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\nthaw_region.jpg", width=6000,height=5000)	
ab<-layout(matrix(seq(1,12), ncol=4, byrow=FALSE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#depth 
#plot each region
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NTV$depth[NTV$region==regID$region[i]],NTV$Mean[NTV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NTV$depth[NTV$region==regID$region[i]],
				NTV$pc2.5[NTV$region==regID$region[i]],
				NTV$depth[NTV$region==regID$region[i]],
				NTV$pc97.5[NTV$region==regID$region[i]], lwd=5,code=0)
		axis(2, seq(0,1.5, by=.5),cex.axis=12, lwd.ticks=8)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,18, by=3),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Freezing n-factor", side=2, outer=TRUE, line=-50, cex=10)
		mtext("Depth (cm)", side=1,  line=20, cex=10)
		}
		


	}
	
#shrub 	
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NTV$shrub.pc[NTV$region==regID$region[i]],NTV$Mean[NTV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NTV$shrub.pc[NTV$region==regID$region[i]],
				NTV$pc2.5[NTV$region==regID$region[i]],
				NTV$shrub.pc[NTV$region==regID$region[i]],
				NTV$pc97.5[NTV$region==regID$region[i]], lwd=5,code=0)
		
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Shrub % Cover", side=1,  line=20, cex=10)
		}
		


	}

#moss
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NTV$moss.pc[NTV$region==regID$region[i]],NTV$Mean[NTV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NTV$moss.pc[NTV$region==regID$region[i]],
				NTV$pc2.5[NTV$region==regID$region[i]],
				NTV$moss.pc[NTV$region==regID$region[i]],
				NTV$pc97.5[NTV$region==regID$region[i]], lwd=5,code=0)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Moss % Cover", side=1,  line=20, cex=10)
		}
		


	}
#air
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,20), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(NTV$MeanA[NTV$region==regID$region[i]],NTV$Mean[NTV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(NTV$MeanA[NTV$region==regID$region[i]],
				NTV$pc2.5[NTV$region==regID$region[i]],
				NTV$MeanA[NTV$region==regID$region[i]],
				NTV$pc97.5[NTV$region==regID$region[i]], lwd=5,code=0)
				
		arrows(NTV$pc2.5A[NTV$region==regID$region[i]],
				NTV$Mean[NTV$region==regID$region[i]],
				NTV$pc97.5A[NTV$region==regID$region[i]],
				NTV$Mean[NTV$region==regID$region[i]], lwd=5,code=0)

		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,20, by=10),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Air T max", side=1,  line=20, cex=10)
		}
		


	}
dev.off()



#################################################
########tmax


wb<-40
hb<-40
nlowT=0
nhighT=20




jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\tmax_region.jpg", width=6000,height=5000)	
ab<-layout(matrix(seq(1,12), ncol=4, byrow=FALSE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#depth 
#plot each region
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TmaxV$depth[TmaxV$region==regID$region[i]],TmaxV$Mean[TmaxV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TmaxV$depth[TmaxV$region==regID$region[i]],
				TmaxV$pc2.5[TmaxV$region==regID$region[i]],
				TmaxV$depth[TmaxV$region==regID$region[i]],
				TmaxV$pc97.5[TmaxV$region==regID$region[i]], lwd=5,code=0)
		axis(2, seq(0,15, by=5),cex.axis=12, lwd.ticks=8, las=2)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,18, by=3),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Freezing n-factor", side=2, outer=TRUE, line=-50, cex=10)
		mtext("Depth (cm)", side=1,  line=20, cex=10)
		}
		


	}
	
#shrub 	
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TmaxV$shrub.pc[TmaxV$region==regID$region[i]],TmaxV$Mean[TmaxV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TmaxV$shrub.pc[TmaxV$region==regID$region[i]],
				TmaxV$pc2.5[TmaxV$region==regID$region[i]],
				TmaxV$shrub.pc[TmaxV$region==regID$region[i]],
				TmaxV$pc97.5[TmaxV$region==regID$region[i]], lwd=5,code=0)
		
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Shrub % Cover", side=1,  line=20, cex=10)
		}
		


	}

#moss
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TmaxV$moss.pc[TmaxV$region==regID$region[i]],TmaxV$Mean[TmaxV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TmaxV$moss.pc[TmaxV$region==regID$region[i]],
				TmaxV$pc2.5[TmaxV$region==regID$region[i]],
				TmaxV$moss.pc[TmaxV$region==regID$region[i]],
				TmaxV$pc97.5[TmaxV$region==regID$region[i]], lwd=5,code=0)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Moss % Cover", side=1,  line=20, cex=10)
		}
		


	}
#air
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,20), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TmaxV$MeanA[TmaxV$region==regID$region[i]],TmaxV$Mean[TmaxV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TmaxV$MeanA[TmaxV$region==regID$region[i]],
				TmaxV$pc2.5[TmaxV$region==regID$region[i]],
				TmaxV$MeanA[TmaxV$region==regID$region[i]],
				TmaxV$pc97.5[TmaxV$region==regID$region[i]], lwd=5,code=0)
				
		arrows(TmaxV$pc2.5A[TmaxV$region==regID$region[i]],
				TmaxV$Mean[TmaxV$region==regID$region[i]],
				TmaxV$pc97.5A[TmaxV$region==regID$region[i]],
				TmaxV$Mean[TmaxV$region==regID$region[i]], lwd=5,code=0)

		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,20, by=10),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Air T max", side=1,  line=20, cex=10)
		}
		


	}
dev.off()



###########################################
########Tmin



wb<-40
hb<-40
nlowT=-35
nhighT=0




jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\tmin_region.jpg", width=6000,height=5000)	
ab<-layout(matrix(seq(1,12), ncol=4, byrow=FALSE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#depth 
#plot each region
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TminV$depth[TminV$region==regID$region[i]],TminV$Mean[TminV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TminV$depth[TminV$region==regID$region[i]],
				TminV$pc2.5[TminV$region==regID$region[i]],
				TminV$depth[TminV$region==regID$region[i]],
				TminV$pc97.5[TminV$region==regID$region[i]], lwd=5,code=0)
		axis(2, seq(-35,-5, by=5),cex.axis=12, lwd.ticks=8, las=2)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,18, by=3),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Tmin ", side=2, outer=TRUE, line=-50, cex=10)
		mtext("Depth (cm)", side=1,  line=20, cex=10)
		}
		


	}
	
#shrub 	
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TminV$shrub.pc[TminV$region==regID$region[i]],TminV$Mean[TminV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TminV$shrub.pc[TminV$region==regID$region[i]],
				TminV$pc2.5[TminV$region==regID$region[i]],
				TminV$shrub.pc[TminV$region==regID$region[i]],
				TminV$pc97.5[TminV$region==regID$region[i]], lwd=5,code=0)
		
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Shrub % Cover", side=1,  line=20, cex=10)
		}
		


	}

#moss
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TminV$moss.pc[TminV$region==regID$region[i]],TminV$Mean[TminV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TminV$moss.pc[TminV$region==regID$region[i]],
				TminV$pc2.5[TminV$region==regID$region[i]],
				TminV$moss.pc[TminV$region==regID$region[i]],
				TminV$pc97.5[TminV$region==regID$region[i]], lwd=5,code=0)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Moss % Cover", side=1,  line=20, cex=10)
		}
		


	}
#air
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-35,0), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(TminV$MeanA[TminV$region==regID$region[i]],TminV$Mean[TminV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(TminV$MeanA[TminV$region==regID$region[i]],
				TminV$pc2.5[TminV$region==regID$region[i]],
				TminV$MeanA[TminV$region==regID$region[i]],
				TminV$pc97.5[TminV$region==regID$region[i]], lwd=5,code=0)
				
		arrows(TminV$pc2.5A[TminV$region==regID$region[i]],
				TminV$Mean[TminV$region==regID$region[i]],
				TminV$pc97.5A[TminV$region==regID$region[i]],
				TminV$Mean[TminV$region==regID$region[i]], lwd=5,code=0)

		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(-30,-5, by=5),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Air T max", side=1,  line=20, cex=10)
		}
		


	}
dev.off()


###########################################
########Zero days



wb<-40
hb<-40
nlowT=0
nhighT=240




jpeg("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\zero_region.jpg", width=6000,height=5000)	
ab<-layout(matrix(seq(1,12), ncol=4, byrow=FALSE),
			width=c(lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),lcm(wb),
			lcm(wb),lcm(wb),lcm(wb),lcm(wb)),

			height=c(lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),lcm(hb),
			lcm(hb),lcm(hb),lcm(hb),lcm(hb)))

#depth 
#plot each region
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-1,21), ylim=c(nlowT,nhighT), axes=FALSE,
		 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(DZV$depth[DZV$region==regID$region[i]],DZV$Mean[DZV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(DZV$depth[DZV$region==regID$region[i]],
				DZV$pc2.5[DZV$region==regID$region[i]],
				DZV$depth[DZV$region==regID$region[i]],
				DZV$pc97.5[DZV$region==regID$region[i]], lwd=5,code=0)
		axis(2, seq(0,220, by=20),cex.axis=12, lwd.ticks=8, las=2)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,18, by=3),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Days in Zero model ", side=2, outer=TRUE, line=-50, cex=10)
		mtext("Depth (cm)", side=1,  line=20, cex=10)
		}
		


	}
	
#shrub 	
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(DZV$shrub.pc[DZV$region==regID$region[i]],DZV$Mean[DZV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(DZV$shrub.pc[DZV$region==regID$region[i]],
				DZV$pc2.5[DZV$region==regID$region[i]],
				DZV$shrub.pc[DZV$region==regID$region[i]],
				DZV$pc97.5[DZV$region==regID$region[i]], lwd=5,code=0)
		
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Shrub % Cover", side=1,  line=20, cex=10)
		}
		


	}

#moss
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(0,80), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(DZV$moss.pc[DZV$region==regID$region[i]],DZV$Mean[DZV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(DZV$moss.pc[DZV$region==regID$region[i]],
				DZV$pc2.5[DZV$region==regID$region[i]],
				DZV$moss.pc[DZV$region==regID$region[i]],
				DZV$pc97.5[DZV$region==regID$region[i]], lwd=5,code=0)
		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(0,60, by=20),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Moss % Cover", side=1,  line=20, cex=10)
		}
		


	}
#air
	for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(-35,0), ylim=c(nlowT,nhighT), axes=FALSE,
			 xlab=" ", ylab=" ", xaxs="i", yaxs="i")
		points(DZV$MeanA[DZV$region==regID$region[i]],DZV$Mean[DZV$region==regID$region[i]],
				pch=19, cex=15, col="lavenderblush4")
		arrows(DZV$MeanA[DZV$region==regID$region[i]],
				DZV$pc2.5[DZV$region==regID$region[i]],
				DZV$MeanA[DZV$region==regID$region[i]],
				DZV$pc97.5[DZV$region==regID$region[i]], lwd=5,code=0)
				
		arrows(DZV$pc2.5A[DZV$region==regID$region[i]],
				DZV$Mean[DZV$region==regID$region[i]],
				DZV$pc97.5A[DZV$region==regID$region[i]],
				DZV$Mean[DZV$region==regID$region[i]], lwd=5,code=0)

		box(which="plot")
		if(i==dim(regID)[1]){
		axis(1, seq(-30,-5, by=5),cex.axis=12, lwd.ticks=8, padj=1)
		mtext("Air T min", side=1,  line=20, cex=10)
		}
		


	}
dev.off()




#################################################################
######try comparing any level of ice disturbance across vege type or region









