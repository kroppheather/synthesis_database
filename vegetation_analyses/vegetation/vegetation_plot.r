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

datVC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\vegeClass.csv")

colnames(datVC)[1]<-"siteid"

className<-c("herb barren",	"gramminoid tundra", "tussock tundra",	
			"shrub tundra",	"gramminoid wetland", "needleleaf deciduous",
				"needleleaf evergreen", "mixed conifer decidous")


Coverall<-list(Shrub.c,Moss.c, Gram.c, Ground.c, datR, Biome, datVC)
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

#get unique vegetation class

vegID<-data.frame(vegeID=unique(NFV$class))
vegID$name<-className[vegID$vegeID]
vegID<-vegID[order(vegID$vegeID),]

#get unique biome table

biomeID<-data.frame(biome=unique(NFV$biome))
biomeID$bioID<-seq(1,dim(biomeID)[1])
#turn variables into a list
varAll<-list(NFV,NTV,TmaxV,TminV,DZV)

#see how many observations by biome x region
bioReg<-aggregate(NFV$Mean, by=list(NFV$biome,NFV$region), FUN="length")
colnames(bioReg)<-c("biome","region","count")
#do not include E Russia data because only one point in region x bio
bioReg<-bioReg[bioReg$count>1,]
bioReg$nameF<-c("Alaska","Alaska","Canada", "Islands")


##########################################################
##########################################################
#######read in model results #############################
##########################################################

#biome only model
#############################################
data.name<-c("nfreeze","nthaw","Tmax","Tmin","DayZero")
dexps<-"\\[*[[:digit:]]*\\]"
dexps2<-"[^[:alpha:]]"
datSt<-list()
datQ<-list()
datC<-list()
b1p<-list()
b2p<-list()
b3p<-list()
b4p<-list()
b5p<-list()
repX<-list()
b2Sig<-list()
b3Sig<-list()
b4Sig<-list()
b5Sig<-list()
muDb<-list()
muAb<-list()
muSb<-list()
muMb<-list()

#read in model output
for(i in 1:length(data.name)){
	datSt[[i]]<-read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biome\\mod4\\",data.name[i],"Temp_mod_stats.csv"))
	datQ[[i]]<-read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biome\\mod4\\",data.name[i],"Temp_mod_quant.csv"))
	datC[[i]]<-cbind(datSt[[i]],datQ[[i]])
	#make parms vectors
	datC[[i]]$parms<-gsub(dexps,"",rownames(datC[[i]]))
	datC[[i]]$parms2<-gsub(dexps2,"",rownames(datC[[i]]))
	b1p[[i]]<-datC[[i]][datC[[i]]$parms=="b1",]
	b2p[[i]]<-datC[[i]][datC[[i]]$parms=="b2",]
	b3p[[i]]<-datC[[i]][datC[[i]]$parms=="b3",]
	b4p[[i]]<-datC[[i]][datC[[i]]$parms=="b4",]
	b5p[[i]]<-datC[[i]][datC[[i]]$parms=="b5",]
	repX[[i]]<-datC[[i]][datC[[i]]$parms=="rep.Xobs",]
	#create a flag to indicate significance
	b2Sig[[i]]<-ifelse(b2p[[i]]$X2.5.<0&b2p[[i]]$X97.5.>0,0,
				ifelse(b2p[[i]]$X2.5.>0&b2p[[i]]$X97.5.<0,0,1))
	b3Sig[[i]]<-ifelse(b3p[[i]]$X2.5.<0&b3p[[i]]$X97.5.>0,0,
				ifelse(b3p[[i]]$X2.5.>0&b3p[[i]]$X97.5.<0,0,1))	
	b4Sig[[i]]<-ifelse(b4p[[i]]$X2.5.<0&b4p[[i]]$X97.5.>0,0,
				ifelse(b4p[[i]]$X2.5.>0&b4p[[i]]$X97.5.<0,0,1))	
	b5Sig[[i]]<-ifelse(b5p[[i]]$X2.5.<0&b5p[[i]]$X97.5.>0,0,
				ifelse(b5p[[i]]$X2.5.>0&b5p[[i]]$X97.5.<0,0,1))	
				
	muDb[[i]]<-datC[[i]][datC[[i]]$parms2=="mudepth",]	
	muDb[[i]]$vegeC<-rep(seq(1,2),each=100)
	muAb[[i]]<-datC[[i]][datC[[i]]$parms2=="muair",]
	muAb[[i]]$vegeC<-rep(seq(1,2),each=100)
	muSb[[i]]<-datC[[i]][datC[[i]]$parms2=="mushrub",]
	muSb[[i]]$vegeC<-rep(seq(1,2),each=100)	
	muMb[[i]]<-datC[[i]][datC[[i]]$parms2=="mumoss",]
	muMb[[i]]$vegeC<-rep(seq(1,2),each=100)				
				
	}


roundI<-c(0,0,0,0,0)
TexM<-numeric(0)
for(i in 1:5){
	TexM[i]<-round(mean(varAll[[i]]$MeanA),roundI[i])

}
#biome region model
#############################################
data.name<-c("nfreeze","nthaw","Tmax","Tmin","DayZero")
dexps<-"\\[*[[:digit:]]*\\]"
dexps2<-"[^[:alpha:]]"
datStr<-list()
datQr<-list()
datCr<-list()
b1pr<-list()
b2pr<-list()
b3pr<-list()
b4pr<-list()
b5pr<-list()
repXr<-list()
b2Sigr<-list()
b3Sigr<-list()
b4Sigr<-list()
b5Sigr<-list()
muD<-list()
muA<-list()
muS<-list()
muM<-list()
#read in model output
for(i in 1:length(data.name)){
	datStr[[i]]<-read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biomeReg\\mod4\\",data.name[i],"Temp_mod_stats.csv"))
	datQr[[i]]<-read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\model\\vege\\biomeReg\\mod4\\",data.name[i],"Temp_mod_quant.csv"))
	datCr[[i]]<-cbind(datStr[[i]],datQr[[i]])
	#make parms vectors
	datCr[[i]]$parms<-gsub(dexps,"",rownames(datCr[[i]]))
	datCr[[i]]$parms2<-gsub(dexps2,"",rownames(datCr[[i]]))
	b1pr[[i]]<-datCr[[i]][datCr[[i]]$parms=="b1",]
	b2pr[[i]]<-datCr[[i]][datCr[[i]]$parms=="b2",]
	b3pr[[i]]<-datCr[[i]][datCr[[i]]$parms=="b3",]
	b4pr[[i]]<-datCr[[i]][datCr[[i]]$parms=="b4",]
	b5pr[[i]]<-datCr[[i]][datCr[[i]]$parms=="b5",]
	repXr[[i]]<-datCr[[i]][datCr[[i]]$parms=="rep.Xobs",]
	#create a flag to indicate significance
	b2Sigr[[i]]<-ifelse(b2pr[[i]]$X2.5.<0&b2pr[[i]]$X97.5.>0,0,
				ifelse(b2pr[[i]]$X2.5.>0&b2pr[[i]]$X97.5.<0,0,1))
	b3Sigr[[i]]<-ifelse(b3pr[[i]]$X2.5.<0&b3pr[[i]]$X97.5.>0,0,
				ifelse(b3pr[[i]]$X2.5.>0&b3pr[[i]]$X97.5.<0,0,1))	
	b4Sigr[[i]]<-ifelse(b4pr[[i]]$X2.5.<0&b4pr[[i]]$X97.5.>0,0,
				ifelse(b4pr[[i]]$X2.5.>0&b4pr[[i]]$X97.5.<0,0,1))	
	b5Sigr[[i]]<-ifelse(b5pr[[i]]$X2.5.<0&b5pr[[i]]$X97.5.>0,0,
				ifelse(b5pr[[i]]$X2.5.>0&b5pr[[i]]$X97.5.<0,0,1))	
	
	muD[[i]]<-datCr[[i]][datCr[[i]]$parms2=="mudepth",]	
	muD[[i]]$vegeCr<-rep(seq(1,4),each=100)
	muA[[i]]<-datCr[[i]][datCr[[i]]$parms2=="muair",]
	muA[[i]]$vegeCr<-rep(seq(1,4),each=100)
	muS[[i]]<-datCr[[i]][datCr[[i]]$parms2=="mushrub",]
	muS[[i]]$vegeCr<-rep(seq(1,4),each=100)	
	muM[[i]]<-datCr[[i]][datCr[[i]]$parms2=="mumoss",]
	muM[[i]]$vegeCr<-rep(seq(1,4),each=100)	
	}


depthseq<-seq(0,20,length.out=100)


airseq<-list(seq(-40,-10,length.out=100),
			seq(0,25,length.out=100),
			seq(0,25,length.out=100),
			seq(-40,-10,length.out=100),
			seq(.6,1,length.out=100),
			seq(.1,.6,length.out=100),
			seq(-40,-10,length.out=100))
			
pcseq<-seq(0,80, length.out=100)


######################################################
######################################################
#plot depth, shrub, moss by region


###############################################################
#######now look at vegetation vs region
#(NFV,NTV,TmaxV,TminV,DZV)

wb<-40
hb<-40
nlow=c(0,0,0,-40,0)
nhigh=c(1.7,1.7,25,-8,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum", "Days in zero mean")
axisL<-c(0,0,0,-40,0)
axisH<-c(1.5,1.5,20,-10,220)
axisI<-c(.5,.5,5,10,20)

for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\region",nameV[k],".jpg"), width=6500,height=6500)	

	ab<-layout(matrix(seq(1,12), ncol=4, byrow=FALSE),
				width=c(rep(lcm(wb),12)),
				height=c(rep(lcm(hb),12)))

	#depth
	for(i in 1:dim(regID)[1]){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(-1,21), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		points(varAll[[k]]$depth[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$Mean[varAll[[k]]$region==regID$region[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$depth[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$depth[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$region==regID$region[i]],lwd=5,code=0)
		mtext(paste(regID$nameF[i]), cex=8, line=20,side=2)
		axis(2, seq(axisL[k],axisH[k],by=axisI[k]), cex.axis=12, las=2, lwd.ticks=8)
		if(i==dim(regID)[1]){
			axis(1, seq(0,18, by=3), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Depth (cm)",side=1, line=20, cex=10)
			mtext(paste(labelV[k]), outer=TRUE, side=2, line=-50,cex=10)
		
		}
		box(which="plot")
	}
	#shrub
		for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		points(varAll[[k]]$shrub.pc[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$Mean[varAll[[k]]$region==regID$region[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$shrub.pc[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$shrub.pc[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$region==regID$region[i]],lwd=5,code=0)
		

		if(i==dim(regID)[1]){
			axis(1, seq(0,60, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Shrub % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}

		#moss
		for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		points(varAll[[k]]$moss.pc[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$Mean[varAll[[k]]$region==regID$region[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$moss.pc[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$moss.pc[varAll[[k]]$region==regID$region[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$region==regID$region[i]],lwd=5,code=0)
		

		if(i==dim(regID)[1]){
			axis(1, seq(0,80, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Moss % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	#air
		for(i in 1:dim(regID)[1]){
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), type="n",xlim=c(min(varAll[[k]]$pc2.5A)-1,max(varAll[[k]]$pc97.5A)+1),
				ylim=c(nlow[k],nhigh[k]), xlab=" ", 
					ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
			points(varAll[[k]]$MeanA[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$Mean[varAll[[k]]$region==regID$region[i]],
					pch=19, col="slategray3", cex=15 )	
			arrows(varAll[[k]]$MeanA[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$pc2.5[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$MeanA[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$pc97.5[varAll[[k]]$region==regID$region[i]],lwd=5,code=0)
			arrows(varAll[[k]]$pc2.5A[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$Mean[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$pc97.5A[varAll[[k]]$region==regID$region[i]],
					varAll[[k]]$Mean[varAll[[k]]$region==regID$region[i]],lwd=5,code=0)
			
			if(i==dim(regID)[1]){
				axis(1, seq(-50,50, by=5), cex.axis=12, lwd.ticks=8,padj=1)
				mtext("Air Temperature",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	dev.off()
}




###############################################################
#######now look at vegetation vs plant community type
#(NFV,NTV,TmaxV,TminV,DZV)

wb<-40
hb<-40
nlow=c(0,0,0,-40,0)
nhigh=c(1.7,1.7,25,-8,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum", "Days in zero mean")
axisL<-c(0,0,0,-40,0)
axisH<-c(1.5,1.5,20,-10,220)
axisI<-c(.5,.5,5,10,20)

for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\",nameV[k],".jpg"), width=7500,height=10000)	

	ab<-layout(matrix(seq(1,28), ncol=4, byrow=FALSE),
				width=c(rep(lcm(wb),28)),
				height=c(rep(lcm(hb),28)))

	#depth
	for(i in 1:dim(vegID)[1]){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(-1,21), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		points(varAll[[k]]$depth[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$Mean[varAll[[k]]$class==vegID$vegeID[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$depth[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$depth[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$class==vegID$vegeID[i]],lwd=5,code=0)
		mtext(paste(vegID$name[i]), cex=8, line=20,side=2)
		axis(2, seq(axisL[k],axisH[k],by=axisI[k]), cex.axis=12, las=2, lwd.ticks=8)
		if(i==dim(vegID)[1]){
			axis(1, seq(0,18, by=3), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Depth (cm)",side=1, line=20, cex=10)
			mtext(paste(labelV[k]), outer=TRUE, side=2, line=-100,cex=10)
		
		}
		box(which="plot")
	}
	#shrub
		for(i in 1:dim(vegID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		points(varAll[[k]]$shrub.pc[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$Mean[varAll[[k]]$class==vegID$vegeID[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$shrub.pc[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$shrub.pc[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$class==vegID$vegeID[i]],lwd=5,code=0)
		

		if(i==dim(vegID)[1]){
			axis(1, seq(0,80, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Shrub % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}

		#moss
		for(i in 1:dim(vegID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		points(varAll[[k]]$moss.pc[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$Mean[varAll[[k]]$class==vegID$vegeID[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$moss.pc[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$moss.pc[varAll[[k]]$class==vegID$vegeID[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$class==vegID$vegeID[i]],lwd=5,code=0)
		

		if(i==dim(vegID)[1]){
			axis(1, seq(0,80, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Moss % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	#air
		for(i in 1:dim(vegID)[1]){
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), type="n",xlim=c(min(varAll[[k]]$pc2.5A)-1,max(varAll[[k]]$pc97.5A)+1),
				ylim=c(nlow[k],nhigh[k]), xlab=" ", 
					ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
			points(varAll[[k]]$MeanA[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$Mean[varAll[[k]]$class==vegID$vegeID[i]],
					pch=19, col="slategray3", cex=15 )	
			arrows(varAll[[k]]$MeanA[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$pc2.5[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$MeanA[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$pc97.5[varAll[[k]]$class==vegID$vegeID[i]],lwd=5,code=0)
			arrows(varAll[[k]]$pc2.5A[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$Mean[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$pc97.5A[varAll[[k]]$class==vegID$vegeID[i]],
					varAll[[k]]$Mean[varAll[[k]]$class==vegID$vegeID[i]],lwd=5,code=0)
			
			if(i==dim(vegID)[1]){
				axis(1, seq(-50,50, by=10), cex.axis=12, lwd.ticks=8,padj=1)
				mtext("Air Temperature",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	dev.off()
}


###################################################################################
##########Not enough data across many vegetation communities so just look
##########at biomes
#and include model regression results

wb<-40
hb<-40
nlow=c(0,0,0,-35,0)
nhigh=c(1.7,1.7,25,0,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum", "Days in zero mean")
axisL<-c(0,0,0,-40,0)
axisH<-c(1.5,1.5,20,-10,20)
axisI<-c(.5,.5,5,10,20)

for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\biome\\",nameV[k],".jpg"), width=6500,height=4500)	

	ab<-layout(matrix(seq(1,8), ncol=4, byrow=FALSE),
				width=c(rep(lcm(wb),8)),
				height=c(rep(lcm(hb),8)))

	#depth
	for(i in 1:dim(biomeID)[1]){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(-1,21), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		#regression CI
			if(b2Sig[[k]][i]==1){
			polygon(c(depthseq, rev(depthseq)),
					c(muDb[[k]]$X2.5.[muDb[[k]]$vegeC==i], rev(muDb[[k]]$X97.5.[muDb[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(depthseq, rev(depthseq)),
					c(rep(b1p[[k]]$X2.5.[i],length(depthseq)),
					rep( b1p[[k]]$X97.5.[i],length(depthseq))),col="grey85",border=FALSE)
			
			}
		
				
		points(varAll[[k]]$depth[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$Mean[varAll[[k]]$biome==biomeID$biome[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$depth[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$depth[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$biome==biomeID$biome[i]],lwd=5,code=0)
		mtext(paste(biomeID$biome[i]), cex=8, line=20,side=2)
		axis(2, seq(axisL[k],axisH[k],by=axisI[k]), cex.axis=12, las=2, lwd.ticks=8)
		#regression line
		if(b2Sig[[k]][i]==1){
		points(depthseq,muDb[[k]]$Mean[muDb[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(depthseq,rep(b1p[[k]]$Mean[i], length(depthseq)), type="l", lwd=5, lty=2)
			}


			
		if(i==dim(biomeID)[1]){
			axis(1, seq(0,18, by=3), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Depth (cm)",side=1, line=20, cex=10)
			mtext(paste(labelV[k]), outer=TRUE, side=2, line=-50,cex=10)
		

		
		
		}
		box(which="plot")
	}
	#shrub
		for(i in 1:dim(biomeID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		
		
		if(b4Sig[[k]][i]==1){
			polygon(c(pcseq, rev(pcseq)),
					c(muSb[[k]]$X2.5.[muSb[[k]]$vegeC==i], rev(muSb[[k]]$X97.5.[muSb[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(pcseq, rev(pcseq)),
					c(rep(b1p[[k]]$X2.5.[i],length(pcseq)),
					rep( b1p[[k]]$X97.5.[i],length(pcseq))),col="grey85",border=FALSE)
			
			}
		
		
		points(varAll[[k]]$shrub.pc[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$Mean[varAll[[k]]$biome==biomeID$biome[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$shrub.pc[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$shrub.pc[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$biome==biomeID$biome[i]],lwd=5,code=0)
		
		#regression line
		if(b4Sig[[k]][i]==1){
		points(pcseq,muSb[[k]]$Mean[muSb[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(pcseq,rep(b1p[[k]]$Mean[i], length(pcseq)), type="l", lwd=5, lty=2)
			}
		if(i==dim(biomeID)[1]){
			axis(1, seq(0,60, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Shrub % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}

		#moss
		for(i in 1:dim(biomeID)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		#regression CI
		if(b5Sig[[k]][i]==1){
			polygon(c(pcseq, rev(pcseq)),
					c(muMb[[k]]$X2.5.[muMb[[k]]$vegeC==i], rev(muMb[[k]]$X97.5.[muMb[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(pcseq, rev(pcseq)),
					c(rep(b1p[[k]]$X2.5.[i],length(pcseq)),
					rep( b1p[[k]]$X97.5.[i],length(pcseq))),col="grey85",border=FALSE)
			
			}
		
		points(varAll[[k]]$moss.pc[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$Mean[varAll[[k]]$biome==biomeID$biome[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$moss.pc[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$moss.pc[varAll[[k]]$biome==biomeID$biome[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$biome==biomeID$biome[i]],lwd=5,code=0)
		
		if(b5Sig[[k]][i]==1){
		points(pcseq,muMb[[k]]$Mean[muMb[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(pcseq,rep(b1p[[k]]$Mean[i], length(pcseq)), type="l", lwd=5, lty=2)
			}
			
		if(i==dim(biomeID)[1]){
			axis(1, seq(0,80, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Moss % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	#air
		for(i in 1:dim(biomeID)[1]){
		toplotlow<-numeric(0)
		toplothigh<-numeric(0)
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), type="n",xlim=c(min(varAll[[k]]$pc2.5A)-1,max(varAll[[k]]$pc97.5A)+1),
				ylim=c(nlow[k],nhigh[k]), xlab=" ", 
					ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
			
		if(b3Sig[[k]][i]==1){
			polygon(c(airseq[[k]], rev(airseq[[k]])),
					c(muAb[[k]]$X2.5.[muAb[[k]]$vegeC==i], rev(muAb[[k]]$X97.5.[muAb[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(airseq[[k]], rev(airseq[[k]])),
					c(rep(b1p[[k]]$X2.5.[i],length(airseq[[k]])),
					rep( b1p[[k]]$X97.5.[i],length(airseq[[k]]))),col="grey85",border=FALSE)
			
			}	
			
			points(varAll[[k]]$MeanA[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$Mean[varAll[[k]]$biome==biomeID$biome[i]],
					pch=19, col="slategray3", cex=15 )	
			arrows(varAll[[k]]$MeanA[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$pc2.5[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$MeanA[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$pc97.5[varAll[[k]]$biome==biomeID$biome[i]],lwd=5,code=0)
			arrows(varAll[[k]]$pc2.5A[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$Mean[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$pc97.5A[varAll[[k]]$biome==biomeID$biome[i]],
					varAll[[k]]$Mean[varAll[[k]]$biome==biomeID$biome[i]],lwd=5,code=0)
			
		if(b3Sig[[k]][i]==1){
		points(airseq[[k]],muAb[[k]]$Mean[muAb[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(airseq[[k]],rep(b1p[[k]]$Mean[i], length(airseq[[k]])), type="l", lwd=5, lty=2)
			}
			if(i==dim(biomeID)[1]){
				axis(1, seq(-50,50, by=5), cex.axis=12, lwd.ticks=8,padj=1)
				mtext("Air Temperature",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	dev.off()
}


###################################################################################
##########Look up biome but break up biomes by geographical regions
##########at biomes


wb<-40
hb<-40
nlow=c(0,0,0,-35,0)
nhigh=c(1.7,1.7,25,0,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege","zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum", "Days in zero mean")
axisL<-c(0,0,0,-40,0)
axisH<-c(1.5,1.5,20,-10,220)
axisI<-c(.5,.5,5,10,20)

for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\VCregion\\",nameV[k],".jpg"), width=6500,height=6500)	

	ab<-layout(matrix(seq(1,16), ncol=4, byrow=FALSE),
				width=c(rep(lcm(wb),16)),
				height=c(rep(lcm(hb),16)))

	#depth
	for(i in 1:dim(bioReg)[1]){
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(-1,21), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		
		if(b2Sigr[[k]][i]==1){
			polygon(c(depthseq, rev(depthseq)),
					c(muD[[k]]$X2.5.[muD[[k]]$vegeC==i], rev(muD[[k]]$X97.5.[muD[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(depthseq, rev(depthseq)),
					c(rep(b1pr[[k]]$X2.5.[i],length(depthseq)),
					rep( b1pr[[k]]$X97.5.[i],length(depthseq))),col="grey85",border=FALSE)
			
			}
		
		
		
		points(varAll[[k]]$depth[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$Mean[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$depth[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$depth[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],lwd=5,code=0)
		#regression line		
			
		if(b2Sigr[[k]][i]==1){
		points(depthseq,muD[[k]]$Mean[muD[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(depthseq,rep(b1pr[[k]]$Mean[i], length(depthseq)), type="l", lwd=5, lty=2)
			}

			
		mtext(paste(bioReg$biome[i]), cex=8, line=20,side=2)
		mtext(paste(bioReg$nameF[i]), cex=8, line=35,side=2)
		axis(2, seq(axisL[k],axisH[k],by=axisI[k]), cex.axis=12, las=2, lwd.ticks=8)
		if(i==dim(bioReg)[1]){
			axis(1, seq(0,18, by=3), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Depth (cm)",side=1, line=20, cex=10)
			mtext(paste(labelV[k]), outer=TRUE, side=2, line=-50,cex=10)
		
		}
		box(which="plot")
	}
	#shrub
		for(i in 1:dim(bioReg)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		
		
		if(b4Sigr[[k]][i]==1){
			polygon(c(pcseq, rev(pcseq)),
					c(muS[[k]]$X2.5.[muS[[k]]$vegeC==i], rev(muS[[k]]$X97.5.[muS[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(pcseq, rev(pcseq)),
					c(rep(b1pr[[k]]$X2.5.[i],length(pcseq)),
					rep( b1pr[[k]]$X97.5.[i],length(pcseq))),col="grey85",border=FALSE)
			
			}
		
		
		points(varAll[[k]]$shrub.pc[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$Mean[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$shrub.pc[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$shrub.pc[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],lwd=5,code=0)
		
		if(b4Sigr[[k]][i]==1){
		points(pcseq,muS[[k]]$Mean[muS[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(pcseq,rep(b1pr[[k]]$Mean[i], length(pcseq)), type="l", lwd=5, lty=2)
			}

		
		if(i==dim(bioReg)[1]){
			axis(1, seq(0,60, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Shrub % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}

		#moss
		for(i in 1:dim(bioReg)[1]){
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n",xlim=c(0,80), ylim=c(nlow[k],nhigh[k]), xlab=" ", 
				ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		
		
			if(b5Sigr[[k]][i]==1){
			polygon(c(pcseq, rev(pcseq)),
					c(muM[[k]]$X2.5.[muM[[k]]$vegeC==i], rev(muM[[k]]$X97.5.[muM[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(pcseq, rev(pcseq)),
					c(rep(b1pr[[k]]$X2.5.[i],length(pcseq)),
					rep( b1pr[[k]]$X97.5.[i],length(pcseq))),col="grey85",border=FALSE)
			
			}
		
		
		
		points(varAll[[k]]$moss.pc[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$Mean[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				pch=19, col="slategray3", cex=15 )	
		arrows(varAll[[k]]$moss.pc[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$pc2.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$moss.pc[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
				varAll[[k]]$pc97.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],lwd=5,code=0)
			
		if(b5Sigr[[k]][i]==1){
		points(pcseq,muM[[k]]$Mean[muM[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(pcseq,rep(b1pr[[k]]$Mean[i], length(pcseq)), type="l", lwd=5, lty=2)
			}


		if(i==dim(bioReg)[1]){
			axis(1, seq(0,80, by=20), cex.axis=12, lwd.ticks=8,padj=1)
			mtext("Moss % cover",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	#air
		for(i in 1:dim(bioReg)[1]){
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), type="n",xlim=c(min(varAll[[k]]$pc2.5A)-1,max(varAll[[k]]$pc97.5A)+1),
				ylim=c(nlow[k],nhigh[k]), xlab=" ", 
					ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
		
		if(b3Sigr[[k]][i]==1){
			polygon(c(airseq[[k]], rev(airseq[[k]])),
					c(muA[[k]]$X2.5.[muA[[k]]$vegeC==i], rev(muA[[k]]$X97.5.[muA[[k]]$vegeC==i])),col="grey85",border=FALSE)
			
			
			}else{
			polygon(c(airseq[[k]], rev(airseq[[k]])),
					c(rep(b1pr[[k]]$X2.5.[i],length(airseq[[k]])),
					rep( b1pr[[k]]$X97.5.[i],length(airseq[[k]]))),col="grey85",border=FALSE)
			
			}		
			
			
			points(varAll[[k]]$MeanA[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$Mean[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					pch=19, col="slategray3", cex=15 )	
			arrows(varAll[[k]]$MeanA[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$pc2.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$MeanA[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$pc97.5[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],lwd=5,code=0)
			arrows(varAll[[k]]$pc2.5A[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$Mean[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$pc97.5A[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],
					varAll[[k]]$Mean[varAll[[k]]$biome==bioReg$biome[i]&varAll[[k]]$region==bioReg$region[i]],lwd=5,code=0)
		
		if(b3Sigr[[k]][i]==1){
		points(airseq[[k]],muA[[k]]$Mean[muA[[k]]$vegeC==i], type="l", lwd=5)
		}else{
			points(airseq[[k]],rep(b1pr[[k]]$Mean[i], length(airseq[[k]])), type="l", lwd=5, lty=2)
			}
			
			if(i==dim(bioReg)[1]){
				axis(1, seq(-50,50, by=5), cex.axis=12, lwd.ticks=8,padj=1)
				mtext("Air Temperature",side=1, line=20, cex=10)

		
		}
		box(which="plot")
	}
	dev.off()
}


#################################################################
######### goodness of fit plots for continuous vege #############
#################################################################

#biome only
nlow=c(0,0,0,-40,0)
nhigh=c(1.7,1.7,25,0,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege", "zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum","Days in zero mean")			
axisL<-c(0,0,0,-40,0)
axisH<-c(1.5,1.5,20,0,220)
axisI<-c(.5,.5,5,10,20)
ADD<-c(.4,.4,7,9,50)
for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\VCfitbiome",nameV[k],".jpg"), width=1000,height=1000)
	par(mai=c(3,3,3,3))
	plot(c(0,1),c(0,1), type="n", xlim=c(nlow[k],nhigh[k]), ylim=c(nlow[k],nhigh[k]),
		xlab=" ",ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
	points(varAll[[k]]$Mean[varAll[[i]]$region!=3],repX[[k]]$Mean, pch=19, cex=2)
	axis(1, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=2)
	axis(2, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=2, las=2)
	mtext(paste("Observed", labelV[k]), side=1,line=5, cex=2)
	mtext(paste("Predicted", labelV[k]), side=2,line=5, cex=2)
	fit<-lm(repX[[k]]$Mean~varAll[[k]]$Mean[varAll[[i]]$region!=3])
	abline(fit, lwd=2, lty=3)
	abline(0,1, lwd=2, col="red")
	text(nlow[k]+ADD[k],nhigh[k]-ADD[k], paste("y=",round(fit$coefficients[1],2),"+",round(fit$coefficients[2],2),"Observed"), cex=1.5)
	text(nlow[k]+ADD[k],nhigh[k]-(ADD[k]+(ADD[k]*.2)), paste("R2=",round(summary(fit)$r.squared,2)), cex=1.5)
dev.off()
}	


#biome region
nlow=c(0,0,0,-40,0)
nhigh=c(1.7,1.7,25,0,240)
nameV<-c("nfreeze_vege","nthaw_vege","Tmax_vege","Tmin_vege", "zero_vege")
labelV<-c("Freeze n-factor", "Thaw n-factor", "Soil temperature maximum", 
			"Soil temperature minimum","Days in zero mean")			
axisL<-c(0,0,0,-40,0)
axisH<-c(1.5,1.5,20,0,220)
axisI<-c(.5,.5,5,10,20)
ADD<-c(.4,.4,7,9,50)
for(k in 1:length(nlow)){
jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out\\plot\\vege\\VCfitbioReg",nameV[k],".jpg"), width=1000,height=1000)
	par(mai=c(3,3,3,3))
	plot(c(0,1),c(0,1), type="n", xlim=c(nlow[k],nhigh[k]), ylim=c(nlow[k],nhigh[k]),
		xlab=" ",ylab=" ", xaxs="i", yaxs="i", axes=FALSE)
	points(varAll[[k]]$Mean[varAll[[k]]$region!=3],repXr[[k]]$Mean, pch=19, cex=2)
	axis(1, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=2)
	axis(2, seq(axisL[k],axisH[k], by=axisI[k]), cex.axis=2, las=2)
	mtext(paste("Observed", labelV[k]), side=1,line=5, cex=2)
	mtext(paste("Predicted", labelV[k]), side=2,line=5, cex=2)
	fit<-lm(repXr[[k]]$Mean~varAll[[k]]$Mean[varAll[[k]]$region!=3])
	abline(fit, lwd=2, lty=3)
	abline(0,1, lwd=2, col="red")
	text(nlow[k]+ADD[k],nhigh[k]-ADD[k], paste("y=",round(fit$coefficients[1],2),"+",round(fit$coefficients[2],2),"Observed"), cex=1.5)
	text(nlow[k]+ADD[k],nhigh[k]-(ADD[k]+(ADD[k]*.2)), paste("R2=",round(summary(fit)$r.squared,2)), cex=1.5)
dev.off()
}	