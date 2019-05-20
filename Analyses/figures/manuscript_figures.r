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
#read in soil temperature
datS<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\soil_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\air_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in siteinfo 
siteinfo <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\siteinfo.csv")

#add shorter name to datVI
datVI$name <- c("herb barren","graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra","wetland",
				"evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")

plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\figures"	

#read in vegetation type regression results
vegeRS <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run4\\vege_mod_stats.csv")
vegeRT <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run4\\vege_mod_quant.csv")
vegeR <- cbind(vegeRS,vegeRT)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
vegeR$parms <- gsub(dexps,"", rownames(vegeR))

#world clim 2 precip in mm
datWC <- read.csv("c:\\Users\\hkropp\\Google Drive\\map_synth\\WCprecSites.csv")


#read in N factor regression results
Nstats <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\n_factor\\model\\run2\\vege_mod_stats.csv")
Nquant <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\n_factor\\model\\run2\\vege_mod_quant.csv")
Ndf <- cbind(Nstats,Nquant)

Ndf$parms <- gsub(dexps,"", rownames(Ndf))

#read in thaw days results
Thawstats <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\thaw\\model\\run1\\vege_mod_stats.csv")
Thawquant <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\thaw\\model\\run1\\vege_mod_quant.csv")

Thawdf <- cbind(Thawstats,Thawquant)
Thawdf$parms <- gsub(dexps,"", rownames(Thawdf))

#######################################
#####vegetation colors            ##### 						
#######################################	
vegeclassColors <- data.frame(vegeclass=seq(1,9),
						coli=c(rgb(77/255,77/255,77/255),
								rgb(0/255,110/255,130/255),
								rgb(160/255,130/255,180/255),
								rgb(130/255,160/255,190/255),
								rgb(250/255,120/255,80/255),
								rgb(0/255,138/255,213/255),
								rgb(50/255,80/255,10/255),
								rgb(170/255,190/255,140/255),
								rgb(240/255,240/255,50/255)))	
datVI$vegeclassColors <- vegeclassColors
#######################################
#####packages                     ##### 
#######################################

library(maps)
library(mapproj)
library(rgdal)
library(sp)
library(raster)
library(plyr)
library(mapplots)
library(rgeos)
library(lubridate)
library(imager)

##########################################################################################
##########################################################################################
################# Figure 1. create map of study sites                    #################
##########################################################################################
##########################################################################################

#######################################
#####projection                   ##### 
#######################################

laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 

#######################################
#####world data                   ##### 
#######################################

worldmap <- map("world", ylim=c(50,90), fill=TRUE)
#focus on a smaller extent
worldmap2 <- map("world", ylim=c(55,90))



#######################################
#####reproject data               ##### 
#######################################

#reproject into lambers equal aarea
#site coordinates
sites <- project(matrix(c(siteinfo$lon,siteinfo$lat),ncol=2,byrow=FALSE),laea)
#world map
world <- project(matrix(c(worldmap$x,worldmap$y), ncol=2,byrow=FALSE),laea)
world2 <- project(matrix(c(worldmap2$x,worldmap2$y), ncol=2,byrow=FALSE),laea)
#set up spatial point class
siteSP <- SpatialPoints(sites)

#######################################
#####aggregate sites              ##### 						
#######################################	
#get coordinates of sites used in vege type analysis
coord.temp <- data.frame(siteid=siteinfo$site_id,lon=siteinfo$lon,lat=siteinfo$lat)
siteP <- data.frame(siteid=unique(SoilParm$siteid))
siteP <- join(siteP,coord.temp, by="siteid",type="left")
#join vegeclass and 
siteP <- join(siteP,datV,by="siteid",type="left")
siteP <- join(siteP,vegeclassColors,by="vegeclass",type="left")

#site coordinates
sitesV <- project(matrix(c(siteP$lon,siteP$lat),ncol=2,byrow=FALSE),laea)
colnames(sitesV) <- c("X","Y")
#add back in to site info
siteP <- cbind(siteP,sitesV)

#set up spatial point class
siteSPV <- SpatialPoints(sitesV)

#####make buffers######

#get buffer around points
allBufferV <- buffer(siteSPV,100000, dissolve=TRUE)


t.cV <- list()
xV <- numeric(0)
yV <- numeric(0)
for(i in 1:19){
	t.cV[[i]]<- allBufferV@polygons[[1]]@Polygons[[i]]@labpt
	xV[i] <- t.cV[[i]][1]
	yV[i] <- t.cV[[i]][2]
	
}
bcV <- matrix(c(xV,yV),ncol=2,byrow=FALSE)
colnames(bcV) <- c("x","y")
#need to get all points contained in each buffer

#check points in each buffer
pbV <- list()
polydfV <- matrix(rep(NA,dim(bcV)[1]*dim(siteP)[1]),ncol=dim(bcV)[1])
for(i in 1:19){
	pbV[[i]] <- point.in.polygon(siteP$X,siteP$Y,allBufferV@polygons[[1]]@Polygons[[i]]@coords[,1],allBufferV@polygons[[1]]@Polygons[[i]]@coords[,2])
	polydfV[,i] <- pbV[[i]]
}
#find out which polygon each sties is in
tpolyV <- numeric()
for(i in 1:dim(siteP)[1]){
	tpolyV[i] <- which(polydfV[i,]==1)
}
#now add site info to the polygon
all.polyV <- data.frame(siteP,polyid=tpolyV)

#now summarize the total number of each vege class in each polygon
all.sumVV <- aggregate(all.polyV$siteid,by=list(all.polyV$vegeclass,all.polyV$polyid), FUN="length")
colnames(all.sumVV) <- c("vegeclass","polyid","nsites")
#get the total number of sites in each polygon
all.sumSV <- aggregate(all.sumVV$nsites,by=list(all.sumVV$polyid),FUN="sum")
colnames(all.sumSV) <- c("polyid","NpolySite")

#join two back together
all.sumVV <- join(all.sumVV,all.sumSV, by="polyid",type="left")
#calculate proportion
all.sumVV$propC <- all.sumVV$nsites/all.sumVV$NpolySite

#join vegeclass colors in
all.sumVV <- join(all.sumVV,vegeclassColors, by="vegeclass",type="left")
#join polygon coordinates
mat.bcV <- cbind(bcV,all.sumSV)
all.sumVV <- join(all.sumVV,mat.bcV,by="polyid",type="left")
#turn into a smaller  dataframe
propAllV <- data.frame(vegeclass=all.sumVV$vegeclass,x=all.sumVV$x,y=all.sumVV$y,propC=all.sumVV$propC)
xyz.allV <- make.xyz(propAllV$x,propAllV$y,propAllV$propC,propAllV$vegeclass)


#######################################
#####map of vege type sites       ##### 						
#######################################	


#######make plot ######
yseq <- seq(1,9)
wd <- 30
hd <- 30
wd2 <- 5

png(paste0(plotDI,"\\vege_site_agg.png"),width=1800,height=1000)
	a <- layout(matrix(c(1,2),ncol=2), height=c(lcm(hd),lcm(hd)), width=c(lcm(wd),lcm(wd2)))
	layout.show(a)
	#set up empty plot
	plot(world2,type="n",axes=FALSE,xlab=" ", ylab=" ",xlim=c(-3500000,3500000),ylim=c(-3500000,3500000))
	#color background
	polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(202/255,242/255,255/255,.3))
	#boundaries
	points(world, type="l", lwd=2, col="grey65")
	#continent color
	polygon(c(world[,1],rev(world[,1])), c(world[,2],rev(world[,2])),col=rgb(253/255,245/255,208/255),border=NA)
	draw.pie(xyz.allV$x,xyz.allV$y,xyz.allV$z,radius=220000,col=as.character(vegeclassColors$coli),border=NA)
	points(mat.bcV$x,mat.bcV$y,pch=19,col="white",cex=4.75)
	text(mat.bcV$x,mat.bcV$y,paste(mat.bcV$NpolySite),cex=1.5)
	#plot legend
	plot(c(0,1),c(0,1), type="n", xlim=c(0,1), ylim=c(0,10), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
		for(i in 1:9){
		polygon(c(0,0,1,1),c(yseq[i]-1,yseq[i],yseq[i],yseq[i]-1),col=as.character(vegeclassColors$coli[i]),border=NA)
	}
	axis(4,yseq-.5,rep(" ",9),lwd.ticks=2)
	mtext(datVI$name,at=yseq-.5,cex=2,line=1,side=4,las=2)
	
dev.off()	

##########################################################################################
##########################################################################################
################# Figure 2. all data & summary                           #################
##########################################################################################
##########################################################################################


#######################################
#####organize temp data for plot  ##### 						
#######################################	

#filter soil temp to only focus on sites with 20cm or less depth
datST <- datS[datS$st_depth<=20,]
#omit site 44 which has problems
datST <- datST[datST$site_id!=44,]

#get siteid actually used in analyses
sitesSubs <- unique(data.frame(site_id=SoilParm$siteid,st_depth=SoilParm$depth,wyear=SoilParm$wyear))


# set up water year
#create a leap year flag
datST$leapid<-ifelse(leap_year(datST$year)==TRUE,1,0)


datST$wdoy<-ifelse(datST$leapid==1&datST$doy<=274, datST$doy+92,
		ifelse(datST$leapid==1&datST$doy>274, datST$doy-274,
		ifelse(datST$leapid==0&datST$doy<=273,datST$doy+92,
		ifelse(datST$leapid==0&datST$doy>273,datST$doy-273,NA))))
		
datST$wyear <- ifelse(datST$leapid == 1 & datST$doy_st <  275,
				datST$year_st,
				ifelse(datST$leapid == 0 & datST$doy_st < 274,
						datST$year_st,datST$year_st+1))	
						
#join back into datST
datST <- join(datST,sitesSubs,by=c("site_id","st_depth","wyear"),type="inner")						
#add vegetation class	
datST <- join(datST,datV,by="site_id",type="left")					

#organize into a matrix with NA for each day
#get each unique depth, year, site
datI <- unique(data.frame(wyear=datST$wyear,st_depth=datST$st_depth,site_id=datST$site_id))
datI$sID <- seq(1,dim(datI)[1])

#join vegeclass

datV$site_id <- datV$siteid
datI <- join(datI,datV,by="site_id",type="left")

#join index back into datST

datST <- join(datST,datI, by=c("wyear","st_depth","site_id"),type="left")	

#create a list of dataframes where each year is filled in with NA for plotting		
dayAll <- data.frame(wdoy=seq(1,366))			

#list of all sites
Soilt <- list()
Soil <- list()

for(i in 1:dim(datI)[1]){
	Soilt[[i]] <- datST[datST$sID==i,]
	Soil[[i]] <- join(Soilt[[i]],dayAll,by="wdoy",type="full")
	Soil[[i]] <- Soil[[i]][order(Soil[[i]]$wdoy),]


}



#get a list of which lists belong to each vegeclass
listV <- list()

for(i in 1:9){
	listV[[i]] <- which(datI$vegeclass==i)
}


#get summary information
#get count of observations for each day of year and depth
datSTn <- na.omit(datST)
#create a depth ID
datSTn$depthID <- ifelse(datSTn$st_depth <= 5,1,
					ifelse(datSTn$st_depth >5 & datSTn$st_depth <= 10, 2,
					ifelse(datSTn$st_depth > 10 & datSTn$st_depth <= 15, 3, 4)))
#get the histogram of temperatures across the vegetation classes and depth

					
					#create depth and vegetation id
vegeDepth <- unique(data.frame(depthID=datSTn$depthID,vegeclass=datSTn$vegeclass))
vegeDepth$vdID <- seq(1,dim(vegeDepth)[1])

#join back into soil
datSTn <- join(datSTn, vegeDepth, by=c("depthID","vegeclass"), type="left")



#######################################
#####work with min and max        ##### 
#######################################
#organize soil output


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)

datWC$siteid <- datWC$site_id
#join world clim data
SoilParm <- join(SoilParm, datWC, by=c("siteid"), type="left")

#create unique names for air
colnames(AirParm)[1:4] <- paste0("A",colnames(AirParm)[1:4])
colnames(AirParm)[8] <- paste0("A",colnames(AirParm)[8])
#pull out each soil parm dataset:
parmV <- unique(SoilParm$parm)
parmA <- unique(AirParm$Aparm)

#pull out relevant parameters
#for analysis: TminS, TmaxS, peakWS
#and subset to relevant depths

parmVs <- c("TminS","TmaxS", "peakWS") 

parmAs <- c("TminA","TmaxA", "peakWA") 


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmVs)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmVs[i]&SoilParm$depth<=20,]
	#calculate mean
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
	#add a regression ID
	SoilL[[i]]$regID <- rep(i,dim(SoilL[[i]])[1])
}

AirL <- list()
AirMs <- numeric(0)
for(i in 1:length(parmAs)){
	AirL[[i]] <- AirParm[AirParm$Aparm==parmAs[i],]
	#calculate mean
	AirMs[i] <- round(mean(AirL[[i]]$AMean),3)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}


#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)



#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#######################################
#####figure with summary          ##### 
#######################################
#get summary

#get daily mean temp for each depth and vege class
medTDF <- aggregate(datSTn$soil_t,by=list(datSTn$wdoy,datSTn$depthID,datSTn$vegeclass),FUN="mean")
colnames(medTDF) <- c("wdoy","depthID","vegeclass","med")
pclTDF <- aggregate(datSTn$soil_t,by=list(datSTn$wdoy,datSTn$depthID,datSTn$vegeclass),FUN="quantile",prob=.25)
colnames(pclTDF) <- c("wdoy","depthID","vegeclass","pcl")
pchTDF <- aggregate(datSTn$soil_t,by=list(datSTn$wdoy,datSTn$depthID,datSTn$vegeclass),FUN="quantile",prob=.75)
colnames(pchTDF) <- c("wdoy","depthID","vegeclass","pch")




#try to represent all four pieces of information nicely
test <- c(rgb(213/255,94/255,0/255,.1),rgb(240/255,228/255,66/255,.1),
			rgb(0/255,114/255,178/255,.1),rgb(0/255,158/255,115/255,.1))
test2 <- c(rgb(213/255,94/255,0/255,.8),rgb(240/255,228/255,66/255,.8),
			rgb(0/255,114/255,178/255,.8),rgb(0/255,158/255,115/255,.8))
test3 <- c(rgb(213/255,94/255,0/255),rgb(240/255,228/255,66/255),
			rgb(0/255,114/255,178/255),rgb(0/255,158/255,115/255))	
test4 <- c(rgb(213/255,94/255,0/255,.4),rgb(240/255,228/255,66/255,.4),
			rgb(0/255,114/255,178/255,.4),rgb(0/255,158/255,115/255,.4))			
#names
name2 <- c("Herb barren", "Graminoid tundra","Tussock tundra","Short shrub tundra","Tall shrub tundra",
					"Wetland","Evergreen needleleaf boreal","Deciduous needleleaf boreal","Mixed boreal")			
			
wd1 <- 82
hd1 <- 76
wd2 <- 34
hd2 <- 20
Cyl <- 	round_any(max(maxC),50,ceiling)
#seq for x2
xh2 <- 3
xl2 <- 0
xi2 <- 1
#line width hist
llw <- 5
#x axis days
xl1 <- 0
xh1 <- 370
xi1 <- 50
#seq for y axis labels
yseq <- seq(-35,25,by=10)
#seq for couns
yseq2 <- seq(0,Cyl,by=50)
#xseq <- c(1,62,124,183,244,305)
#xlseq <- c("Oct","Dec","Feb","Apr","Jun","Aug")
xseq <- c(1,124,244,335)
xlseq <- c("Oct","Feb","Jun","Sept")
#tick width
lwt <- 9
#line axis 
alh <- 4
xalh <- 8
#cex axis
mx <- 13
#box line width
blw <- 3
#label line
llh <- 20
#y label line axis 
yllh <- 22
#label size
lx <- 14
#mean line width
mlwd <- 7
#density axis line
dcx <- 3
#box error bar width
alw <- 5
#box mean width
mlw <- 7
#sequence for min and max plot
xseqP <- c(1,2)

lgry <- rgb(165/255,165/255,165/255,.2)
	
for(i in 1:9){
	png(paste0(plotDI,"\\all\\all_data_vege_",i,".png"),width=2400,height=2200)
	layout(matrix(c(1), ncol=1, byrow=TRUE), widths=c(lcm(wd1)),
			heights=c(lcm(hd1)))

	##temperature##		
		par(mai=c(4.5,5.75,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,35),xlim=c(0,370),col="white",
				xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		for(j in 1:length(listV[[i]])){		
		
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
					col=lgry,lwd=3)
			}
	
		
		#add summary
		if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==1))!=0){
		polygon(c(pclTDF$wdoy[pclTDF$depthID==1&pclTDF$vegeclass==i],
					rev(pclTDF$wdoy[pclTDF$depthID==1&pclTDF$vegeclass==i])),
				c(pclTDF$pcl[pclTDF$depthID==1&pclTDF$vegeclass==i],
					rev(pchTDF$pch[pclTDF$depthID==1&pclTDF$vegeclass==i])),
					col=test4[1],border=NA)
		points(medTDF$wdoy[medTDF$depthID==1&medTDF$vegeclass==i],medTDF$med[medTDF$depthID==1&medTDF$vegeclass==i],
					type="l",lwd=mlwd,col=test2[1])
					
		}
		if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==2))!=0){
		polygon(c(pclTDF$wdoy[pclTDF$depthID==2&pclTDF$vegeclass==i],
					rev(pclTDF$wdoy[pclTDF$depthID==2&pclTDF$vegeclass==i])),
				c(pclTDF$pcl[pclTDF$depthID==2&pclTDF$vegeclass==i],
					rev(pchTDF$pch[pclTDF$depthID==2&pclTDF$vegeclass==i])),
					col=test4[2],border=NA)
				points(medTDF$wdoy[medTDF$depthID==2&medTDF$vegeclass==i],medTDF$med[medTDF$depthID==2&medTDF$vegeclass==i],
					type="l",lwd=mlwd,col=test2[2])			
		}
		if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==3))!=0){
		polygon(c(pclTDF$wdoy[pclTDF$depthID==3&pclTDF$vegeclass==i],
					rev(pclTDF$wdoy[pclTDF$depthID==3&pclTDF$vegeclass==i])),
				c(pclTDF$pcl[pclTDF$depthID==3&pclTDF$vegeclass==i],
					rev(pchTDF$pch[pclTDF$depthID==3&pclTDF$vegeclass==i])),
					col=test4[3],border=NA)
				points(medTDF$wdoy[medTDF$depthID==3&medTDF$vegeclass==i],medTDF$med[medTDF$depthID==3&medTDF$vegeclass==i],
					type="l",lwd=mlwd,col=test2[3])			
		}	
		if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==4))!=0){
		polygon(c(pclTDF$wdoy[pclTDF$depthID==4&pclTDF$vegeclass==i],
					rev(pclTDF$wdoy[pclTDF$depthID==4&pclTDF$vegeclass==i])),
				c(pclTDF$pcl[pclTDF$depthID==4&pclTDF$vegeclass==i],
					rev(pchTDF$pch[pclTDF$depthID==4&pclTDF$vegeclass==i])),
					col=test4[4],border=NA)
				points(medTDF$wdoy[medTDF$depthID==4&medTDF$vegeclass==i],medTDF$med[medTDF$depthID==4&medTDF$vegeclass==i],
					type="l",lwd=mlwd,col=test2[4])			
		}		
		axis(1, c(xseq,400),rep(" ",length(xseq)+1),lwd.ticks=lwt,lwd=blw)
		mtext(xlseq,at=xseq,side=1,line=xalh,cex=mx)	
		axis(2, yseq, rep(" ", length(yseq)),lwd.ticks=lwt,lwd=blw)
		mtext(yseq,at=yseq,side=2,line=alh,cex=mx,las=2)
		#box(which="plot",lwd=blw)
		mtext("Soil temperature (C)", side=2,line=yllh, cex=lx) 
		mtext("Day of water year", side=1,line=llh, cex=lx) 
		mtext(paste(name2[i]), side=3, outer=TRUE,line=-12, cex=lx) 
		
	dev.off()
}	

#plot all images
#read in all images
plotOrder <- c(1,2,4,3,5,6,7,8,9)
png(paste0(plotDI,"\\all_panel_data.png"),width=6000,height=6000)

	layout(matrix(seq(1,9),ncol=3,byrow=TRUE))
	for(i in 1:9){
		j <- plotOrder[i]
		par(mai=c(0,0,0,0))
		plot(load.image(paste0(plotDI,"\\all\\all_data_vege_",j,".png")),axes=FALSE)
	}	
		
dev.off()				



##########################################################################################
##########################################################################################
################# Figure 3. nfactor & thaw                               #################
##########################################################################################
##########################################################################################



#join vegeclass to data
Nfactor2 <- join(Nfactor,datV, by="siteid",type="left")
#create regression id
Nfactor2$regID <- ifelse(Nfactor2$parm=="Fn",1,2)


#create dataframe
#for n factor regression ids
regvegeID <- unique(data.frame(vegeclass=Nfactor2$vegeclass,regID=Nfactor2$regID))
regvegeID <- regvegeID[order(regvegeID$regID,regvegeID$vegeclass),]
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])


#join vegeclass to data
ThawParm2 <- join(ThawParm,datV, by="siteid",type="left")
vegeID <- data.frame(vegeclass=unique(ThawParm2$vegeclass))
vegeIDc <- join(vegeID,datVI,by="vegeclass",type="left")
vegeIDc <- vegeIDc[order(vegeIDc$vegeclass),]

#intercept for n factor
beta0 <- Ndf[Ndf$parms=="beta0",]
beta0 <- cbind(beta0,regvegeID)

#intercept for thaw days
Tbeta0 <- Thawdf[Thawdf$parms=="beta0",]
Tbeta0 <- cbind(Tbeta0,vegeIDc)

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

					
					
wd <-100
hd <- 60

#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)

yli <- c(0,0,75)
yhi <- c(1.5,1.7,200)
yii <- c(.5,.5,25)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#lwd of ticks
tlw <- 10
#size of x labels
axc <- 10
axc2 <- 14
#line for label num
xll <- 2

#line for units
yll1 <- 20
#line for name
yll2 <- 35
#cex of axis label
mcx <- 13
#one line width
zlw <- 9


png(paste0(plotDI,"\\intercepts_N.png"), width=3000,height=6200,
			units="px")
	layout(matrix(seq(1,4),ncol=1), width=rep(lcm(wd),4),height=c(lcm(hd),lcm(hd),lcm(hd),lcm(35)))
		#plot intercept
	
		par(mai=c(1,6,2,0))
		
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
			
		par(mai=c(1,6,1,0))
		
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

			
	par(mai=c(1,6,1,0),xpd=TRUE)
		plot(c(0,1),c(0,1), ylim=c(yli[3],yhi[3]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		
			for(j in 1:9){
				polygon(c(xseq[j]-1,xseq[j]-1,xseq[j]+1,xseq[j]+1),
						c(Tbeta0$X25.[Tbeta0$vegeclass==j],Tbeta0$X75.[Tbeta0$vegeclass==j],
							Tbeta0$X75.[Tbeta0$vegeclass==j],Tbeta0$X25.[Tbeta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[j]-1,Tbeta0$Mean[Tbeta0$vegeclass==j],
						xseq[j]+1,Tbeta0$Mean[Tbeta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[j],Tbeta0$X0.3.[Tbeta0$vegeclass==j],
						xseq[j],Tbeta0$X99.7.[Tbeta0$vegeclass==j],
						code=0, lwd=alw)
				}		
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			axis(2, seq(yli[3],yhi[3], by=yii[3]), rep(" ",length(seq(yli[3],yhi[3], by=yii[3]))),
				 lwd.ticks=tlw)
			mtext(seq(yli[3],yhi[3], by=yii[3]),at=seq(yli[3],yhi[3], by=yii[3]), side=2, line=xll,cex=axc,las=2)	
			#mtext(expression(paste("(N"[thaw],")")),side=2,line=yll1,cex=mcx)			
			mtext("Days above freezing",side=2,line=yll2,cex=mcx)
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
			
	par(mai=c(5,6,0,0))
		plot(c(0,1),c(0,1), ylim=c(-10,0), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			text(xseq,rep(-.25,length(xseq)),datVI$name2,srt=35, adj=1,cex=axc2,xpd=TRUE)
dev.off()	