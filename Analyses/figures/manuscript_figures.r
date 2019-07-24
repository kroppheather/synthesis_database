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
datS<-read.csv("z:\\projects\\synthesis\\run1\\Tsoil_model.csv")
#read in air temperature
datA<-read.csv("z:\\projects\\synthesis\\run1\\Tair_model.csv")
#read in siteinfo 
siteinfo <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\siteinfo.csv")

#add shorter name to datVI
datVI$name <- c("herb barren","graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra","wetland",
				"evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")

plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\figures"	

#read in vegetation type regression results
vegeRS <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run6\\vege_mod_stats.csv")
vegeRT <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\model_all\\run6\\vege_mod_quant.csv")
vegeR <- cbind(vegeRS,vegeRT)

#pull out parm names
dexps <- "\\[*[[:digit:]]*\\]"
vegeR$parms <- gsub(dexps,"", rownames(vegeR))
vegeR$parms2 <- gsub("\\d","",gsub("\\W","",rownames(vegeR)))

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

#read in average pattern results
patternStat <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\pattern\\model\\run1\\pattern_mod_stats.csv")
patternQuant <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\vege_type\\pattern\\model\\run1\\pattern_mod_quant.csv")
patternDF <- cbind(patternStat,patternQuant )
patternDF$parms <- gsub(dexps,"", rownames(patternDF))

#read in info about sites for data table
tabA <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\air_obs_summary.csv")
tabS <- read.csv("c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\soil_obs_summary.csv")



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
								rgb(240/255,240/255,50/255)),
						colt1=c(rgb(77/255,77/255,77/255,.1),
								rgb(0/255,110/255,130/255,.1),
								rgb(160/255,130/255,180/255,.1),
								rgb(130/255,160/255,190/255,.1),
								rgb(250/255,120/255,80/255,.1),
								rgb(0/255,138/255,213/255,.1),
								rgb(50/255,80/255,10/255,.1),
								rgb(170/255,190/255,140/255,.1),
								rgb(240/255,240/255,50/255,.1)), 
						colt2=c(rgb(77/255,77/255,77/255,.2),
								rgb(0/255,110/255,130/255,.2),
								rgb(160/255,130/255,180/255,.2),
								rgb(130/255,160/255,190/255,.2),
								rgb(250/255,120/255,80/255,.2),
								rgb(0/255,138/255,213/255,.2),
								rgb(50/255,80/255,10/255,.2),
								rgb(170/255,190/255,140/255,.2),
								rgb(240/255,240/255,50/255,.2))								
								)	
datVI$vegeclassColors <- vegeclassColors$coli

#add a color for height
heightCols <- data.frame(vegeH=c(1,2,3), heightName=c("short","mixed","tall"),
						colsH=c(rgb(10/255,100/255,160/255),
						rgb(170/255,190/255,140/255),
						rgb(250/255,40/255,0/255)))
								
vegeHeight <- data.frame(vegeclass=seq(1,9),vegeH=c(1,1,2,1,3,2,3,3,3))

#join height info with vegeclass
vegeHeight <- join(vegeHeight,heightCols,by="vegeH",type="left")

#join into vegeclass colors
vegeclassColors <- join(vegeclassColors,vegeHeight,by="vegeclass",type="left")

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
################# Figure 1a. create map of study sites to go with data   #################
##########################################################################################
##########################################################################################
#read in permafrost shapefile
shapeC <- readOGR("e:\\GIS\\NSIDC\\cont_dissolve.shp")
shapeD <- readOGR("e:\\GIS\\NSIDC\\discont_dissolve.shp")
shapeS <- readOGR("e:\\GIS\\NSIDC\\spor_dissolve.shp")
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

shapeCp <- spTransform(shapeC, laea)
shapeDp <- spTransform(shapeD, laea)
shapeSp <- spTransform(shapeS, laea)
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
wd <- 45
hd <- 45

plotorder <- c(1,2,4,3,6,5,7,8,9)
Hseqh <- c(3,5,9)
Hseql <- c(0,3,5)
#,width=1500,height=1500,
png(paste0(plotDI,"\\vege_site_agg.png"), width = 20, height = 20, units = "in", res=300)
	a <- layout(matrix(c(1),ncol=1, byrow=TRUE), height=c(lcm(hd)), 
					width=c(lcm(wd)))
	layout.show(a)
	#set up empty plot
	
	plot(world2,type="n",axes=FALSE,xlab=" ", ylab=" ",xlim=c(-3500000,3500000),ylim=c(-3500000,3500000))
	#color background
	polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(202/255,242/255,255/255,.3))
	#boundaries
	points(world, type="l", lwd=2, col="grey65")
	#continent color
	polygon(c(world[,1],rev(world[,1])), c(world[,2],rev(world[,2])),col=rgb(253/255,245/255,208/255),border=NA)
	#midnight blue
	plot(shapeCp, col=rgb(0/255,51/255,102/255),add=TRUE,border=NA)
	#royalblue3 rgb(58/255,95/255,205/255)
	plot(shapeDp, col=rgb(57/255,105/255,153/255),add=TRUE,border=NA)
	#darkseagreen2
	plot(shapeSp, col=rgb(236/255,245/255,255/255),add=TRUE,border=NA)
	draw.pie(xyz.allV$x,xyz.allV$y,xyz.allV$z,radius=220000,col=as.character(vegeclassColors$coli),border=NA)
	points(mat.bcV$x,mat.bcV$y,pch=19,col="white",cex=8)
	text(mat.bcV$x,mat.bcV$y,paste(mat.bcV$NpolySite),cex=2.5)
	legend(-3700000,-3800000,c("continuous","discontinuous","sporadic"), 
			fill=c( rgb(0/255,51/255,102/255),rgb(57/255,105/255,153/255),rgb(236/255,245/255,255/255)),
			horiz=TRUE,bty="n",cex=3,border=NA,xpd=TRUE)
	
dev.off()	

##########################################################################################
##########################################################################################
################# Figure 1b. all data & summary                           #################
##########################################################################################
##########################################################################################


#######################################
#####organize temp data for plot  ##### 						
#######################################	



				
#add vegetation class	
datST <- join(datS,datV,by="siteid",type="left")					

#organize into a matrix with NA for each day
#get each unique depth, year, site
datI <- unique(data.frame(wyear=datST$wyear,depth=datST$depth,siteid=datST$siteid))
datI$sID <- seq(1,dim(datI)[1])

#join vegeclass

datI <- join(datI,datV,by="siteid",type="left")

#join index back into datST

datST <- join(datST,datI, by=c("wyear","depth","siteid"),type="left")	

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
datSTn$depthID <- ifelse(datSTn$depth <= 5,1,
					ifelse(datSTn$depth >5 & datSTn$depth <= 10, 2,
					ifelse(datSTn$depth > 10 & datSTn$depth <= 15, 3, 4)))
#get the histogram of temperatures across the vegetation classes and depth

					
					#create depth and vegetation id
vegeDepth <- unique(data.frame(depthID=datSTn$depthID,vegeclass=datSTn$vegeclass))
vegeDepth$vdID <- seq(1,dim(vegeDepth)[1])

#join back into soil
datSTn <- join(datSTn, vegeDepth, by=c("depthID","vegeclass"), type="left")



#######################################
#####figure with summary          ##### 
#######################################
#get summary

#get daily mean temp for each depth and vege class
medTDF <- aggregate(as.numeric(datSTn$T),by=list(datSTn$wdoy,datSTn$depthID,datSTn$vegeclass),FUN="mean")
colnames(medTDF) <- c("wdoy","depthID","vegeclass","med")
pclTDF <- aggregate(datSTn$T,by=list(datSTn$wdoy,datSTn$depthID,datSTn$vegeclass),FUN="quantile",prob=.25)
colnames(pclTDF) <- c("wdoy","depthID","vegeclass","pcl")
pchTDF <- aggregate(datSTn$T,by=list(datSTn$wdoy,datSTn$depthID,datSTn$vegeclass),FUN="quantile",prob=.75)
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

#xseq <- c(1,62,124,183,244,305)
#xlseq <- c("Oct","Dec","Feb","Apr","Jun","Aug")
xseq <- c(1,124,244,335)
xlseq <- c("Oct","Feb","Jun","Sept")
#tick width
lwt <- 6
#line axis 
alh <- 3
xalh <- 6
#cex axis
mx <- 6
#box line width
blw <- 6
#x label line
llhx <- 15
#if outer
llhxo <- -29
#y label line axis for soil temp
#if outer
yllh <- -10
yllh2 <- -80
#if inner
yllhi <- 16
#label size
lx <- 6
#label vegetation size
vlx <- 5
#mean line width
mlwd <- 5
#density axis line
dcx <- 3
#box error bar width
alw <- 5
#box mean width
mlw <- 7
#sequence for min and max plot
xseqP <- c(1,2)
#title line for vege type
tlll <- -5.25
#vegetation stature text size
stcx <- 8
stll <- -10
lgry <- rgb(165/255,165/255,165/255,.2)
#depth legend cex
lcex <- 5
plotOrder <- c(1,2,4,3,6,5,7,8,9)
xflag <- c(0,0,0,0,0,0,1,1,1)
yflag <- c(1,0,0,1,0,0,1,0,0)


xpolyL <- c(7,7,363,363)
ypolyL <- c(28,37,37,28)
			
#for(i in 1:36){
#plot(c(0,1),c(0,1))
#box(which="plot")
#}			
#dev.off()

			
wd1 <- 35
wd2 <- 15
hd1 <- 20
hd2 <- 7
png(paste0(plotDI,"\\all_panel_datab.png"),width=80,height=40, units="in",res=300)
	layout(matrix(seq(1,36), ncol=6, byrow=FALSE), widths=rep(lcm(wd1),6),
			heights=c(lcm(hd2),rep(lcm(hd1),4),lcm(hd2)))
			
par(mai=c(1,0,0,0))
plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
polygon(c(0,0,1,1),c(0,1,1,0), col=paste(heightCols$colsH[1]), border=NA)
mtext( "Short", cex=stcx,side=3,line=stll, col="white")			
#plot short tundra			
for(j in 1:3){
	i <- plotOrder[j]
	##temperature##		
		par(mai=c(1.5,1.5,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,37),xlim=c(0,370),col="white",
				xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		for(k in 1:length(listV[[i]])){		
		
				points(Soil[[listV[[i]][k]]]$wdoy,
					Soil[[listV[[i]][k]]]$T,type="l",
					col=lgry,lwd=3)
			}
		polygon(xpolyL ,ypolyL,col=as.character(vegeclassColors$coli[i]),border=NA)
		
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
		
	mtext(paste(name2[i]),col="white", side=3, line=tlll, cex=vlx) 
	
		if(j==2){
		mtext("Soil temperature (C)", side=2,line=yllh, cex=lx,outer=TRUE, at = 0.75) 
		}
	
	if(j == 3){
	legend("bottomright", c("0-5 cm", "5-10 cm", "10-15 cm", "15-20 cm"), col=test2, lwd=6,	bty="n", cex=lcex)
	mtext("Day of water year",  side=1,line=llhx, cex=lx)
	}
}	
#blank plots
for(n in 1:12){
	plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)

}

#plot first mixed height			
for(j in 4){
	i <- plotOrder[j]
	##temperature##		
		par(mai=c(1.5,1.5,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,37),xlim=c(0,370),col="white",
				xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		for(k in 1:length(listV[[i]])){		
		
				points(Soil[[listV[[i]][k]]]$wdoy,
					Soil[[listV[[i]][k]]]$T,type="l",
					col=lgry,lwd=3)
			}
	
		polygon(xpolyL ,ypolyL,col=as.character(vegeclassColors$coli[i]),border=NA)
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
		
	mtext(paste(name2[i]),col="white", side=3, line=tlll, cex=vlx) 
			
		mtext("Soil temperature (C)", side=2,line=yllhi, cex=lx) 
		mtext("Day of water year",  side=1,line=llhxo, cex=lx, outer=TRUE)
}	
par(mai=c(0,0,1,0), xpd=NA)
plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
polygon(c(0,0,1,1),c(0,1,1,0), col=paste(heightCols$colsH[2]), border=NA)	

par(xpd=TRUE)
#blank plots
for(n in 1:4){
	plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)

}
	
#plot first mixed height			
for(j in 5){
	i <- plotOrder[j]
	##temperature##		
		par(mai=c(1.5,1.5,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,37),xlim=c(0,370),col="white",
				xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		for(k in 1:length(listV[[i]])){		
		
				points(Soil[[listV[[i]][k]]]$wdoy,
					Soil[[listV[[i]][k]]]$T,type="l",
					col=lgry,lwd=3)
			}
	
		polygon(xpolyL ,ypolyL,col=as.character(vegeclassColors$coli[i]),border=NA)
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
		
	mtext(paste(name2[i]),col="white", side=3, line=tlll, cex=vlx) 
	
	legend("bottomright", c("0-5 cm", "5-10 cm", "10-15 cm", "15-20 cm"), col=test2, lwd=6,	bty="n", cex=lcex)
	
	
}	
par(mai=c(0,0,1,0))
plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
polygon(c(0,0,1,1),c(0,1,1,0), col=paste(heightCols$colsH[2]), border=NA)
mtext( "Mixed",at=0,line=stll,side=3, cex=stcx, col="white", xpd=NA)
#blank plots
for(n in 1:6){
	plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)

}
par(mai=c(1,0,0,0))
plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
polygon(c(0,0,1,1),c(0,1,1,0), col=paste(heightCols$colsH[3]), border=NA)
mtext( "Tall", cex=stcx,side=3,line=stll, col="white")
#plot tall			
for(j in 6:9){
	i <- plotOrder[j]
	##temperature##		
		par(mai=c(1.5,1.5,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,37),xlim=c(0,370),col="white",
				xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		for(k in 1:length(listV[[i]])){		
		
				points(Soil[[listV[[i]][k]]]$wdoy,
					Soil[[listV[[i]][k]]]$T,type="l",
					col=lgry,lwd=3)
			}
	
		polygon(xpolyL ,ypolyL,col=as.character(vegeclassColors$coli[i]),border=NA)
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
		
	mtext(paste(name2[i]),col="white", side=3, line=tlll, cex=vlx) 
	if(j == 9){
	legend("bottomright", c("0-5 cm", "5-10 cm", "10-15 cm", "15-20 cm"), col=test2, lwd=6,	bty="n", cex=lcex)
	mtext("Day of water year",  side=1,line=llhx, cex=lx)
	
	}
		if(j==6){
		mtext("Soil temperature (C)", side=2,line=-487, cex=lx,outer=TRUE, at = 0.5) 
		}
}
plot(c(0,1),c(0,1), type="n",	xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
dev.off()
			

		

##########################################################################################
##########################################################################################
################# Figure 3. nfactor & thaw & min                         #################
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
#####work with min and max        ##### 
#######################################
#organize soil output


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)

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
	AirMs[i] <- round(mean(AirL[[i]]$AMean),0)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}


#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)



#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#get unique veg regression id

regvegeID <- unique(data.frame(vegeclass=ParmAll$vegeclass,regID=ParmAll$regID))
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#now join back into parmall
ParmAll <- join(ParmAll,regvegeID, by=c("vegeclass","regID"),type="left")




#get range of precip and air temps in each vegetation group for each regression
#get minimumAir
minAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="min")
maxAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="max")
meanAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="mean")




regvegeID$minAir <- minAir$x
regvegeID$maxAir <- maxAir$x
regvegeID$meanAir <- round(meanAir$x,0)




#add to intercept
compAll <- vegeR[vegeR$parms=="meanComp",]
compAll <- cbind(compAll,regvegeID)


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

					
					
wd <-140
hd <- 60

#make a panel of parameters for each regression


xseq <-c(1,4,7,10,13,16,19,22,25)
Hseql <- c(0,9,15)
Hseqh <- c(9,15,26)
yli <- c(0,0,75,-35)
yhi <- c(1.5,1.7,200,5)
yii <- c(.5,.5,25,5)
xl <- -1
xh <- 27
#mean line width
mlw <- 6
#arrow line width
alw <- 4
#axis lwed
alwd <- 3
#lwd of ticks
tlw <- 13
#size of x labels
axc <- 12
axc2 <- 18
#line for label num
xll <- 3

#line for units
yll1 <- 20
#line for name
yll2 <- 65
yll3 <- 45
#cex of axis label
mcx <- 15
#text coordinates
xc <- 0
yc <- .03
tcc <- 25
#one line width
zlw <- 9
plotOrder <- c(1,2,4,3,6,5,7,8,9)

png(paste0(plotDI,"\\intercepts_N.png"), width=6000,height=8000,
			units="px")
	layout(matrix(seq(1,5),ncol=1,byrow=TRUE), width=c(lcm(wd)),
				height=c(lcm(hd),lcm(hd),lcm(hd),lcm(75),lcm(10)))
	#comparision of minimum air temperatures

	par(mai=c(0,0,2.5,0),xpd=TRUE)
		plot(c(0,1),c(0,1), ylim=c(yli[4],yhi[4]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		points(c(xl,xh),c(-22,-22),type="l",lwd=zlw, col="grey75",lty=3)
			for(i in 1:9){
				j <- plotOrder[i]
				polygon(c(xseq[i]-1,xseq[i]-1,xseq[i]+1,xseq[i]+1),
						c(compAll$X25.[compAll$vegeclass==j&compAll$regID==1],compAll$X75.[compAll$vegeclass==j&compAll$regID==1],
							compAll$X75.[compAll$vegeclass==j&compAll$regID==1],compAll$X25.[compAll$vegeclass==j&compAll$regID==1]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[i]-1,compAll$Mean[compAll$vegeclass==j&compAll$regID==1],
						xseq[i]+1,compAll$Mean[compAll$vegeclass==j&compAll$regID==1],code=0,lwd=mlw)
				arrows(	xseq[i],compAll$X0.2.[compAll$vegeclass==j&compAll$regID==1],
						xseq[i],compAll$X99.8.[compAll$vegeclass==j&compAll$regID==1],
						code=0, lwd=alw)
				}		
		
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw,lwd=alwd)
			axis(2, seq(yli[4],yhi[4], by=yii[4]), rep(" ",length(seq(yli[4],yhi[4], by=yii[4]))),
				 lwd.ticks=tlw,lwd=alwd)
			mtext(seq(yli[4],yhi[4], by=yii[4]),at=seq(yli[4],yhi[4], by=yii[4]), side=2, line=xll,cex=axc,las=2)	
		
			mtext("Minimum",side=2,line=yll2,cex=mcx)		
			mtext("temperature (C)",side=2,line=yll3,cex=mcx)
			text(xc,yhi[4]-(yhi[4]*yc), "a",cex=tcc)
		#plot intercept thaw n factor	
		par(mai=c(0,0,2.5,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[2],yhi[2]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		
			points(c(xl,xh),c(1,1),type="l",lwd=zlw, col="grey75",lty=3)
			for(i in 1:9){
				j <- plotOrder[i]
				polygon(c(xseq[i]-1,xseq[i]-1,xseq[i]+1,xseq[i]+1),
						c(beta0$X25.[beta0$regID==2&beta0$vegeclass==j],beta0$X75.[beta0$regID==2&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==2&beta0$vegeclass==j],beta0$X25.[beta0$regID==2&beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[i]-1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],
						xseq[i]+1,beta0$Mean[beta0$regID==2&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[i],beta0$X0.3.[beta0$regID==2&beta0$vegeclass==j],
						xseq[i],beta0$X99.7.[beta0$regID==2&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw,lwd=alwd)
			axis(2, seq(yli[2],yhi[2], by=yii[2]), rep(" ",length(seq(yli[2],yhi[2], by=yii[2]))),
				 lwd.ticks=tlw,lwd=alwd)
			mtext(seq(yli[2],yhi[2], by=yii[2]),at=seq(yli[2],yhi[2], by=yii[2]), side=2, line=xll,cex=axc,las=2)	
			#mtext(expression(paste("(N"[thaw],")")),side=2,line=yll1,cex=mcx)		

			mtext("Thaw n-factor",side=2,line=yll2,cex=mcx)
			mtext(" (-) ",side=2,line=yll3,cex=mcx)
			text(xc,yhi[2]-(yhi[2]*yc), "b",cex=tcc)
			
	
	
	
	#maximum temp
	par(mai=c(0,0,2.5,0),xpd=TRUE)
	
			plot(c(0,1),c(0,1), ylim=c(0,20), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		points(c(xl,xh),c(12,12),type="l",lwd=zlw, col="grey75",lty=3)
			for(i in 1:9){
				j <- plotOrder[i]
				polygon(c(xseq[i]-1,xseq[i]-1,xseq[i]+1,xseq[i]+1),
						c(compAll$X25.[compAll$vegeclass==j&compAll$regID==2],compAll$X75.[compAll$vegeclass==j&compAll$regID==2],
							compAll$X75.[compAll$vegeclass==j&compAll$regID==2],compAll$X25.[compAll$vegeclass==j&compAll$regID==2]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[i]-1,compAll$Mean[compAll$vegeclass==j&compAll$regID==2],
						xseq[i]+1,compAll$Mean[compAll$vegeclass==j&compAll$regID==2],code=0,lwd=mlw)
				arrows(	xseq[i],compAll$X0.2.[compAll$vegeclass==j&compAll$regID==2],
						xseq[i],compAll$X99.8.[compAll$vegeclass==j&compAll$regID==2],
						code=0, lwd=alw)
				}		
	
	axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw,lwd=alwd)
			axis(2, seq(0,20, by=5), rep(" ",length(seq(0,20, by=5))),
				 lwd.ticks=tlw,lwd=alwd)
			mtext(seq(0,20, by=5),at=seq(0,20, by=5), side=2, line=xll,cex=axc,las=2)	
			mtext("Maximum",side=2,line=yll2,cex=mcx)
			mtext("temperature (C)",side=2,line=yll3,cex=mcx)	
			
	text(xc,20-(20*yc), "c",cex=tcc)
	
	#x labels
	par(mai=c(0,0,0,0),xpd=TRUE)
		plot(c(0,1),c(0,1), ylim=c(-10,0), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			text(xseq,rep(-.1,length(xseq)),datVI$name2[plotOrder],
			srt=90, adj=1,cex=axc2,xpd=TRUE)	



	
	par(mai=c(0,0,0,0),xpd=TRUE)
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(0,1), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
	for(i in 1:3){
		polygon(c(Hseql[i],Hseql[i],Hseqh[i],Hseqh[i]),c(0,1,1,0),col=as.character(heightCols$colsH[i]),border=NA)
		
	}
	
	text(4.5,0.5,"short",cex=axc2,col="white")
	text(12,0.5,"mixed",cex=axc2,col="white")
	text(20.5,0.5,"tall",cex=axc2,col="white")		
	

				
dev.off()




#######################################
#####make a plot of the minimum   #####
#####to be used in supplement     #####
#######################################
	
png(paste0(plotDI,"\\intercepts_max_supp.png"), width=4000,height=5000,
			units="px")
	layout(matrix(c(1,2,3),ncol=1,byrow=TRUE), width=lcm(wd),height=c(lcm(hd),lcm(70),lcm(10)))
	par(mai=c(.5,13.5,0,0))
		
	plot(c(0,1),c(0,1), ylim=c(yli[3],yhi[3]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
	
		
			for(i in 1:9){
				j <- plotOrder[i]
				polygon(c(xseq[i]-1,xseq[i]-1,xseq[i]+1,xseq[i]+1),
						c(Tbeta0$X25.[Tbeta0$vegeclass==j],Tbeta0$X75.[Tbeta0$vegeclass==j],
							Tbeta0$X75.[Tbeta0$vegeclass==j],Tbeta0$X25.[Tbeta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[i]-1,Tbeta0$Mean[Tbeta0$vegeclass==j],
						xseq[i]+1,Tbeta0$Mean[Tbeta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[i],Tbeta0$X0.3.[Tbeta0$vegeclass==j],
						xseq[i],Tbeta0$X99.7.[Tbeta0$vegeclass==j],
						code=0, lwd=alw)
				}		
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw,lwd=alwd)
			axis(2, seq(yli[3],yhi[3], by=yii[3]), rep(" ",length(seq(yli[3],yhi[3], by=yii[3]))),
				 lwd.ticks=tlw,lwd=alwd)
			mtext(seq(yli[3],yhi[3], by=yii[3]),at=seq(yli[3],yhi[3], by=yii[3]), side=2, line=xll,cex=axc,las=2)	
				
			mtext("Days above freezing",side=2,line=yll2,cex=mcx)
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw)
	

	par(mai=c(0,13.5,0,0),xpd=TRUE)
		plot(c(0,1),c(0,1), ylim=c(-10,0), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			text(xseq,rep(-.25,length(xseq)),datVI$name2[plotOrder],srt=90, adj=1,cex=15,xpd=TRUE)

	par(mai=c(0,13.5,0,0),xpd=TRUE)	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(0,1), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
	for(i in 1:3){
		polygon(c(Hseql[i],Hseql[i],Hseqh[i],Hseqh[i]),c(0,1,1,0),col=as.character(heightCols$colsH[i]),border=NA)
		
	}
	text(4.5,0.5,"short",cex=axc2,col="white")
	text(12,0.5,"mixed",cex=axc2,col="white")
	text(20.5,0.5,"tall",cex=axc2,col="white")					
	
dev.off()



#######################################
#####make a plot of the freeze   #####
#####n-factor for supplement     #####
#######################################

#plot intercept freeze n factor
png(paste0(plotDI,"\\intercepts_freezeN_supp.png"), width=4000,height=5000,
			units="px")
	layout(matrix(c(1,2,3),ncol=1,byrow=TRUE), width=lcm(wd),height=c(lcm(hd),lcm(70),lcm(10)))
	par(mai=c(.5,13.5,0,0))
		
			plot(c(0,1),c(0,1), ylim=c(yli[1],yhi[1]), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			points(c(xl,xh),c(1,1),type="l",lwd=zlw, col="grey75",lty=3)
			for(i in 1:9){
				j <- plotOrder[i]
				polygon(c(xseq[i]-1,xseq[i]-1,xseq[i]+1,xseq[i]+1),
						c(beta0$X25.[beta0$regID==1&beta0$vegeclass==j],beta0$X75.[beta0$regID==1&beta0$vegeclass==j],
							beta0$X75.[beta0$regID==1&beta0$vegeclass==j],beta0$X25.[beta0$regID==1&beta0$vegeclass==j]),
						col=paste(vegeclassColors$coli[j]),border=NA)
				arrows(xseq[i]-1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],
						xseq[i]+1,beta0$Mean[beta0$regID==1&beta0$vegeclass==j],code=0,lwd=mlw)
				arrows(	xseq[i],beta0$X0.3.[beta0$regID==1&beta0$vegeclass==j],
						xseq[i],beta0$X99.7.[beta0$regID==1&beta0$vegeclass==j],
						code=0, lwd=alw)
				}
				
			axis(1, xseq, rep(" ",length(xseq)), lwd.ticks=tlw,lwd=alwd)
			axis(2, seq(yli[1],yhi[1], by=yii[1]), rep(" ",length(seq(yli[1],yhi[1], by=yii[1]))),
				 lwd.ticks=tlw,lwd=alwd)
			mtext(seq(yli[1],yhi[1], by=yii[1]),at=seq(yli[1],yhi[1], by=yii[1]), side=2, line=xll,cex=axc,las=2)	
			#mtext(expression(paste("(N"[freeze],")")),side=2,line=yll1,cex=mcx)
			mtext("Freeze n-factor",side=2,line=yll2,cex=mcx)
	par(mai=c(0,13.5,0,0),xpd=TRUE)
		plot(c(0,1),c(0,1), ylim=c(-10,0), xlim=c(xl,xh),
				xlab=" ", ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
			text(xseq,rep(-.25,length(xseq)),datVI$name2[plotOrder],srt=90, adj=1,cex=15,xpd=TRUE)

	par(mai=c(0,13.5,0,0),xpd=TRUE)	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(0,1), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
	for(i in 1:3){
		polygon(c(Hseql[i],Hseql[i],Hseqh[i],Hseqh[i]),c(0,1,1,0),col=as.character(heightCols$colsH[i]),border=NA)
		
	}
	text(4.5,0.5,"short",cex=axc2,col="white")
	text(12,0.5,"mixed",cex=axc2,col="white")
	text(20.5,0.5,"tall",cex=axc2,col="white")		

dev.off()	
#######################################
#####write tables to reference    #####
#####results in text              #####
#######################################
#first join vegetation id into both
compAll <- join(compAll, datVI, by="vegeclass", type="left")
compAll <- data.frame(Mean=compAll$Mean, pc.l =compAll$X0.2., pc.h = compAll$X99.8,
				vegeclass=compAll$vegeclass, name=compAll$name,regID=compAll$regID)		
write.table(compAll, paste0(plotDI,"\\mean_comp.csv")	, sep=",", row.names=FALSE)

#output n factors
beta0 <- join(beta0, datVI, by="vegeclass", type="left")
beta0 <- data.frame(Mean=beta0$Mean, pc.l =beta0$X0.3., pc.h = beta0$X99.7,
				vegeclass=beta0$vegeclass, name=beta0$name,regID=beta0$regID)	
write.table(beta0, paste0(plotDI,"\\inter_n_factor.csv")	, sep=",", row.names=FALSE)

#thawing days
Tbeta0 <- data.frame(Mean=Tbeta0$Mean, pc.l =Tbeta0$X0.3., pc.h = Tbeta0$X99.7,
				vegeclass=Tbeta0$vegeclass, name=Tbeta0$name)
write.table(Tbeta0, paste0(plotDI,"\\inter_thawing_days.csv")	, sep=",", row.names=FALSE)
##########################################################################################
##########################################################################################
################# Figure 4. average patterns                             #################
##########################################################################################
##########################################################################################


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


SoilL <- list()
SoilMs <- numeric(0)
for(i in 1:length(parmV)){
	SoilL[[i]] <- SoilParm[SoilParm$parm==parmV[i],]
	SoilMs[i] <- round(mean(SoilL[[i]]$Mean),3)
}
#pull out only revelevant comparisons that are meaningful

#Tmin vs Tave
#Tmax vs Tave
#set up comparisions
#x values
xcomp <- c(4,5)
ycomp <- c(6,6)
compNameX <- parmV[xcomp]
compNameY <- parmV[ycomp]
xcent <- SoilMs[xcomp]


SoilCompDF <- data.frame(xobs = c(SoilL[[xcomp[1]]]$Mean,SoilL[[xcomp[2]]]$Mean),
						yobs= c(SoilL[[ycomp[1]]]$Mean,SoilL[[ycomp[2]]]$Mean),
						xSD=c(SoilL[[xcomp[1]]]$SD,SoilL[[xcomp[2]]]$SD),
						ySD=c(SoilL[[ycomp[1]]]$SD,SoilL[[ycomp[2]]]$SD),
						vegeClass = c(SoilL[[xcomp[1]]]$vegeclass,SoilL[[xcomp[2]]]$vegeclass),			
						comp= rep(seq(1,length(xcomp)), each=dim(SoilL[[xcomp[1]]])[1]))

#data frame of vege class and comparision ids

vegeComp <- unique(data.frame(vegeClass=SoilCompDF$vegeClass,comp=SoilCompDF$comp))						
vegeComp <- vegeComp[order(vegeComp$comp,vegeComp$vegeClass),]						
vegeComp$vegeCompID <- seq(1,dim(vegeComp)[1])	

#join back into soildf
SoilCompDF2 <- join(SoilCompDF,vegeComp, by=c("vegeClass","comp"),type="left")


#make plotting data

xcompDF <- data.frame(xcomp=xcomp,ycomp=ycomp,compNameY=compNameY,compNameX=compNameX,xcent=xcent,
				xmin=c(round_any(min(SoilL[[xcomp[1]]]$pc2.5),5,floor),round_any(min(SoilL[[xcomp[2]]]$pc2.5),5,floor)),
				xmax=c(round_any(max(SoilL[[xcomp[1]]]$pc97.5),5,ceiling),round_any(max(SoilL[[xcomp[2]]]$pc97.5),5,ceiling)),
				ymin=c(round_any(min(SoilL[[ycomp[1]]]$pc2.5),5,floor),round_any(min(SoilL[[ycomp[2]]]$pc2.5),5,floor)),
				ymax=c(round_any(max(SoilL[[ycomp[1]]]$pc97.5),5,ceiling),round_any(max(SoilL[[ycomp[2]]]$pc97.5),5,ceiling)))




xplot <- matrix(rep(NA,100*dim(vegeComp)[1]),ncol=dim(vegeComp)[1])

	for(j in 1:dim(vegeComp)[1]){
		xplot[,j] <- seq(xcompDF$xmin[vegeComp$comp[j]],xcompDF$xmax[vegeComp$comp[j]],length.out=100)
	}
xplotDF <- data.frame(xplot=as.vector(xplot),vegeCompID=rep(seq(1,dim(vegeComp)[1]),each=100), comp=rep(vegeComp$comp,each=100),vegeClass=rep(vegeComp$vegeClass,each=100))




hhxplot <- matrix(rep(NA,100*dim(xcompDF)[1]), ncol=dim(xcompDF)[1])

	for(j in 1:dim(xcompDF)[1]){
		hhxplot[,j] <- seq(xcompDF$xmin[j],xcompDF$xmax[j],length.out=100)
	}


hhxplotDF <- data.frame(hhxplot=as.vector(hhxplot), comp=rep(seq(1,dim(xcompDF)[1]),each=100))


#multiple comparision quantile
mcQ <- round_any(0.05/(dim(vegeComp)[1]-1)	,0.001)

#pull out data of interest

muPlot <- patternDF[patternDF$parms=="mu.plot",]

regPlot <- cbind(xplotDF,muPlot)


#indicate if slope is significant from zero
patternSlope <- patternDF[patternDF$parms=="beta1",]
patternSlope <- cbind(patternSlope,vegeComp)
#add significance
patternSlope$sig <- ifelse(patternSlope$X0.3.<0&patternSlope$X99.7.<0,1,
						ifelse(patternSlope$X0.3.>0&patternSlope$X99.7.>0,1,0))
#pattern intercept
patternInt  <- patternDF[patternDF$parms=="beta0",]
patternInt <- cbind(patternInt,vegeComp)
#######################################
#####make plot                    ##### 
#####points have no colors        #####
#######################################

datVI$name2 <- c("herb barren", "graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra",
					"wetland","evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")
					
					
wd <- 50
hd <- 50
# axis limits
aveL <- -15
aveH <- 5
minL <- -30
minH <- 1
maxL <- 0
maxH <- 20
seqMax <- seq(0,20,by=5)
seqAve <- seq(-15,5,by=5)
seqMin <- seq(-30,0,by=5)
#point size
px <- 7
#point color
pcol <- rgb(216/255,216/255,216/255,.5)
#line width of regression mean
mlw <- 14
#tick width
lwt <- 8
#axis line width
lwa <- 4
#line location of axis labels
yll <- 5
#size of axis labels
amx <- 6
#size of plot labels 
llmx <- 9
#line of x axis plot labels
xll <- 15
#legend lwd
lglw <- 10
#legend size
lgcx <- 6
#legend point size
lgpt <- 7
#break into two panels
vg1 <- c(1,2,3,4)
vg2 <- c(5,6,7,8,9)

png(paste0(plotDI,"\\patterns_ave.png"), width=3600,height=3600,
			units="px")
	layout(matrix(seq(1,4),ncol=2,byrow=FALSE), width=rep(lcm(wd),4),height=rep(lcm(hd),4))
	par(mai=c(1,0,0,1))
	plot(c(0,1),c(0,1),type="n", ylim=c(aveL,aveH), xlim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	
		#vegetation mean vs min group 1
	for(i in 1:4){
		j <- vg1[i]
		points(SoilL[[xcomp[1]]]$Mean[SoilL[[xcomp[1]]]$vegeclass==j],
				SoilL[[ycomp[1]]]$Mean[SoilL[[xcomp[1]]]$vegeclass==j], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
	#credible interval regression
	for(i in 1:4){
		j <- vg1[i]
		if(patternSlope$sig[patternSlope$comp==1&patternSlope$vegeClass==j]==1){
			polygon(c(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j])),
					c(regPlot$X0.3.[regPlot$comp==1&regPlot$vegeClass==j],	
					rev(regPlot$X99.7.[regPlot$comp==1&regPlot$vegeClass==j])),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}else{
		polygon(c(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j])),
				c(rep(patternInt$X0.3.[patternInt$comp==1&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j])),
				rep(patternInt$X99.7.[patternInt$comp==1&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j]))),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}	
	#regression line	
	for(i in 1:4){
		j <- vg1[i]
		if(patternSlope$sig[patternSlope$comp==1&patternSlope$vegeClass==j]==1){
			points(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j],
					regPlot$Mean[regPlot$comp==1&regPlot$vegeClass==j], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))
		}else{
			abline(h=patternInt$Mean[patternInt$comp==1&patternInt$vegeClass==j],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=3)
		}
	
	}
	axis(2, seqAve, rep(" ",length(seqAve)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqAve,at=seqAve, line=yll, cex=amx,las=2, side=2)
	
	

	
	legend("bottomright",datVI$name2[vg1], col=as.character(paste(vegeclassColors$coli[vg1])),
			 lwd=lglw, cex=lgcx,bty="n")
	
	
	mtext("Average soil temperature (C)", outer=TRUE, line=-12, cex=llmx, side=2)
	par(mai=c(0,0,1,1))
	plot(c(0,1),c(0,1),type="n", ylim=c(aveL,aveH), xlim=c(minL,minH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	
		#vegetation mean vs min group 2
	for(i in 1:5){
		j <- vg2[i]
		points(SoilL[[xcomp[1]]]$Mean[SoilL[[xcomp[1]]]$vegeclass==j],
				SoilL[[ycomp[1]]]$Mean[SoilL[[xcomp[1]]]$vegeclass==j], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
	
		#credible interval regression
	for(i in 1:5){
		j <- vg2[i]
		if(patternSlope$sig[patternSlope$comp==1&patternSlope$vegeClass==j]==1){
			polygon(c(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j])),
					c(regPlot$X0.3.[regPlot$comp==1&regPlot$vegeClass==j],	
					rev(regPlot$X99.7.[regPlot$comp==1&regPlot$vegeClass==j])),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}else{
		polygon(c(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j])),
				c(rep(patternInt$X0.3.[patternInt$comp==1&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j])),
				rep(patternInt$X99.7.[patternInt$comp==1&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j]))),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}
	
	for(i in 1:5){
		j <- vg2[i]
		if(patternSlope$sig[patternSlope$comp==1&patternSlope$vegeClass==j]==1){
			points(regPlot$xplot[regPlot$comp==1&regPlot$vegeClass==j],
					regPlot$Mean[regPlot$comp==1&regPlot$vegeClass==j], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))
		}else{
			abline(h=patternInt$Mean[patternInt$comp==1&patternInt$vegeClass==j],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=3)
		}
	
	}
	axis(2, seqAve, rep(" ",length(seqAve)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqAve,at=seqAve, line=yll, cex=amx,las=2, side=2)
	axis(1, seqMin, rep(" ",length(seqMin)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqMin,at=seqMin, line=yll, cex=amx, side=1)
	mtext("Minimum soil temperature (C)",  line=xll, cex=llmx, side=1)


	
		legend("bottomright",datVI$name2[vg2], col=as.character(paste(vegeclassColors$coli[vg2])),
			 lwd=lglw, cex=lgcx,bty="n")
	
	#average vs max group 1 	
	par(mai=c(1,1,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(aveL,aveH), xlim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
			
		for(i in 1:4){
		j <- vg1[i]
		points(SoilL[[xcomp[2]]]$Mean[SoilL[[xcomp[2]]]$vegeclass==j],
				SoilL[[ycomp[2]]]$Mean[SoilL[[xcomp[2]]]$vegeclass==j], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
		for(i in 1:4){
		j <- vg1[i]
		if(patternSlope$sig[patternSlope$comp==2&patternSlope$vegeClass==j]==1){
			polygon(c(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j])),
					c(regPlot$X0.3.[regPlot$comp==2&regPlot$vegeClass==j],	
					rev(regPlot$X99.7.[regPlot$comp==2&regPlot$vegeClass==j])),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}else{
		polygon(c(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j])),
				c(rep(patternInt$X0.3.[patternInt$comp==2&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j])),
				rep(patternInt$X99.7.[patternInt$comp==2&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j]))),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}
		for(i in 1:4){
		j <- vg1[i]
		if(patternSlope$sig[patternSlope$comp==2&patternSlope$vegeClass==j]==1){
			points(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j],
					regPlot$Mean[regPlot$comp==2&regPlot$vegeClass==j], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))
		}else{
			abline(h=patternInt$Mean[patternInt$comp==2&patternInt$vegeClass==j],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=3)
		}
	
	}
	
	axis(4, seqAve, rep(" ",length(seqAve)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqAve,at=seqAve, line=yll, cex=amx,las=2, side=4)
	mtext("Average soil temperature (C)", outer=TRUE, line=-12, cex=llmx, side=4)
	#average vs max group 2 	
	par(mai=c(0,1,1,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(aveL,aveH), xlim=c(maxL,maxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
			
		for(i in 1:5){
		j <- vg2[i]
		points(SoilL[[xcomp[2]]]$Mean[SoilL[[xcomp[2]]]$vegeclass==j],
				SoilL[[ycomp[2]]]$Mean[SoilL[[xcomp[2]]]$vegeclass==j], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
		for(i in 1:5){
		j <- vg2[i]
		if(patternSlope$sig[patternSlope$comp==2&patternSlope$vegeClass==j]==1){
			polygon(c(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j])),
					c(regPlot$X0.3.[regPlot$comp==2&regPlot$vegeClass==j],	
					rev(regPlot$X99.7.[regPlot$comp==2&regPlot$vegeClass==j])),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}else{
		polygon(c(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j],
						rev(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j])),
				c(rep(patternInt$X0.3.[patternInt$comp==2&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j])),
				rep(patternInt$X99.7.[patternInt$comp==2&patternInt$vegeClass==j], 
					length(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j]))),	
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}
		for(i in 1:5){
		j <- vg2[i]
		if(patternSlope$sig[patternSlope$comp==2&patternSlope$vegeClass==j]==1){
			points(regPlot$xplot[regPlot$comp==2&regPlot$vegeClass==j],
					regPlot$Mean[regPlot$comp==2&regPlot$vegeClass==j], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))
		}else{
			abline(h=patternInt$Mean[patternInt$comp==2&patternInt$vegeClass==j],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=2)
		}
	
	}
	axis(4, seqAve, rep(" ",length(seqAve)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqAve,at=seqAve, line=yll, cex=amx,las=2, side=4)
	axis(1, seqMax, rep(" ",length(seqMax)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqMax,at=seqMax, line=yll, cex=amx, side=1)
	mtext("Maximum soil temperature (C)",  line=xll, cex=llmx, side=1)	
dev.off()	


#######################################
#####output table of regression   #####
##### R2 for fit                  #####
#######################################

regRep <- cbind(patternDF[patternDF$parms=="rep.yvar",], SoilCompDF2)

#get goodness of fit for each group
vegeCompID <- unique(data.frame(vegeCompID=regRep$vegeCompID,vegeClass=regRep$vegeClass,comp=regRep$comp))
vegeCompID <- vegeCompID[order(vegeCompID$vegeCompID),]


regFit <- list()
R2 <- numeric(0)
for(i in 1:dim(vegeCompID)[1]){
	regFit <- lm(regRep$Mean[regRep$vegeCompID==i]~regRep$yobs[regRep$vegeCompID==i])
	R2[i] <- summary(regFit)$r.squared

}

vegeCompID$r.sq <- round(R2,3) 

write.table(vegeCompID,paste0(plotDI,"\\soil_ave_r2.csv"), sep=",", row.names=FALSE)



##########################################################################################
##########################################################################################
################# Figure 5. min/max regression                           #################
##########################################################################################
##########################################################################################


#######################################
#####organize data                ##### 
#######################################


#join vege class to soilParm
SoilParm <- join(SoilParm,datV, by=c("siteid"), type="left")
unique(SoilParm$vegeclass)

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
	AirMs[i] <- round(mean(AirL[[i]]$AMean),0)
	#add a regression ID
	AirL[[i]]$regID <- rep(i,dim(AirL[[i]])[1])
}


#turn back into a data frame

SoilR <- ldply(SoilL,data.frame)
AirR <- ldply(AirL,data.frame)



#now join soil and air DF
ParmAll <- join(SoilR,AirR, by=c("siteid","wyear","regID"),type="left")


#get unique veg regression id

regvegeID <- unique(data.frame(vegeclass=ParmAll$vegeclass,regID=ParmAll$regID))
regvegeID$regvegeID <- seq(1,dim(regvegeID)[1])

#now join back into parmall
ParmAll <- join(ParmAll,regvegeID, by=c("vegeclass","regID"),type="left")




#get range of precip and air temps in each vegetation group for each regression
#get minimumAir
minAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="min")
maxAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="max")
meanAir <- aggregate(ParmAll$AMean,by=list(ParmAll$regvegeID),FUN="mean")




regvegeID$minAir <- minAir$x
regvegeID$maxAir <- maxAir$x
regvegeID$meanAir <- round(meanAir$x,0)



#get min and max of data for plotting
#air
regMinA <- aggregate(ParmAll$AMean,by=list(ParmAll$regID),FUN="min")
regMinA$x <- round_any(regMinA$x,5,floor)

regMaxA <- aggregate(ParmAll$AMean,by=list(ParmAll$regID),FUN="max")
regMaxA$x <- round_any(regMaxA$x,5,ceiling)



#set up in matrix for plotting
regPlotA <- matrix(rep(NA,dim(regvegeID)[1]*200),ncol=dim(regvegeID)[1])
regTempA <- numeric(0)

for(i in 1:dim(regvegeID)[1]){
	regTempA <- seq(regMinA$x[regvegeID$regID[i]],regMaxA$x[regvegeID$regID[i]],length.out=200)
	regPlotA[,i] <- regTempA

}

#######################################
#####organize model output        ##### 
#######################################

regMu <- vegeR[vegeR$parms2=="plotRegA",]
regMu$regvegeID <- rep(seq(1,dim(regvegeID)[1]),each=200)

regMu <- join(regMu,regvegeID, by="regvegeID", type="left")

datVI$name2 <- c("herb barren", "graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra",
					"wetland","evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")
					
	

#isolate betas

beta0 <- vegeR[vegeR$parms=="beta0",]
beta1 <- vegeR[vegeR$parms=="beta1",]
beta2 <- vegeR[vegeR$parms=="beta2",]

#add in sig test
beta1$sigID <- ifelse(beta1$X0.2.<0&beta1$X99.8.<0,1,
				ifelse(beta1$X0.2.>0&beta1$X99.8.>0,1,0))
				
beta2$sigID <- ifelse(beta2$X0.2.<0&beta2$X99.8.<0,1,
				ifelse(beta2$X0.2.>0&beta2$X99.8.>0,1,0))	


beta2 <- cbind(beta2,regvegeID)
beta0 <- cbind(beta0,regvegeID)

	
wd <- 50
hd <- 50
# axis limits

SminL <- -35
SminH <- 1
SmaxL <- 0
SmaxH <- 20

AminL <- regMinA$x[1]
AminH <- regMaxA$x[1]
AmaxL <- regMinA$x[2]
AmaxH <- regMaxA$x[2]

seqSMax <- seq(0,20,by=5)
seqSMin <- seq(-35,0,by=5)
seqAMin <- seq(-40,-5,by=5)
seqAMax <- seq(5,25,by=5)
#point size
px <- 7
#point color
pcol <- rgb(216/255,216/255,216/255,.5)
#line width of regression mean
mlw <- 14
#tick width
lwt <- 8
#axis line width
lwa <- 4
#line location of axis labels
yll <- 5
#size of axis labels
amx <- 6
#size of plot labels 
llmx <- 9
#line of x axis plot labels
xll <- 15
#legend lwd
lglw <- 10
#legend size
lgcx <- 6
#legend point size
lgpt <- 7
#break into two panels
vg1 <- c(1,2,3,4)
vg2 <- c(5,6,7,8,9)

png(paste0(plotDI,"\\Air_reg.png"), width=3600,height=3600,
			units="px")
	layout(matrix(seq(1,4),ncol=2,byrow=FALSE), width=rep(lcm(wd),4),height=rep(lcm(hd),4))
	par(mai=c(1,0,0,1))
	plot(c(0,1),c(0,1),type="n", xlim=c(AminL,AminH), ylim=c(SminL,SminH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	
		#vegetation mean vs min group 1
	for(i in 1:4){
		j <- vg1[i]
		points(ParmAll$AMean[ParmAll$vegeclass==j&ParmAll$regID==1],
				ParmAll$Mean[ParmAll$vegeclass==j&ParmAll$regID==1], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
	#credible interval regression
	for(i in 1:4){
		j <- vg1[i]
	if(beta2$sigID[beta2$vegeclass==j&beta2$regID==1]==1){
		polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]])),
				c(regMu$X0.2[regMu$vegeclass==j&regMu$regID==1], 
				rev(regMu$X99.8[regMu$vegeclass==j&regMu$regID==1])), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
	}else{
		polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]])),
				c(rep(beta0$X0.2[beta0$vegeclass==j&beta0$regID==1],200), 
				rev(rep(beta0$X99.8[beta0$vegeclass==j&beta0$regID==1],200))), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}
	#regression line	
	for(i in 1:4){
		j <- vg1[i]
		if(beta2$sigID[beta2$vegeclass==j&beta2$regID==1]==1){
			points(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]],
					regMu$Mean[regMu$vegeclass==j&regMu$regID==1], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))
		}else{
		
		abline(h=beta0$Mean[beta0$vegeclass==j&beta0$regID==1],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=2)
		}
	
	}
	axis(2, seqSMin, rep(" ",length(seqSMin)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqSMin,at=seqSMin, line=yll, cex=amx,las=2, side=2)
	

	
	legend("bottomright",datVI$name2[vg1], col=as.character(paste(vegeclassColors$coli[vg1])),
			 lwd=lglw, cex=lgcx,bty="n")
	
	
	mtext("Minimum soil temperature (C)", outer=TRUE, line=-12, cex=llmx, side=2)
	
	#minimum temp group 2
	par(mai=c(0,0,1,1))
	plot(c(0,1),c(0,1),type="n", xlim=c(AminL,AminH), ylim=c(SminL,SminH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
	#data points		
	for(i in 1:5){
		j <- vg2[i]		
		points(ParmAll$AMean[ParmAll$vegeclass==j&ParmAll$regID==1],
				ParmAll$Mean[ParmAll$vegeclass==j&ParmAll$regID==1], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
	
		#credible interval regression
	for(i in 1:5){
		j <- vg2[i]
	if(beta2$sigID[beta2$vegeclass==j&beta2$regID==1]==1){
		polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]])),
				c(regMu$X0.2[regMu$vegeclass==j&regMu$regID==1], 
				rev(regMu$X99.8[regMu$vegeclass==j&regMu$regID==1])), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		
	}else{
			polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]])),
				c(rep(beta0$X0.2[beta0$vegeclass==j&beta0$regID==1],200), 
				rev(rep(beta0$X99.8[beta0$vegeclass==j&beta0$regID==1],200))), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}
	#regression line
	for(i in 1:5){
		j <- vg2[i]
	if(beta2$sigID[beta2$vegeclass==j&beta2$regID==1]==1){
		points(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==1]],
					regMu$Mean[regMu$vegeclass==j&regMu$regID==1], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))

	}else{
			abline(h=beta0$Mean[beta0$vegeclass==j&beta0$regID==1],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=2)
		}
	}
	axis(2, seqSMin, rep(" ",length(seqSMin)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqSMin,at=seqSMin, line=yll, cex=amx,las=2, side=2)
	axis(1, seqAMin, rep(" ",length(seqAMin)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqAMin,at=seqAMin, line=yll, cex=amx, side=1)
	mtext("Minimum air temperature (C)",  line=xll, cex=llmx, side=1)


	
		legend("bottomright",datVI$name2[vg2], col=as.character(paste(vegeclassColors$coli[vg2])),
			 lwd=lglw, cex=lgcx,bty="n")
	
	#max group 1 	
	par(mai=c(1,1,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(SmaxL,SmaxH), xlim=c(AmaxL,AmaxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
			
		for(i in 1:4){
		j <- vg1[i]
		points(ParmAll$AMean[ParmAll$vegeclass==j&ParmAll$regID==2],
				ParmAll$Mean[ParmAll$vegeclass==j&ParmAll$regID==2], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
		for(i in 1:4){
		j <- vg1[i]
		if(beta2$sigID[beta2$vegeclass==j&beta2$regID==2]==1){
			polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]])),
				c(regMu$X0.2[regMu$vegeclass==j&regMu$regID==2], 
				rev(regMu$X99.8[regMu$vegeclass==j&regMu$regID==2])), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}else{
		
					polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]])),
				c(rep(beta0$X0.2[beta0$vegeclass==j&beta0$regID==2],200), 
				rev(rep(beta0$X99.8[beta0$vegeclass==j&beta0$regID==2],200))), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}
	}
		for(i in 1:4){
		j <- vg1[i]
	if(beta2$sigID[beta2$vegeclass==j&beta2$regID==2]==1){
		points(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]],
					regMu$Mean[regMu$vegeclass==j&regMu$regID==2], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))
	}else{
				abline(h=beta0$Mean[beta0$vegeclass==j&beta0$regID==2],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=2)
	}
	
	}
	
	axis(4, seqSMax, rep(" ",length( seqSMax)), lwd.ticks=lwt, lwd=lwa)
	mtext( seqSMax,at= seqSMax, line=yll, cex=amx,las=2, side=4)
	mtext("Maximum soil temperature (C)", outer=TRUE, line=-12, cex=llmx, side=4)
	
	
	#average vs max group 2 	
	par(mai=c(0,1,1,0))
	plot(c(0,1),c(0,1),type="n",ylim=c(SmaxL,SmaxH), xlim=c(AmaxL,AmaxH), xlab=" ", ylab=" ",
			xaxs="i", yaxs="i", axes=FALSE)
			
		for(i in 1:5){
		j <- vg2[i]
		points(ParmAll$AMean[ParmAll$vegeclass==j&ParmAll$regID==2],
				ParmAll$Mean[ParmAll$vegeclass==j&ParmAll$regID==2], pch=19,
				col=as.character(paste(vegeclassColors$colt1[j])),cex=px)
	
	}
		for(i in 1:5){
		j <- vg2[i]
		if(beta2$sigID[beta2$vegeclass==j&beta2$regID==2]==1){
		polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]])),
				c(regMu$X0.2[regMu$vegeclass==j&regMu$regID==2], 
				rev(regMu$X99.8[regMu$vegeclass==j&regMu$regID==2])), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
		}else{
				polygon(c(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]],
						rev(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]])),
				c(rep(beta0$X0.2[beta0$vegeclass==j&beta0$regID==2],200), 
				rev(rep(beta0$X99.8[beta0$vegeclass==j&beta0$regID==2],200))), 
					border=NA,col=as.character(paste(vegeclassColors$colt2[j])))
	
		}
	}
		for(i in 1:5){
		j <- vg2[i]
	if(beta2$sigID[beta2$vegeclass==j&beta2$regID==2]==1){
		points(regPlotA[,regvegeID$regvegeID[regvegeID$vegeclass==j&regvegeID$regID==2]],
					regMu$Mean[regMu$vegeclass==j&regMu$regID==2], type="l",
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])))

	}else{
					abline(h=beta0$Mean[beta0$vegeclass==j&beta0$regID==2],
					lwd=mlw, col=as.character(paste(vegeclassColors$coli[j])),lty=2)
	
	}
	}
	axis(4, seqSMax, rep(" ",length( seqSMax)), lwd.ticks=lwt, lwd=lwa)
	mtext( seqSMax,at= seqSMax, line=yll, cex=amx,las=2, side=4)
	axis(1, seqAMax, rep(" ",length(seqAMax)), lwd.ticks=lwt, lwd=lwa)
	mtext(seqAMax,at=seqAMax, line=yll, cex=amx, side=1)
	mtext("Maximum air temperature (C)",  line=xll, cex=llmx, side=1)	
dev.off()	




##########################################################################################
##########################################################################################
################# Appendix data summary table                            #################
##########################################################################################
##########################################################################################


#######################################
#####organize data                ##### 
#######################################

colnames(tabA) [4] <- "nA"

tabSum <- join(tabA,tabS, by=c("siteid","wyear"),type="full")

tabSum <- tabSum[order(tabSum$siteid),]

#fix wyear with incorrect number
tabSum$wyear <- ifelse(tabSum$wyear<1000&tabSum$wyear<50,tabSum$wyear+2000,
						ifelse(tabSum$wyear<1000&tabSum$wyear>=50, tabSum$wyear+1990,tabSum$wyear))

#organize to match requirements

tabSub <- tabSum[tabSum$nA>=floor(365*.75) & tabSum$n>=floor(365*.75) & tabSum$height >= 1 & tabSum$depth <= 20,]

#create a summary table
#for full data and for data used
#site id, years, air heights, soil depths, 

#start with all data
#get unique siteid
siteSum <- unique(data.frame(siteid=tabSum$siteid))



#years
yearSiL <- list()
yearSi <- character()
for(i in 1:dim(siteSum)[1]){

	yearSiL[[i]] <- unique(tabSum$wyear[tabSum$siteid==siteSum$siteid[i]])
	yearSi[i] <- paste(yearSiL[[i]],collapse=", ")
}

#depths
depthSiL <- list()
depthSi <- character()
for(i in 1:dim(siteSum)[1]){

	depthSiL[[i]] <- unique(tabSum$depth[tabSum$siteid==siteSum$siteid[i]])
	depthSiL[[i]] <- depthSiL[[i]][!is.na(depthSiL[[i]])]
	depthSi[i] <- paste(depthSiL[[i]],collapse=", ")
}

#air height
heightSiL <- list()
heightSi <- character()
for(i in 1:dim(siteSum)[1]){

	heightSiL[[i]] <- unique(tabSum$height[tabSum$siteid==siteSum$siteid[i]])
	heightSiL[[i]] <- heightSiL[[i]][!is.na(heightSiL[[i]])]
	heightSi[i] <- paste(heightSiL[[i]],collapse=", ")
}


siteSum$depth <- depthSi
siteSum$wyear <- yearSi
siteSum$height <- heightSi

#add in more site info that is relevant
#first subset siteinfo relevant info
siteToJoin <- data.frame(siteid=siteinfo$site_id, latitude=siteinfo$lat,longitude=siteinfo$lon)

siteSum <- join(siteSum, siteToJoin, by="siteid",type="left")
#join in vegetation class
datVI$name2 <- c("herb barren", "graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra",
					"wetland","evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")


					
vegeToJoin1 <- data.frame(vegeclass=datVI$vegeclass,vegetation.type=datVI$name2)

vegeToJoin2 <- join(datV,vegeToJoin1, by="vegeclass")

siteSum <- join(siteSum, vegeToJoin2, by="siteid", type="left")

#reorganize table
siteSumSave <- data.frame(siteid=siteSum$siteid,latitude=siteSum$latitude,longitude=siteSum$longitude,
						vegetation.type=siteSum$vegetation.type,wyear=siteSum$wyear,depth=siteSum$depth,height=siteSum$height)	

write.table(siteSumSave, paste0(plotDI,"\\summary_all_sites.csv"),sep=",",row.names=FALSE)


#now just subset sites, years and depths used in the analysis

#get unique values from soil parm
sumS <- unique(data.frame(siteid=SoilParm$siteid,depth=SoilParm$depth,wyear=SoilParm$wyear))

sumA <- unique(data.frame(siteid=AirParm$siteid, height=AirParm$height))
sumSsites <- unique(data.frame(siteid=sumS$siteid))
#sumarize all
#years
syearSiL <- list()
syearSi <- character()

for(i in 1:dim(sumSsites)[1]){

	syearSiL[[i]] <- unique(sumS$wyear[sumS$siteid==sumSsites$siteid[i]])
	syearSi[i] <- paste(syearSiL[[i]],collapse=", ")
}

#depths
sdepthSiL <- list()
sdepthSi <- character()
for(i in 1:dim(sumSsites)[1]){

	sdepthSiL[[i]] <- unique(sumS$depth[sumS$siteid==sumSsites$siteid[i]])
	sdepthSiL[[i]] <- sdepthSiL[[i]][!is.na(sdepthSiL[[i]])]
	sdepthSi[i] <- paste(sdepthSiL[[i]],collapse=", ")
}

#air height
sheightSiL <- list()
sheightSi <- character()
for(i in 1:dim(sumSsites)[1]){

	sheightSiL[[i]] <- unique(sumA$height[sumA$siteid==sumSsites$siteid[i]])
	sheightSiL[[i]] <- sheightSiL[[i]][!is.na(sheightSiL[[i]])]
	sheightSi[i] <- paste(sheightSiL[[i]],collapse=", ")
}


sumSsites$depth <- sdepthSi
sumSsites$wyear <- syearSi
sumSsites$height <- sheightSi

#join together
sumAll <- join(sumSsites,vegeToJoin2, by=c("siteid"), type="left")

sumAll <- data.frame(sumAll[,1:4],vegetation.type=sumAll$vegetation.type)
#join other siteinfo
sumAll <- join(sumAll,siteToJoin, by="siteid",type="left")

sumAllSave <- data.frame(siteid=sumAll$siteid,latitude=sumAll$latitude,longitude=sumAll$longitude,
						vegetation.type=sumAll$vegetation.type,wyear=sumAll$wyear,
						depth=sumAll$depth,height=sumAll$height)	


write.table(sumAllSave, paste0(plotDI,"\\summary_analysis_sites.csv"),sep=",",row.names=FALSE)