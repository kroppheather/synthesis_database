

#######################################
#####read in data                 ##### 
#######################################

                    
#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

#read in vege class data: check that patterns don't vary between vege type
datV <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vege_class.csv")
datVI <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\vegeID.csv")

#read in siteinfo 
siteinfo <- read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6\\siteinfo.csv")

#add shorter name to datVI
datVI$name <- c("herb barren","graminoid tundra","tussock tundra","short shrub tundra","tall shrub tundra","wetland",
				"evergreen needleleaf boreal","deciduous needleleaf boreal","mixed boreal")

plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\map"				
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
#####vegetation colors            ##### 						
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
#####organize data to set up      ##### 
#####aggregation of all sites     #####
#######################################

#get buffer around points
allBuffer <- buffer(siteSP,70000, dissolve=TRUE)


t.c <- list()
x <- numeric(0)
y <- numeric(0)
for(i in 1:27){
	t.c[[i]]<- allBuffer@polygons[[1]]@Polygons[[i]]@labpt
	x[i] <- t.c[[i]][1]
	y[i] <- t.c[[i]][2]
	
}
bc <- matrix(c(x,y),ncol=2,byrow=FALSE)
colnames(bc) <- c("x","y")
#need to get all points contained in each buffer

#check points in each buffer
pb <- list()
polydf <- matrix(rep(NA,dim(bc)[1]*dim(sites)[1]),ncol=dim(bc)[1])
for(i in 1:27){
	pb[[i]] <- point.in.polygon(sites[,1],sites[,2],allBuffer@polygons[[1]]@Polygons[[i]]@coords[,1],allBuffer@polygons[[1]]@Polygons[[i]]@coords[,2])
	polydf[,i] <- pb[[i]]
}
#find out which polygon each sties is in
tpoly <- numeric()
for(i in 1:dim(sites)[1]){
	tpoly[i] <- which(polydf[i,]==1)
}
#now add site info to the polygon
all.poly <- data.frame(siteid=siteinfo$site_id,polyid=tpoly)
#join vegetation info 
all.poly <- join(all.poly,datV,by="siteid",type="left")

#now summarize the total number of each vege class in each polygon
all.sumV <- aggregate(all.poly$siteid,by=list(all.poly$vegeclass,all.poly$polyid), FUN="length")
colnames(all.sumV) <- c("vegeclass","polyid","nsites")
#get the total number of sites in each polygon
all.sumS <- aggregate(all.sumV$nsites,by=list(all.sumV$polyid),FUN="sum")
colnames(all.sumS) <- c("polyid","NpolySite")

#join two back together
all.sumV <- join(all.sumV,all.sumS, by="polyid",type="left")
#calculate proportion
all.sumV$propC <- all.sumV$nsites/all.sumV$NpolySite

#join vegeclass colors in
all.sumV <- join(all.sumV,vegeclassColors, by="vegeclass",type="left")
#join polygon coordinates
mat.bc <- cbind(bc,all.sumS)
all.sumV <- join(all.sumV,mat.bc,by="polyid",type="left")
#turn into a smaller  dataframe
propAll <- data.frame(vegeclass=all.sumV$vegeclass,x=all.sumV$x,y=all.sumV$y,propC=all.sumV$propC)
xyz.all <- make.xyz(propAll$x,propAll$y,propAll$propC,propAll$vegeclass)
#######################################
#####basic map of all sites       ##### 						
#######################################						
						
wd <- 20
hd <- 20

a <- layout(matrix(c(1),ncol=1), height=rep(lcm(hd),1), width=rep(lcm(wd),1))
layout.show(a)
#set up empty plot
plot(world2,type="n",axes=FALSE,xlab=" ", ylab=" ")
#color background
polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(180/255,205/255,205/255,.5))
#boundaries
points(world, type="l", lwd=2, col="grey65")
#continent color
polygon(c(world[,1],rev(world[,1])), c(world[,2],rev(world[,2])),col="cornsilk2",border=NA)
#site points
points(sites,pch=19)

#points(bc, pch=19,col="blue",cex=2)

#######################################
#####map of all sites aggregated  ##### 						
#######################################	

wd <- 30
hd <- 30
wd2 <- 5

yseq <- seq(1,9)

jpeg(paste0(plotDI,"\\all_site_agg.jpg"),width=1800,height=1000)
	a <- layout(matrix(c(1,2),ncol=2), height=c(lcm(hd),lcm(hd)), width=c(lcm(wd),lcm(wd2)))
	layout.show(a)
	#set up empty plot
	plot(world2,type="n",axes=FALSE,xlab=" ", ylab=" ")
	#color background
	polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(180/255,205/255,205/255,.5))
	#boundaries
	points(world, type="l", lwd=2, col="grey65")
	#continent color
	polygon(c(world[,1],rev(world[,1])), c(world[,2],rev(world[,2])),col="cornsilk2",border=NA)
	draw.pie(xyz.all$x,xyz.all$y,xyz.all$z,radius=250000,col=as.character(vegeclassColors$coli),border=NA)
	points(mat.bc$x,mat.bc$y,pch=19,col="white",cex=5)
	text(mat.bc$x,mat.bc$y,paste(mat.bc$NpolySite),cex=1.5)
	#plot vegeclass legend
	plot(c(0,1),c(0,1), type="n", xlim=c(0,1), ylim=c(0,10), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
	for(i in 1:9){
		polygon(c(0,0,1,1),c(yseq[i]-1,yseq[i],yseq[i],yseq[i]-1),col=as.character(vegeclassColors$coli[i]),border=NA)
	}
	axis(4,yseq-.5,rep(" ",9),lwd.ticks=2)
	mtext(datVI$name,at=yseq-.5,cex=2,line=1,side=4,las=2)
dev.off()


#######################################
#####map of vege type sites       ##### 						
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
allBufferV <- buffer(siteSPV,70000, dissolve=TRUE)


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





#######make plot ######
yseq <- seq(1,9)
wd <- 30
hd <- 30
wd2 <- 5

png(paste0(plotDI,"\\vege_site_agg.png"),width=1800,height=1000)
	a <- layout(matrix(c(1,2),ncol=2), height=c(lcm(hd),lcm(hd)), width=c(lcm(wd),lcm(wd2)))
	layout.show(a)
	#set up empty plot
	plot(world2,type="n",axes=FALSE,xlab=" ", ylab=" ")
	#color background
	polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(180/255,205/255,205/255,.1))
	#boundaries
	points(world, type="l", lwd=2, col="grey65")
	#continent color
	polygon(c(world[,1],rev(world[,1])), c(world[,2],rev(world[,2])),col=rgb(250/255,230/255,190/255),border=NA)
	draw.pie(xyz.allV$x,xyz.allV$y,xyz.allV$z,radius=250000,col=as.character(vegeclassColors$coli),border=NA)
	points(mat.bcV$x,mat.bcV$y,pch=19,col="white",cex=5)
	text(mat.bcV$x,mat.bcV$y,paste(mat.bcV$NpolySite),cex=1.5)
	#plot legend
	plot(c(0,1),c(0,1), type="n", xlim=c(0,1), ylim=c(0,10), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
		for(i in 1:9){
		polygon(c(0,0,1,1),c(yseq[i]-1,yseq[i],yseq[i],yseq[i]-1),col=as.character(vegeclassColors$coli[i]),border=NA)
	}
	axis(4,yseq-.5,rep(" ",9),lwd.ticks=2)
	mtext(datVI$name,at=yseq-.5,cex=2,line=1,side=4,las=2)
	
dev.off()	
