

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

#set up vegeclass colors
vegeclassColors <- data.frame(vegeclass=seq(1,9),
					coli=c("grey50","grey25", "deepskyblue2","steelblue4","seagreen4","hotpink3","gold1","midnightblue","chocolate2"))
vegeclassColors$colrgb <- c(rgb(127/255	,127/255,127/255,.25), rgb(63/255,63/255,63/255,.25),	rgb(0/255,178/255,238/255,.5),
						rgb(54/255,100/255,139/255,.25),rgb(46/255,139/255,87/255,.25),rgb(205/255,96/255,144/255,.25),
						rgb(255/255,215/255,0/255,.25),rgb(25/255,25/255,112/255,.25),rgb(238/255,118/255,33/255,.25))
vegeclassColors$colrgb2 <- c(rgb(127/255	,127/255,127/255,.5),rgb(63/255,63/255,63/255,.5), 	rgb(0/255,178/255,238/255,.5),	
						rgb(54/255,100/255,139/255,.5),rgb(46/255,139/255,87/255,.5),rgb(205/255,96/255,144/255,.5),
						rgb(255/255,215/255,0/255,.5),rgb(25/255,25/255,112/255,.5),rgb(238/255,118/255,33/255,.5))
#######################################
#####organize data to set up      ##### 
#####aggregation of all sites     #####
#######################################

#get buffer around points
allBuffer <- buffer(siteSP,70000)


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

jpeg(paste0(plotDI,"\\all_site_agg.jpg"),width=1500,height=1000)
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
	points(mat.bc$x,mat.bc$y,pch=19,col="white",cex=2.5)
	text(mat.bc$x,mat.bc$y,paste(mat.bc$NpolySite),cex=.9)
	#plot vegeclass legend
	plot(c(0,1),c(0,1), type="n", xlim=c(0,1), ylim=c(0,10), xaxs="i",yaxs="i",xlab=" ", ylab=" ",axes=FALSE)
	for(i in 1:9){
		polygon(c(0,0,1,1),c(yseq[i]-1,yseq[i],yseq[i],yseq[i]-1),col=as.character(vegeclassColors$coli[i]),border=NA)
	}
	axis(4,yseq-.5,rep(" ",9),lwd.ticks=2)
	mtext(datVI$name,at=yseq-.5,cex=1,line=1,side=4,las=2)
dev.off()
