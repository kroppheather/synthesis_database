

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
#######################################
#####packages                     ##### 
#######################################

library(maps)
library(mapproj)
library(rgdal)
library(sp)
library(raster)
library(plyr)



laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
worldmap <- map("world", ylim=c(50,90), fill=TRUE)
#focus on a smaller extent
worldmap2 <- map("world", ylim=c(55,90))


#reproject into lambers equal aarea
sites <- project(matrix(c(siteinfo$lon,siteinfo$lat),ncol=2,byrow=FALSE),laea)
world <- project(matrix(c(worldmap$x,worldmap$y), ncol=2,byrow=FALSE),laea)
world2 <- project(matrix(c(worldmap2$x,worldmap2$y), ncol=2,byrow=FALSE),laea)
siteSP <- SpatialPoints(sites)
#get buffer around points
test <- buffer(siteSP,70000)
test2 <- as.SpatialPolygons.PolygonsList(test@polygons[[1]])
plot(test, add=TRUE)
t.c <- list()
x <- numeric(0)
y <- numeric(0)
for(i in 1:27){
	t.c[[i]]<- test@polygons[[1]]@Polygons[[i]]@labpt
	x[i] <- t.c[[i]][1]
	y[i] <- t.c[[i]][2]
	
}
bc <- matrix(c(x,y),ncol=2,byrow=FALSE)
#need to get all points contained in each buffer
pb <- over(siteSP, geometry(test))

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

points(bc, pch=19,col="blue",cex=2)


