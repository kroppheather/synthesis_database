

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
#####packages               ##### 
#######################################

library(maps)
library(mapproj)
library(rgdal)

laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 

wd <- 20
hd <- 20

a <- layout(matrix(c(1),ncol=1), height=rep(lcm(hd),1), width=rep(lcm(wd),1))
layout.show(a)
plot(map("world", project="azequidistant", col=c("grey50"),lwd=2, fill=FALSE, ylim=c(55,90),mar=c(0,0,0,0)),type="n",axes=FALSE)
polygon(c(-1,-1,1,1),c(-1,1,1,-1),border=NA,col=rgb(180/255,205/255,205/255,.5))
map("world", project="azequidistant", col=c("grey50"),lwd=2, fill=FALSE, ylim=c(55,90),mar=c(0,0,0,0),add=TRUE)
map("world", project="azequidistant", col=c("cornsilk2"), fill=TRUE,border=NA, ylim=c(55,90),mar=c(0,0,0,0),add=TRUE)


points(mapproject(siteinfo$lon,siteinfo$lat,"azeqarea"),pch=19)

a <- layout(matrix(c(1),ncol=1), height=rep(lcm(hd),1), width=rep(lcm(wd),1))
layout.show(a)
plot(map("world", project="azequalarea", col=c("grey50"),lwd=2, fill=FALSE, ylim=c(55,90),mar=c(0,0,0,0)),type="n",axes=FALSE)
polygon(c(-1,-1,1,1),c(-1,1,1,-1),border=NA,col=rgb(180/255,205/255,205/255,.5))
map("world", project="azequalarea", col=c("grey50"),lwd=2, fill=FALSE, ylim=c(55,90),mar=c(0,0,0,0),add=TRUE)
map("world", project="azequalarea", col=c("cornsilk2"), fill=TRUE,border=NA, ylim=c(55,90),mar=c(0,0,0,0),add=TRUE)


points(mapproject(siteinfo$lon,siteinfo$lat,"azequalarea"),pch=19)

test <- project(matrix(c(siteinfo$lon,siteinfo$lat),ncol=2,byrow=FALSE),laea2)
test3 <- mapproject(siteinfo$lon,siteinfo$lat,"azequalarea")
 test2 <- project(,laea)
test <- project(matrix(c(siteinfo$lon,siteinfo$lat),ncol=2,byrow=FALSE),laea)

test4 <- map("world", ylim=c(55,90))

test5 <- project(matrix(c(test4$x,test4$y), ncol=2,byrow=FALSE),laea)

a <- layout(matrix(c(1),ncol=1), height=rep(lcm(hd),1), width=rep(lcm(wd),1))
layout.show(a)
plot(test5, type="l")
points(test,pch=19)


