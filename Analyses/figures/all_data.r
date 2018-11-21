##########################################################
##########################################################
########### Data plotting for all temp obs  ##############
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
##########################################################
                   
#run script that processes model output and puts it into organized dataframes
source("c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\Analyses\\temp_parm_extract.r")

library(plyr)
library(lubridate)
library(imager)


# set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_6")
#read in soil temperature
datS<-read.table("soil_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in model run status
modrun <- read.csv("site_model_statusr5.csv")
#vegetation class
datV <- read.csv("vege_class.csv")
colnames(datV)[1] <- "site_id"
#plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\synthesis_model\\analyses\\figure"

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

countAll <- list()
maxC <- numeric(0)
for(i in 1:dim(vegeDepth)[1]){
	countAll[[i]] <- hist(datSTn$wdoy[datSTn$vdID==i], breaks=seq(0,367),freq=FALSE)
	maxC[i] <- max(countAll[[i]]$counts)
}


tempHist <- list()
maxT <- numeric(0)
for(i in 1:dim(vegeDepth)[1]){
	tempHist[[i]] <- hist(datSTn$soil_t[datSTn$vdID==i], breaks=seq(-41,35),freq=FALSE)
	maxT[i] <- max(tempHist[[i]]$density)
}
#######################################
#####figure of data               ##### 
#######################################
#get the maximum for each vegetation type
maxTdf <- aggregate(maxT,by=list(vegeDepth$vegeclass),FUN="max")
colnames(maxTdf) <- c("vegeclass","maxT")
#try to represent all four pieces of information nicely
test <- c(rgb(213/255,94/255,0/255,.1),rgb(240/255,228/255,66/255,.1),
			rgb(0/255,114/255,178/255,.1),rgb(0/255,158/255,115/255,.1))
test2 <- c(rgb(213/255,94/255,0/255,.8),rgb(240/255,228/255,66/255,.8),
			rgb(0/255,114/255,178/255,.8),rgb(0/255,158/255,115/255,.8))	
#names
name2 <- c("Herb barren", "Graminoid tundra","Tussock tundra","Short shrub tundra","Tall shrub tundra",
					"Wetland","Evergreen needleleaf boreal","Deciduous needleleaf boreal","Mixed boreal")			
			
wd1 <- 40
hd1 <- 40
wd2 <- 20
hd2 <- 20
Cyl <- 	round_any(max(maxC),50,ceiling)
#seq for x2
xh2 <- round_any(maxTdf$maxT,.1,ceiling)
xl2 <- 0
xi2 <- 0.1
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

#tick width
lwt <- 5
#line axis 
alh <- 6
#x line axis 
yllh <- 18
#cex axis
mx <- 5
#box line width
blw <- 2
#label line
llh <- 15
#label size
lx <- 7

	
for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual2d_class",i,".jpg"),width=2500,height=2500,quality=100)
	layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(lcm(wd1),lcm(wd2)),
			heights=c(lcm(hd1)))

	##temperature##		
		par(mai=c(0,0,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,35),xlim=c(0,370),col="white",
				xlab=" ",ylab=" ",xaxs="i",yaxs="i",axes=FALSE)
		for(j in 1:length(listV[[i]])){		
			if(datI$st_depth[listV[[i]][j]]<=5){
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
					col=test[1],lwd=3)
			}
			if(datI$st_depth[listV[[i]][j]]>5&datI$st_depth[listV[[i]][j]]<=10){	
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
				col=test[2],lwd=3)	
				}
			if(datI$st_depth[listV[[i]][j]]>10&datI$st_depth[listV[[i]][j]]<=15){	
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
				col=test[3],lwd=3)	
				}	
			if(datI$st_depth[listV[[i]][j]]>15&datI$st_depth[listV[[i]][j]]<=20){	
				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
				col=test[4],lwd=3)	
				}	
		}
		axis(1, seq(xl1,xh1,xi1),rep(" ",length(seq(xl1,xh1,xi1))),lwd.ticks=lwt)
		mtext(seq(xl1,xh1,xi1),at=seq(xl1,xh1,xi1),side=1,line=alh,cex=mx)	
		axis(2, yseq, rep(" ", length(yseq)),lwd.ticks=lwt)
		mtext(yseq,at=yseq,side=2,line=alh,cex=mx,las=2)
		box(which="plot",lwd=blw)
		mtext("Soil temperature (C)", side=2,line=yllh, cex=lx) 
		mtext("Day of water year", side=1,line=llh, cex=lx) 
		mtext(paste(name2[i]), side=3, outer=TRUE,line=-40, cex=lx) 
	##temperature histogram##
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), ylim=c(-41,35), xlim=c(xl2,xh2[i]),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==1))!=0){	
				
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids,
					
					col=test2[1], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==2))!=0){	
			
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids,
					
					col=test2[2], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==3))!=0){	
				
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids,
					
					col=test2[3], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==4))!=0){	
				
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids,
					
					col=test2[4], type="l",lwd=llw)
			}	
		axis(1, seq(xl2,xh2[i],xi2),rep(" ",length(seq(xl2,xh2[i],xi2))),lwd.ticks=lwt)
		mtext(seq(xl2+xi2,xh2[i],xi2),at=seq(xl2+xi2,xh2[i],xi2),side=1,line=alh,cex=mx)
		mtext("Density", side=1,line=llh, cex=lx) 		
	
	dev.off()
}	

#plot all images
#read in all images

jpeg(paste0(plotDI,"\\all_panel_ind.jpg"),width=7500,height=3000,quality=100)
	layout(matrix(seq(1,10),ncol=5,byrow=TRUE))
	for(i in 1:9){
		par(mai=c(0,0,0,0))
		plot(load.image(paste0(plotDI,"\\individual2d_class",i,".jpg")),axes=FALSE)
	}	
		##empty##		
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), xlim=c(0,1), ylim=c(0,1),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")	
			legend("center", c("0-5 cm", "5-10 cm", "10-15 cm", "15-20 cm"), col=test2, lwd=6,	bty="n", cex=8)
dev.off()				
	
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



#get the maximum for each vegetation type
maxTdf <- aggregate(maxT,by=list(vegeDepth$vegeclass),FUN="max")
colnames(maxTdf) <- c("vegeclass","maxT")
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
			
wd1 <- 40
hd1 <- 40
wd2 <- 20
hd2 <- 20
Cyl <- 	round_any(max(maxC),50,ceiling)
#seq for x2
xh2 <- round_any(maxTdf$maxT,.1,ceiling)
xl2 <- 0
xi2 <- 0.1
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
xseq <- c(1,62,124,183,244,305)
xlseq <- c("Oct","Dec","Feb","Apr","Jun","Aug")
#tick width
lwt <- 5
#line axis 
alh <- 6
#x line axis 
yllh <- 18
#cex axis
mx <- 5
#box line width
blw <- 2
#label line
llh <- 15
#label size
lx <- 7
#mean line width
mlwd <- 6

lgry <- rgb(165/255,165/255,165/255,.2)
	
for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual2d_summary_class",i,".jpg"),width=2500,height=2500,quality=100)
	layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(lcm(wd1),lcm(wd2)),
			heights=c(lcm(hd1)))

	##temperature##		
		par(mai=c(0,0,0,0))				
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
		axis(1, xseq,rep(" ",length(xseq)),lwd.ticks=lwt)
		mtext(xlseq,at=xseq,side=1,line=alh,cex=mx)	
		axis(2, yseq, rep(" ", length(yseq)),lwd.ticks=lwt)
		mtext(yseq,at=yseq,side=2,line=alh,cex=mx,las=2)
		box(which="plot",lwd=blw)
		mtext("Soil temperature (C)", side=2,line=yllh, cex=lx) 
		mtext("Day of water year", side=1,line=llh, cex=lx) 
		mtext(paste(name2[i]), side=3, outer=TRUE,line=-40, cex=lx) 
	##temperature histogram##
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), ylim=c(-41,35), xlim=c(xl2,xh2[i]),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==1))!=0){	
				
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids,
					
					col=test2[1], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==2))!=0){	
			
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids,
					
					col=test2[2], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==3))!=0){	
				
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids,
					
					col=test2[3], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==4))!=0){	
				
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids,
					
					col=test2[4], type="l",lwd=llw)
			}	

		axis(1, seq(xl2,xh2[i],xi2),rep(" ",length(seq(xl2,xh2[i],xi2))),lwd.ticks=lwt)
		mtext(seq(xl2+xi2,xh2[i],xi2),at=seq(xl2+xi2,xh2[i],xi2),side=1,line=alh,cex=mx)
		mtext("Density", side=1,line=llh, cex=lx) 		
	
	dev.off()
}	

#plot all images
#read in all images

jpeg(paste0(plotDI,"\\all_panel_ind_summary.jpg"),width=7500,height=3000,quality=100)
	layout(matrix(seq(1,10),ncol=5,byrow=TRUE))
	for(i in 1:9){
		par(mai=c(0,0,0,0))
		plot(load.image(paste0(plotDI,"\\individual2d_summary_class",i,".jpg")),axes=FALSE)
	}	
		##empty##		
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), xlim=c(0,1), ylim=c(0,1),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")	
			legend("center", c("0-5 cm", "5-10 cm", "10-15 cm", "15-20 cm"), col=test2, lwd=6,	bty="n", cex=8)
dev.off()				
	

