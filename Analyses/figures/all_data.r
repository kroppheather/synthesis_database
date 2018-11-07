##########################################################################
##########################################################################
########### Data plotting for all temp obs                  ##############
##########################################################################

library(plyr)
library(lubridate)
library(scatterplot3d)

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

for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual3d_class",i,".jpg"),width=1000,height=1000,quality=100)
		s3d <- scatterplot3d(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$st_depth,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(0,20),xlim=c(0,370),zlim=c(-35,35),angle=60,color=rgb(205/255,79/255,57/255,.4),
				xlab="day of year",ylab="depth",zlab="temperature",box=FALSE)
		for(j in 1:length(listV[[i]])){			
			s3d$points3d(Soil[[listV[[i]][j]]]$wdoy,Soil[[listV[[i]][j]]]$st_depth,Soil[[listV[[i]][j]]]$soil_t,type="l",col=rgb(205/255,79/255,57/255,.4))
		}

	dev.off()
}	

#get summary information
#get count of observations for each day of year and depth
datSTn <- na.omit(datST)
#create a depth ID
datSTn$depthID <- ifelse(datSTn$st_depth <= 5,1,
					ifelse(datSTn$st_depth >5 & datSTn$st_depth <= 10, 2,
					ifelse(datSTn$st_depth > 10 & datSTn$st_depth <= 15, 3, 4)))

#create depth and vegetation id
vegeDepth <- unique(data.frame(depthID=countAll$depthID,vegeclass=countAll$vegeclass))
vegeDepth$vdID <- seq(1,dim(vegeDepth)[1])

countAll <- list()

for(i in 1:dim(vegeDepth)[1]){
	countAll[[i]] <- hist(datSTn$wdoy[datSTn$vdID==i], breaks=seq(0,367),freq=FALSE)
}


#get the histogram of temperatures across the vegetation classes and depth


#join back into soil
datSTn <- join(datSTn, vegeDepth, by=c("depthID","vegeclass"), type="left")

tempHist <- list()

for(i in 1:dim(vegeDepth)[1]){
	tempHist[[i]] <- hist(datSTn$soil_t[datSTn$vdID==i], breaks=seq(-41,35),freq=FALSE)
}



#try to represent all four pieces of information nicely
test <- c(rgb(213/255,94/255,0/255,.1),rgb(240/255,228/255,66/255,.1),
			rgb(0/255,114/255,178/255,.1),rgb(0/255,158/255,115/255,.1))
test2 <- c(rgb(213/255,94/255,0/255,.8),rgb(240/255,228/255,66/255,.8),
			rgb(0/255,114/255,178/255,.8),rgb(0/255,158/255,115/255,.8))			
			
wd1 <- 40
hd1 <- 40
wd2 <- 20
hd2 <- 20
Cyl <- 	c(500,500,500,500,500,500,500,500,500)	
Tyl <- c(500,7000,2000,2000,2000,2000,2000,2000,2000)
#line width hist
llw <- 5
			
for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual2d_class",i,".jpg"),width=1800,height=1800,quality=100)
	layout(matrix(c(1,2,3,4), ncol=2,nrow=2, byrow=TRUE), widths=c(lcm(wd1),lcm(wd2)),
			heights=c(lcm(hd2),lcm(hd1)))
	##daily sample size##		
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), xlim=c(0,370), ylim=c(0,Cyl[i]),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==1))!=0){	

				polygon( c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids,
							rev(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids)),
							c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$counts,
								rep(0, length(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$counts))),
						col=test[1],border=NA)		
				points(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids,
					countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$counts,
					col=test2[1], type="l",lwd=llw)						
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==2))!=0){	

				polygon( c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids,
							rev(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids)),
							c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$counts,
								rep(0, length(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$counts))),
						col=test[2],border=NA)		
				points(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids,
					countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$counts,
					col=test2[2], type="l",lwd=llw)						
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==3))!=0){	
				polygon( c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids,
							rev(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids)),
							c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$counts,
								rep(0, length(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$counts))),
						col=test[3],border=NA)	
				points(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids,
					countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$counts,
					col=test2[3], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==4))!=0){	
				polygon( c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids,
							rev(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids)),
							c(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$counts,
								rep(0, length(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$counts))),
						col=test[4],border=NA)	
				points(countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids,
					countAll[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$counts,
					col=test2[4], type="l",lwd=llw)
			}	
	##empty##		
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), xlim=c(0,1), ylim=c(0,1),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")	
	##temperature##		
		par(mai=c(0,0,0,0))				
		plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-41,35),xlim=c(0,370),col="white",
				xlab="day of year",ylab="temperature",xaxs="i",yaxs="i")
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
	##temperature histogram##
		par(mai=c(0,0,0,0))
			plot(c(0,1),c(0,1), ylim=c(-41,35), xlim=c(0,1),type="n",xlab= " ", ylab=" ",axes=FALSE,
				xaxs="i",yaxs="i")
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==1))!=0){	
				polygon(c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$density,
								rep(0, length(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$density))), 
								c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids,
							rev(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids)),
							
						col=test[1],border=NA)
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==1)]]$mids,
					
					col=test2[1], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==2))!=0){	
				polygon(c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$density,
								rep(0, length(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$density))),
								c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids,
							rev(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids)),
							
						col=test[2],border=NA)
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==2)]]$mids,
					
					col=test2[2], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==3))!=0){	
				polygon( c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$density,
								rep(0, length(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$density))),
								c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids,
							rev(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids)),
							
						col=test[3],border=NA)	
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==3)]]$mids,
					
					col=test2[3], type="l",lwd=llw)
			}
			if(length(which(vegeDepth$vegeclass==i&vegeDepth$depthID==4))!=0){	
				polygon( c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$density,
								rep(0, length(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$density))),
								c(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids,
							rev(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids)),
							
						col=test[4],border=NA)	
				points(tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$density,
				tempHist[[which(vegeDepth$vegeclass==i&vegeDepth$depthID==4)]]$mids,
					
					col=test2[4], type="l",lwd=llw)
			}	
	dev.off()	
}	




for(i in 1:9){
	jpeg(paste0(plotDI,"\\individual2d_class_col_",i,".jpg"),width=1000,height=1000,quality=100)
plot(Soil[[listV[[i]][1]]]$wdoy,Soil[[listV[[i]][1]]]$soil_t,
				type="l", ylim=c(-35,35),xlim=c(0,370),col=rgb(205/255,79/255,57/255,.3),
				xlab="day of year",ylab="temperature")
		for(j in 1:length(listV[[i]])){		

				points(Soil[[listV[[i]][j]]]$wdoy,
					Soil[[listV[[i]][j]]]$soil_t,type="l",
					col=rgb(205/255,79/255,57/255,.3),lwd=3)
			}	
	dev.off()	
}				