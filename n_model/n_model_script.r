library(plyr)
library(lubridate)
library(rjags)
library(coda)
library(xtable)
library(mcmcplots)

setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7")
#read in n factor outputs
datF<-read.csv("FreezeN_out.csv")
datT<-read.csv("ThawN_out.csv")
#siteinfo
datI<-read.table("siteinfo.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))

#species cover
datC<-read.table("spcov.csv",sep=",", header=TRUE, na.strings=c("NaN","NA"))
#species biomass
datB<-read.table("spec_bio.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
#read in soil organic layer data
datS<-read.table("soil.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
	