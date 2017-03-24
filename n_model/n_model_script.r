library(plyr)
library(lubridate)
library(rjags)
library(coda)
library(xtable)
library(mcmcplots)

setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7")
#read in n factor outputs
datF<-read.csv("FreezeN_out.csv")
