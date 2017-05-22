#######################################################
########this script reads in output from model 10 #####
########and plots data and empirical mdoel output #####
########that looks at variation across vegetation #####
#######################################################
library(plyr)

setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\mod10_out")
#read in data
datN<-read.csv("nfactor.csv")
datS<-read.csv("soilParm.csv")
datA<-read.csv("airParm.csv")

#now read in some site data
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\region.csv")
datVC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u7\\vegeClass.csv")

colnames(datR)[1]<-"siteid"
colnames(datVC)[1]<-"siteid"

datI<-join(datR, datVC, by="siteid", type="left")

#match these data by siteid
datN1<-join(datN, datI, by="siteid", type="left")
datS1<-join(datS, datI, by="siteid", type="left")


#now match air temp min and max to N factor

datNF<-datN1[datN1$parm=="Fn",]
datNF<-datN1[datN1$parm=="Tn",]
datTmax<-datS1[datS1$parm=="TmaxS",]
datTmin<-datS1[datS1$parm=="TminS",]
datDZ<-datS1[datS1$parm=="DayZero",]
datPS<-datS1[datS1$parm=="peakSS",]
datPW<-datS1[datS1$parm=="peakWS",]


#now seperate out air to match
datTmaxA<-datA[datA$parm=="TmaxA",]
colnames(datTmaxA)[1:4]<-paste0(colnames(datTmaxA)[1:4],"A")
datTminA<-datA[datA$parm=="TminA",]
colnames(datTminA)[1:4]<-paste0(colnames(datTminA)[1:4],"A")
datPSA<-datA[datA$parm=="peakSA",]
colnames(datPSA)[1:4]<-paste0(colnames(datPSA)[1:4],"A")
datPWA<-datA[datA$parm=="peakWA",]
colnames(datPWA)[1:4]<-paste0(colnames(datPWA)[1:4],"A")


#now combine air measure with matching parm
datNF<-join(datNF, datTminA, by=c("siteid","height","wyear"), type="left")
datNT<-join(datNT, datTmaxA, by=c("siteid","height","wyear"), type="left")

datTmax<-join(datTmax, datTmaxA, by=c("siteid","wyear"), type="left")
datTmin<-join(datTmin, datTminA, by=c("siteid","wyear"), type="left")

datPS<-join(datPS,datPSA,by=c("siteid","wyear"), type="left")
datPW<-join(datPW,datPWA,by=c("siteid","wyear"), type="left")

datDZ<-join(datDZ,datTminA, by=c("siteid","wyear"), type="left")
