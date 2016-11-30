################################################################################
##########this file organizes snow water equivilent data #######################
##########to  be matched up with the model               #######################
################################################################################

### set wd
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6")

#data file from GlobSNOW data extracted by
#Mike Loranty on November 29 2016
datSW<-read.csv("site_SWE_data.csv")

#pull out date information
timeIn<-gsub("\\D","",names(datSW)[19:355])
timeY<-as.numeric(substr(timeIn, 1,4))
timeM<-as.numeric(substr(timeIn,5,6))

#winter definition for N factors is 274-122 and is the year of 122
#note this May 2 -Oct 2 on non-leap years
#Thus just look at SWE for May-Oct


#there are also some missing months
#so see how many years this is possible for
#also filter out anytime before 1989
#there should be 26 years total

#filter to focus only on winter months
