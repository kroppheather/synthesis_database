############################################################################
##########This script calculates the daily normalized ######################
##########difference between the air and soil         ######################
############################################################################
library(plyr)
# set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3")
#read in soil temperature
datS<-read.table("soil_temp_fixed_u6_out.csv", sep=",", header=TRUE, na.string=c("NaN"))
#read in air temperature
datA<-read.table("air_temp_fixed_u6_out.csv", sep=",", header=TRUE, na.string=c("NaN"))
#change date labels for easier merging
colnames(datA)<-c("air_id", "doy_st","year_st","air_t","air_height","site_id")
#read in site info
siteinf<-read.table("siteinfo.csv", sep=",", header=TRUE, na.string=c("NaN"))


#remove any missing data values
datSf<-na.omit(datS)
datAf<-na.omit(datA)

#####This should be fixed in the future
#####Exclude site 188 in 2014 because there
#####are issues with data that need to be figured
#####out since it looks like a data flag may be 
#####present in T data
datSf2<-datSf[datSf$site_id!=188,]
datAf2<-datAf[datAf$site_id!=188,]

#match names and exclude database id
Airall<-data.frame(doy=datAf2$doy_st,year=datAf2$year_st,siteid=datAf2$site_id,
					aT=datAf2$air_t,height=datAf2$air_height)

Soilall<-data.frame(doy=datSf2$doy_st,year=datSf2$year_st,siteid=datSf2$site_id,
					sT=datSf2$soil_t,depth=datSf2$st_depth)

#start pairing up air and soil temp measurements
Tempall<-join(Airall,Soilall,by=c("siteid","doy", "year"), type="inner")

#Only focus on growing season temperatures. Look at May 1 - September 30
#create a dataflag to grab out data 
#and also account for leap year
datause<-ifelse(Tempall$doy>=121&Tempall$doy<274&floor(Tempall$year/4)-(Tempall$year/4)<0,1,
		ifelse(Tempall$doy>=122&Tempall$doy<275&floor(Tempall$year/4)-(Tempall$year/4)==0,1,0))

#subset data to growing season
TempGrow<-Tempall[datause==1,]

#calculate T diff normalized by Air temp
#This is essentially the proportion of difference from air temp
#Include T diff in percent as well as in proportion since it is
#helpful to interpret
TempGrow$Tdiff<-(TempGrow$sT-TempGrow$aT)/TempGrow$aT
TempGrow$TdiffP<-TempGrow$Tdiff*100

#get a table of how many observations in a growing season there are for site, year and soil depth
SeasObs<-aggregate(TempGrow$sT, by=list(TempGrow$siteid,TempGrow$year,TempGrow$depth), FUN="length")

#see how many sites and years are between 1-10 cm
GroupShallow<-unique(data.frame(site=SeasObs$Group.1[SeasObs$Group.3<=10&SeasObs$Group.3>0],
								year=SeasObs$Group.2[SeasObs$Group.3<=10&SeasObs$Group.3>0]))
#see how many of these sites occur in a given year
YGroupShallow<-aggregate(GroupShallow$site, by=list(GroupShallow$year), FUN="length")

# create a day sequence for ease of comparision 
#across growing season. 
#need to account for the shift in leap year
#in the days included.
groD<-data.frame(doy=seq(121,275),dayseq=c(1,1,seq(2,152),153,153))
#now add dayseq into the dataset
TempGrow<-join(TempGrow,groD,by="doy", type="inner")

#sort by shallow depths for start
ShallowD<-TempGrow[TempGrow$depth>0&TempGrow$depth<=10,]
GroundD<-TempGrow[TempGrow$depth==0,]
######################################################
########make some plots to help visualize data########
######################################################

#growing season labels
xta<-seq(1,153)
xla<-round(xta/153,2)
xlength<-seq(1, 152, by=4)
xtl<-xta[xlength]
xll<-xla[xlength]
#start by focusing on specific years that have a lot of data
#start by looking at 2014 with 50 sites
#start with shallow in 2014
#get the number of sites with ground temp data
Ssite<-unique(ShallowD$siteid[ShallowD$year==2014])
colvec<-terrain.colors(length(Ssite), alpha=1)
par(mai=c(2,2,1,1))
plot(c(0,1),c(0,1), type="n", axes=FALSE,xlim=c(0,154), ylim=c(-10,10), xlab=" ", ylab=" ", xaxs="i",
		yaxs="i")
for(i in 1:length(Ssite)){
	points(ShallowD$dayseq[ShallowD$siteid==Ssite[i]],ShallowD$Tdiff[ShallowD$siteid==Ssite[i]],, col=colvec[i], pch=19)

}
axis(1, xtl, xll, cex.axis=1.5)
axis(2, seq(-10,10, by=1), seq(-1000,1000,by=100), las=2,cex.axis=1.5)
mtext("Difference % of Air Temperature", side=2, line=7, cex=2)
mtext("Length into Growing Season", side=1, line=4, cex=2)
text(100,8, paste0("Number of sites= ",length(Ssite)), cex=2)