################################################
######this file organizes vegetation data#######
######and gets it matched up with DD and #######
###### n factors                         #######
################################################

#read in vegetation data from database
#species cover
datC<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\spcov.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
#species biomass
datB<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\spec_bio.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
#read in soil organic layer data
datS<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\backup_3\\soil.csv",
				sep=",", header=TRUE, na.strings=c("NaN","NA"))
				
#Names likely have slight variations in
#capitalization, plural, and spellint
#need to go through and create consistant names

#just focus on functional type right now
#get rid of any case issues
funcLC<-tolower(datC$func_type)
#get the functional type names
Funct.all<-unique(funcLC)

#set up a vector to make names consistent 


#rename to have more consistent names
#moss here is defined as any bryophyte
funcT<- ifelse(funcLC=="Tall sedges", "sedge",
		ifelse(funcLC=="tussock sedge","tussock",
		ifelse(funcLC=="organic","bare ground",
		ifelse(funcLC=="soil","bare ground",
		ifelse(funcLC=="dryas integrifolia", "evergreen shrub",
		ifelse(funcLC=="herb","forb",
		ifelse(funcLC=="sedges","sedge",
		ifelse(funcLC=="forbs","forb",
		ifelse(funcLC=="mosses","moss",
		ifelse(funcLC=="heath","evergreen shrub",
		ifelse(funcLC=="grasses","graminoid",
		ifelse(funcLC=="gramminoid","graminoid",
		ifelse(funcLC=="bare soil","bare ground",funcLC
		)))))))))))))
		
#now check unique names again
Funct.allRN<-unique(funcT)

#now create an even simple classification that is
#just tree shrub ect
simpFT<-ifelse(funcT=="moss","moss",
		ifelse(funcT=="shrub","shrub",
		ifelse(funcT=="evergreen shrub", "shrub",
		ifelse(funcT=="deciduous shrub", "shrub",
		ifelse(funcT=="litter", "litter",
		ifelse(funcT=="bare ground", "bare ground",
		ifelse(funcT=="forb", "forb",
		ifelse(funcT=="sedge", "graminoid",
		ifelse(funcT=="tussock", "graminoid",
		ifelse(funcT=="graminoid","graminoid",
		ifelse(funcT=="deciduous tree", "tree",
		ifelse(funcT=="evergreen tree", "tree",
		ifelse(funcT=="deciduous coniferous tree", "tree","other"
				)))))))))))))

#start by aggregating across functional type
#aggregate by more detailed FT classification
perc.ftm<-aggregate(datC$perc_cover, by=list(funcT,
											datC$site_id),
					FUN="mean")
perc.sm<-aggregate(datC$perc_cover, by=list(simpFT,
											datC$site_id),
					FUN="mean")
colnames(perc.sm)<-c("FuncType", "siteid", "p.cover")	
#subset into data frames to join
Tree.c<-perc.sm[perc.sm$FuncType=="tree",]
colnames(Tree.c)[3]<-"tree.pc"
Shrub.c<-perc.sm[perc.sm$FuncType=="shrub",]
colnames(Shrub.c)[3]<-"shrub.pc"
Moss.c<-perc.sm[perc.sm$FuncType=="moss",]
colnames(Moss.c)[3]<-"moss.pc"
Ground.c<-perc.sm[perc.sm$FuncType=="bare ground",]
colnames(Ground.c)[3]<-"ground.pc"
Gram.c<-perc.sm[perc.sm$FuncType=="graminoid",]
colnames(Gram.c)[3]<-"gram.pc"
				
organic<-data.frame(siteid=datS$site_id,olt=datS$organic_thick)				
				
#########################################
####### ##################################
####now create dataframes for ###########
#### winter and summer metrics###########
#### that have vege data combined #######
#########################################

#Read in winter csv
datnW<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\WinterN.csv")
datnS<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\SummerN.csv")

#combine organic layer thickness and % cover tree, shrub, grass, moss ,bare ground
library(plyr)

datW1<-join(datnW, Tree.c, by=c("siteid"), type="left")

datW2<-join(datW1, Shrub.c, by=c("siteid"), type="left")

datW3<-join(datW2, Moss.c, by=c("siteid"), type="left")

datW4<-join(datW3, Ground.c,by=c("siteid"), type="left")

datW5<-join(datW4, Gram.c,by=c("siteid"), type="left")

datWf<-join(datW5, organic,by=c("siteid"), type="left")

#now do for summer
datS1<-join(datnS, Tree.c, by=c("siteid"), type="left")

datS2<-join(datS1, Shrub.c, by=c("siteid"), type="left")

datS3<-join(datS2, Moss.c, by=c("siteid"), type="left")

datS4<-join(datS3, Ground.c,by=c("siteid"), type="left")

datS5<-join(datS4, Gram.c,by=c("siteid"), type="left")

datSf<-join(datS5, organic,by=c("siteid"), type="left")


write.table(datSf,"c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\SummerNvege.csv",
			sep=",", row.names=FALSE)
			
write.table(datWf,"c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\WinterNvege.csv",
			sep=",", row.names=FALSE)