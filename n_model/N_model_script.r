######################################
###This script runs a model ##########
###to look at drivers in    ##########
###variation of N factors   ##########
######################################
library(plyr)
library(rjags)
library(xtable)
library(ggmcmc)

##this model version starts with just
##looking at the amount of variation
##resulting from the distance between
##sensors, location, and year

#read in winter N factor data
datWN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\WinterNvege.csv")
#summer N factor data
datSN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\SummerNvege.csv")
#read in EVI data
datEVI<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\Site_EVI_out.csv")

#there is a site in the summer that clearly has some numbers
#that are not correct for soil temperature. There is likely
#a large number used for a NA. This needs to be fixed, but
#until that can happen need filter out
datSN<-datSN[datSN$T<3000,]

#calculate the distance between the two measurements

datWN$Dist<-datWN$height+(datWN$depth/100)
datSN$Dist<-datSN$height+(datSN$depth/100)

#now filter to only focus on 0-10cm
datWN<-datWN[datWN$depth<=10,]
datSN<-datSN[datSN$depth<=10,]


#now work on structuring the data to match up with the model
#start by getting the indexes together for the data
#set up a table to get unique site
Ssite<-unique(data.frame(siteid=datSN$siteid, lat=datSN$lat, lon=datSN$lon,
						loc=datSN$loc))
Ssite$siteIDm<-seq(1,length(Ssite$siteid))

#now create the siteid to match the summer n factor
datSNi<-join(datSN,Ssite, by=c("siteid","lat","lon","loc"), type="right")

#now create unique site table for winter n factor
Wsite<-unique(data.frame(siteid=datWN$siteid, lat=datWN$lat, lon=datWN$lon,
						loc=datWN$loc))

Wsite$siteIDm<-seq(1,length(Wsite$siteid))						

#now create the siteid to match the summer n factor
datWNi<-join(datWN,Wsite, by=c("siteid","lat","lon","loc"), type="right")						
						
#need an index that varies by N data for site and year
#get unique years in the dataset
Syear<-data.frame(year=unique(datSN$year))
Syear$year<-sort.int(Syear$year)
Syear$yearID<-seq(1,length(Syear$year))

#get winter year year
Wyear<-data.frame(wyear=unique(datWN$wyear))
Wyear$wyear<-sort.int(Wyear$wyear)
Wyear$yearID<-seq(1,length(Wyear$wyear))

#now add the year Id to the table too
datSNii<-join(datSNi,Syear,by=c("year"), type="right")

#add wyear to table
datWNii<-join(datWNi,Wyear,by=c("wyear"), type="right")

#now add the EVI data to the data frame
datSNiii<-join(datSNii, datEVI, by=c("siteid"), type="left")
datWNiii<-join(datWNii, datEVI, by=c("siteid"), type="left")


#siteid 22-33,43-45,42+46,54-186,57-59,63-63,72-73
#have same lat lon even though taken at different plots
#see if that is causing the covariance matrix problems
#add slight variation to see if it helps
#calculate D.S in code
D.S<-matrix(rep(NA,dim(Ssite)[1]*dim(Ssite)[1]), ncol=dim(Ssite)[1])
D.Scor<-matrix(rep(NA,dim(Ssite)[1]*dim(Ssite)[1]), ncol=dim(Ssite)[1])
for(i in 1:dim(Ssite)[1]){
	for(j in 1:dim(Ssite)[1]){
		D.S[i,j]<-sqrt(((Ssite$lat[i]-Ssite$lat[j])^2)+ 
					((Ssite$lon[i]-Ssite$lon[j])^2))
	if(i!=j&D.S[i,j]==0){
	D.Scor[i,j]<-.001
	}else{D.Scor[i,j]<-D.S[i,j]}
	
	}
}

#set up D.W 
D.W<-matrix(rep(NA,dim(Wsite)[1]*dim(Wsite)[1]), ncol=dim(Wsite)[1])
D.Wcor<-matrix(rep(NA,dim(Wsite)[1]*dim(Wsite)[1]), ncol=dim(Wsite)[1])
for(i in 1:dim(Wsite)[1]){
	for(j in 1:dim(Wsite)[1]){
		D.W[i,j]<-sqrt(((Wsite$lat[i]-Wsite$lat[j])^2)+ 
					((Wsite$lon[i]-Wsite$lon[j])^2))
	if(i!=j&D.W[i,j]==0){
	D.Wcor[i,j]<-.001
	}else{D.Wcor[i,j]<-D.W[i,j]}
	
	}
}



#set up data list

datamodellist<-list(NobsS=dim(datSNiii)[1],n.factS=datSNiii$n,
					EVIS=datSNiii$EVI, yearS=datSNiii$yearID,
					siteS=datSNiii$siteIDm, NyearS=dim(Syear)[1],
					xS=Syear$yearID, yS=rep(1,dim(Syear)[1]),
					NsiteS=dim(Ssite)[1], D.S=D.Scor,
					NobsW=dim(datWNiii)[1], n.factW=datWNiii$n, EVIW=datWNiii$EVI,
					yearW=datWNiii$yearID,siteW=datWNiii$siteIDm,
					NyearW=dim(Wyear)[1], xW=Wyear$wyear,yW=rep(1,dim(Wyear)[1]),
					NsiteW=dim(Wsite)[1], D.W=D.Wcor)
					
Samplelist<-c("deviance", "nbeta.star1","nbeta.star2","nbeta1", "nbeta2","eps.star","alpha.star",
				"mu.nS","epsW.star","alphaW.star","mu.nW","rho.alphaW",
				"rho.epsW","rho.alpha",
				"rho.eps")
				
inits.SN<-list(list(t.eps=1,t.alpha=1,rho.alpha=.99,rho.eps=.99,
					t.epsW=1,t.alphaW=1,rho.alphaW=.99,rho.epsW=.99),
				list(t.eps=1.5,t.alpha=1.5,rho.alpha=.98,rho.eps=.90,
					t.epsW=1.5,t.alphaW=1.5,rho.alphaW=.98,rho.epsW=.90),
				list(t.eps=.5,t.alpha=.5,rho.alpha=.97,rho.eps=.83,
					t.epsW=.5,t.alphaW=.5,rho.alphaW=.97,rho.epsW=.83))
				
n.model.init=jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\n_model\\N_model_code.r",
						data=datamodellist,
						n.adapt=1000,
						n.chains=3,
						inits=inits.SN)
						
#run initial coda samples to check burn in and check duration needed for run
#define burn in and iteration
#note: n.iter does not include n.adapt			  
n.iter.i=35000
codaobj.init = coda.samples(n.model.init,variable.names=Samplelist,
                       n.iter=n.iter.i, thin=15)
					   
  
#check trace plots
#plot function will prompt to click to display each window of parameters
windows(18)
plot(codaobj.init[,"eps.star[26]"], ask=TRUE)	


#generate summary

Mod.out<-summary(codaobj.init)	

write.table(Mod.out$statistics, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_variaion1_stats.csv",
			sep=",",row.names=TRUE)
write.table(Mod.out$quantiles, "c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_variaion1_quant.csv",
			sep=",",row.names=TRUE)
						


				  
