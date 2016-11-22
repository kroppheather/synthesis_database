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


#calculate the distance between the two measurements

datWN$Dist<-datWN$height+(datWN$depth/100)
datSN$Dist<-datSN$height+(datSN$depth/100)

#now work on structuring the data to match up with the model
#start by getting the indexes together for the data
#set up a table to get unique site
Ssite<-unique(data.frame(siteid=datSN$siteid, lat=datSN$lat, lon=datSN$lon,
						loc=datSN$loc))
Ssite$siteIDm<-seq(1,length(Ssite$siteid))

#now create the siteid to match the summer n factor
datSNi<-join(datSN,Ssite, by=c("siteid","lat","lon","loc"), type="right")



#need an index that varies by N data for site and year
#get unique years in the dataset
Syear<-data.frame(year=unique(datSN$year))
Syear$year<-sort.int(Syear$year)
Syear$yearID<-seq(1,length(Syear$year))

#now add the year Id to the table too
datSNii<-join(datSNi,Syear,by=c("year"), type="right")

#siteid 22-33,43-45,42+46,54-186,57-59,63-63,72-73
#have same lat lon even though taken at different plots
#see if that is causing the covariance matrix problems
#add slight variation to see if it helps
#calculate D.S in code
D.S<-matrix(rep(NA,73*73), ncol=73)
D.Scor<-matrix(rep(NA,73*73), ncol=73)
for(i in 1:73){
	for(j in 1:73){
		D.S[i,j]<-sqrt(((Ssite$lat[i]-Ssite$lat[j])^2)+ 
					((Ssite$lon[i]-Ssite$lon[j])^2))
	if(i!=j&D.S[i,j]==0){
	D.Scor[i,j]<-.001
	}else{D.Scor[i,j]<-D.S[i,j]}
	
	}
}


#set up data list
datamodellist<-list(NobsS=dim(datSNii)[1],n.factS=datSNii$n,
					DistS=datSNii$Dist, yearS=datSNii$yearID,
					siteS=datSNii$siteIDm, NyearS=dim(Syear)[1],
					xS=Syear$yearID, yS=rep(1,dim(Syear)[1]),
					NsiteS=dim(Ssite)[1], latS=Ssite$lat,
					longS=Ssite$lon, D.S=D.Scor)
Samplelist<-c("deviance", "nbeta.star", "nbeta[2]","eps.star","alpha.star",
				"mu.nS")
				
inits.SN<-list(list(t.eps=1,t.alpha=1,rho.alpha=.99,rho.eps=.99),
				list(t.eps=1.5,t.alpha=1.5,rho.alpha=.98,rho.eps=.90),
				list(t.eps=.5,t.alpha=.5,rho.alpha=.97,rho.eps=.83))
				
n.model.init=jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\n_model\\N_model_code.r",
						data=datamodellist,
						n.adapt=1000,
						n.chains=3,
						inits=inits.SN)
						
#run initial coda samples to check burn in and check duration needed for run
#define burn in and iteration
#note: n.iter does not include n.adapt			  
n.iter.i=3000
codaobj.init = coda.samples(n.model.init,variable.names=Samplelist,
                       n.iter=n.iter.i, thin=5)
					   
#check trace plots
#plot function will prompt to click to display each window of parameters
plot(codaobj.init, ask=TRUE)						
						
						
### try troubleshooting in bugs						
#library(R2OpenBUGS)
#library(coda)
#n.model.initial<-bugs(data=datamodellist,
#	inits=inits.SN,
#	parameters=Samplelist,
#	n.iter=3000,
#	model.file="c:\\Users\\hkropp\\Documents\\GitHub\\synthesis_database\\n_model\\N_model_code.txt",
#	n.chains=3,
#	n.burnin=1000,
#	n.thin=1,working.directory="c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6",
#	debug=TRUE,codaPkg=TRUE)
#set up model

				  
