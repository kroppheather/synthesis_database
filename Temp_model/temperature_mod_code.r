model{
	#likelihood for air temperature observations

	for(i in 1:NobsA){
		TempA[i]~dnorm(muA[i], tau.muA)
		T.offA[i]<-T.yrA[i]-startA[site.depthidA[i]]
		
		sineA[i]<-T.aveA[site.depthidA[i]]-(AmpA[site.depthidA[i]]*sin(2*3.14159265*T.offA[i]))
		
		#cacluation for freezing degree day
		#set to zero if not freezing
		#FreezeA[i]<-(1-step(TempA[i]))*TempA[i]
		#cacluation for thawing degree day
		#set to zero if not above zero
		#ThawA[i]<-step(TempA[i])*TempA[i]		
	}	
		muA[1]<-sineA[1]
	for(i in 2:NobsA){	
		muA[i]<-(step(startFLAGA[i])*(sineA[i]+airAR[site.depthidA[i]]*(TempA[i-1]-sineA[i-1])))+((1-step(startFLAGA[i]))*sineA[i])
		
	}

	#likelihood for soil temperature observations
		for(i in 1:NobsS){
		TempS[i]~dnorm(muS[i], tau.muS)
		T.offS[i]<-T.yrS[i]-startS[site.depthidS[i]]
		sineS[i]<-T.aveS[site.depthidS[i]]-(AmpS[site.depthidS[i]]*sin(2*3.14159265*T.offS[i]))
		#cacluation for freezing degree day
		#set to zero if not freezing
		#FreezeS[i]<-(1-step(TempS[i]))*TempS[i]
		#cacluation for thawing degree day
		#set to zero if not above zero		
		#ThawS[i]<-step(TempS[i])*TempS[i]	
	}
	muS[1]<-sineS[1]
	for(i in 2:NobsS){
		muS[i]<-(step(startFLAGS[i])*(sineS[i]+soilAR[site.depthidS[i]]*(TempS[i-1]-sineS[i-1])))+((1-step(startFLAGS[i]))*sineS[i])

		}

	#look at a subset of replicated data since there is too much temp data to monitor
	for(i in 1:NrepS){
		TempS.rep[i]~dnorm(muS[SrepSub[i]], tau.muS)
	
	}
	
	for(i in 1:NrepA){
		TempA.rep[i]~dnorm(muA[ArepSub[i]], tau.muA)
	
	}
	
	#prior for likelihood

	
	for(i in 1:NsitedepthA){
		airAR[i]~dunif(-1.1,1.1)
		startA[i]~dunif(0,.3)
		T.aveA[i]~dnorm(0,.001)
		AmpA[i]~dunif(0,70)
	}
	#prior for likelihood

	
	for(i in 1:NsitedepthS){
		soilAR[i]~dunif(-1.1,1.1)
		startS[i]~dunif(0,.3)
		T.aveS[i]~dnorm(0,.001)
		AmpS[i]~dunif(0,70)
	}
	#now need to calculate the predicted temperature for all air observations
	#this is going to vary by site, wyear, and depth
	#get the predicted temperature
	
	#add up thawing and freezing degree days
	#for(i in 1:NSDWA){
	#	FDDA[i]<-sum(FreezeA[ASY[i]:AEY[i]])
	#	TDDA[i]<-sum(ThawA[ASY[i]:AEY[i]])
	#}		
	#for(i in 1:NSDWS){
	#	FDDS[i]<-sum(FreezeS[SSY[i]:SEY[i]])
	#	TDDS[i]<-sum(ThawS[SSY[i]:SEY[i]])
	#}	
	
	#now calculate N factors
	#for(i in 1:Ncombo){
	#	Fn[i]<-FDDS[SoilIND[i]]/FDDA[AirIND[i]]
	#	Tn[i]<-TDDS[SoilIND[i]]/TDDA[AirIND[i]]
	#}
	
	#prior for variance term
	tau.muA<-pow(sig.muA,-2)
	sig.muA~dunif(0,100)
	#prior for variance term
	tau.muS<-pow(sig.muS,-2)
	sig.muS~dunif(0,100)
}