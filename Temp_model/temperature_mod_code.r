model{
	#likelihood for air temperature observations
	for(i in 1:NobsA){
		TempA[i]~dnorm(muA[i], tau.muA)
		muA[i]<-T.aveA[SDWA[i]]+ AmpA[SDWA[i]]*sin(-2*3.14159265*(T.yrA[i]-startA[site.depthidA[i]]))
		FreezeA[i]<-(1-step(muA[i]))*muA[i]
		ThawA[i]<-step(muA[i])*muA[i]
		
	}
	
	#likelihood for soil temperature observations
	for(i in 1:NobsS){
		TempS[i]~dnorm(muS[i], tau.muS)
		muS[i]<-T.aveS[SDWS[i]]+ AmpS[SDWS[i]]*sin(-2*3.14159265*(T.yrS[i]-startS[site.depthidS[i]]))
		#TSrep[i]~dnorm(muS[i], tau.muS)
		FreezeS[i]<-(1-step(muS[i]))*muS[i]
		ThawS[i]<-step(muS[i])*muS[i]
	}
	#prior for likelihood
	for(i in 1:NSDWA){
		T.aveA[i]~dnorm(0,.0001)
		AmpA[i]~dunif(0,100)	
	}
	
	for(i in 1:NsitedepthA){

		startA[i]~dunif(0,.5)
	}
	#prior for likelihood
	for(i in 1:NSDWS){
		T.aveS[i]~dnorm(0,.0001)
		AmpS[i]~dunif(0,100)	
	
	}
	
	for(i in 1:NsitedepthS){
		startS[i]~dunif(0,.7)
	}
	#now need to calculate the predicted temperature for all air observations
	#this is going to vary by site, wyear, and depth
	#Temp.predA~dnorm(muApred, tau.muA)
	#muApred<-
	#FreezeA<-
	#ThawA<-
	
	
	#add up thawing and freezing degree days
	for(i in 1:NSDWA){
		FDDA[i]<-sum(FreezeA[ASY[i]:AEY[i]])
		TDDA[i]<-sum(ThawA[ASY[i]:AEY[i]])
	}		
	for(i in 1:NSDWS){
		FDDS[i]<-sum(FreezeS[SSY[i]:SEY[i]])
		TDDS[i]<-sum(ThawS[SSY[i]:SEY[i]])
	}	
	
	#now calculate N factors
	for(i in 1:Ncombo){
		Fn[i]<-FDDS[SoilIND[i]]/FDDA[AirIND[i]]
		Tn[i]<-TDDS[SoilIND[i]]/TDDA[AirIND[i]]
	}
	
	#prior for variance term
	tau.muA<-pow(sig.muA,-2)
	sig.muA~dunif(0,100)
	#prior for variance term
	tau.muS<-pow(sig.muS,-2)
	sig.muS~dunif(0,100)
}