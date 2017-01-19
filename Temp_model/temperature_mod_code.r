model{
	#likelihood for air temperature observations
	for(i in 1:NobsA){
		TempA[i]~dnorm(muA[i], tau.muA)
		muA[i]<-T.aveA[site.depthidA[i]]+ AmpA[site.depthidA[i]]*sin(-2*3.14159265*(T.yrA[i]-startA[site.depthidA[i]]))
	
	}
	
	#likelihood for soil temperature observations
	for(i in 1:NobsS){
		TempS[i]~dnorm(muS[i], tau.muS)
		muS[i]<-T.aveS[site.depthidS[i]]+ AmpS[site.depthidS[i]]*sin(-2*3.14159265*(T.yrS[i]-startS[site.depthidS[i]]))
		TSrep[i]~dnorm(muS[i], tau.muS)
	}
	#prior for likelihood
	for(i in 1:NsitedepthA){
		T.aveA[i]~dnorm(0,.0001)
		AmpA[i]~dunif(0,100)
		startA[i]~dunif(0,.5)
	}
	#prior for likelihood
	for(i in 1:NsitedepthS){
		T.aveS[i]~dnorm(0,.0001)
		AmpS[i]~dunif(0,100)
		startS[i]~dunif(0,.7)
	}
	#prior for variance term
	tau.muA<-pow(sig.muA,-2)
	sig.muA~dunif(0,100)
	#prior for variance term
	tau.muS<-pow(sig.muS,-2)
	sig.muS~dunif(0,100)
}