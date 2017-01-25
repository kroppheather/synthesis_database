model{
	#likelihood for air temperature observations
	for(i in 1:NobsA){
		TempA[i]~dnorm(muA[i], tau.muA)
		muA[i]<-T.aveA[site.depthidA[i]]+ AmpA[site.depthidA[i]]*sin(-2*3.14159265*(T.yrA[i]-startA[site.depthidA[i]]))
	
	}
	
	#likelihood for soil temperature observations
	for(i in 1:NobsS){
		TempS[i]~dnorm(muS[i], tau.muS)
		muS[i]<-T.aveS[siteM[i]]+ 
			(AmpS[siteM[i]]*exp(-b[siteM[i]]*depthF[i]))*sin(-2*3.14159265*(T.yrS[i]-startS[siteM[i]])+(b[siteM[i]]*depthF[i]))
	
	}
	#prior for likelihood
	for(i in 1:NsitedepthA){
		T.aveA[i]~dnorm(0,.0001)
		AmpA[i]~dunif(0,100)
		startA[i]~dunif(0,.65)
	}
	#prior for likelihood
	for(i in 1:NsiteS){
		T.aveS[i]~dnorm(mu.Ta,tau.Ta)
		AmpS[i]~dnorm(mu.As,tau.As)T(0,)
		startS[i]~dnorm(mu.sS[depthFLAG[i]],tau.sS[depthFLAG[i]])T(0,.7)
		b[i]~dnorm(mu.bS[depthFLAG[i]],tau.bS[depthFLAG[i]])T(1/10000,1/100)
	}
	
	#hyperpriors for temperature funcion
	#mean priors
	mu.Ta~dnorm(0,.001)
	mu.As~dunif(0,30)
	

	#variance priors
	tau.Ta<-pow(sig.Ta,-2)
	tau.As<-pow(sig.As,-2)
	
	
	sig.Ta~dunif(0,30)
	sig.As~dunif(0,30)
	
	
	#do a seperate prior for depth flag zero
	#so it can't influence sites with real data
	for(i in 1:2){
		mu.bS[i]~dunif(1/10000,1/100)	
		tau.bS[i]<-pow(sig.bS[i],-2)
		sig.bS[i]~dunif(0,.1)
		#variance prior for start
		tau.sS[i]<-pow(sig.sS[i],-2)
		sig.sS[i]~dunif(0,.7)
	}
	#
	mu.sS[1]~dunif(0,.7)
	mu.sS[2]~dunif(0,.4)
	#prior for variance term
	tau.muA<-pow(sig.muA,-2)
	sig.muA~dunif(0,100)
	#prior for variance term
	tau.muS<-pow(sig.muS,-2)
	sig.muS~dunif(0,100)
}