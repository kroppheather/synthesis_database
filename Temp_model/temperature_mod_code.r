model{
	#likelihood for air temperature observations
	for(i in 1:NobsA){
		TempA[i]~dnorm(muA[i], tau.muA)
		muA[i]<-T.aveA[siteAll.A[i]]+ AmpA[siteAll.A[i]]*sin(-2*3.14159265*(T.yrA[i]-startA[siteAll.A[i]]))
	
	}
	
	#likelihood for soil temperature observations
	for(i in 1:NobsS){
		TempS[i]~dnorm(muS[i], tau.muS)
		muS[i]<-T.aveS[siteAll.S[i]]+ 
			(AmpS[siteAll.S[i]]*exp(-b[siteAll.S[i]]*depthF[i]))*sin(-2*3.14159265*(T.yrS[i]-(startS[siteAll.S[i]]+(b[siteAll.S[i]]*depthF[i]))))
	
	}
	#prior for likelihood
	for(i in 1:NsiteAll){
		T.aveA[i]~dnorm(0,.0001)
		AmpA[i]~dunif(0,100)
		startA[i]~dunif(0,.65)
	}
	#prior for likelihood
	for(i in 1:NsiteAll){
		T.aveS[i]~dnorm(0,.01)
		AmpS[i]~dunif(0,30)
		startS[i]~dunif(0,.30)
		b[i]~dnorm(mu.b[vegClass[i]], tau.b)T(0,.3)
	}
		#priors for b
		
	for(i in 1:NVClass){
		mu.b[i]~dnorm(mu.bAll, tau.ball)T(0,.3)
		
		}
		mu.bAll~dunif(0,.3)
		tau.b<-pow(sig.b,-2)
		sig.b~dunif(0,1)
		tau.ball<-pow(sig.b,-2)
		sig.ball~dunif(0,1)
	#prior for variance term
	tau.muA<-pow(sig.muA,-2)
	sig.muA~dunif(0,100)
	#prior for variance term
	tau.muS<-pow(sig.muS,-2)
	sig.muS~dunif(0,100)
}