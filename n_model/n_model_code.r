model{
	#Thawing N factor model
	for(i in 1:NobsT){
	nT[i]~dnorm(mu.nT[i], tau.nT)
	mu.nT[i]<-betaT1[orgID.T[i]]+ betaT2[orgID.T[i]]*EVI.T[i] + betaT3[orgID.T[i]]*depth.T[i]
				 + epsT[vegeID.T[i]]
	nT.rep[i]~dnorm(mu.nT[i], tau.nT)
	}
	#Freezing N factor model
	for(i in 1:NobsF){
	nF[i]~dnorm(mu.nF[i], tau.nF)
	mu.nF[i]<-betaF1[orgID.F[i]]+ betaF2[orgID.F[i]]*EVI.F[i] + betaF3[orgID.F[i]]*depth.F[i]
			+ epsF[vegeID.F[i]]
	nF.rep[i]~dnorm(mu.nF[i], tau.nF)
	}
	
	#empirical mean model priors
	for(i in 1:Norg){
		#nonidentifiable intercept
		betaF1[i]~dnorm(0,.0001)
		betaT1[i]~dnorm(0,.0001)
		#identifiable intercept
		betaF1star[i]<-betaF1[i]+epsF.mean
		betaT1star[i]<-betaT1[i]+epsT.mean
		
		#priors for other regression
		#freezing
		betaF2[i]~dnorm(0,.0001)
		betaF3[i]~dnorm(0,.0001)
		betaF4[i]~dnorm(0,.0001)
		#thawing
		betaT2[i]~dnorm(0,.0001)
		betaT3[i]~dnorm(0,.0001)
		betaT4[i]~dnorm(0,.0001)
	}
	
	#eps model for vegetation class
	for(i in 1:Nvegeclass){
	epsF[i]~dnorm(0, tau.epsF)
	epsF.star[i]<-epsF[i]-epsF.mean
	epsT[i]~dnorm(0, tau.epsT)
	epsT.star[i]<-epsT[i]-epsT.mean
	}
	#sweeping for eps
	epsT.mean<-mean(epsT[])
	epsF.mean<-mean(epsF[])
	
	#eps variance priors
	tau.epsF~dgamma(0.001,0.001)
	sig.epsF<-sqrt(1/tau.epsF)
	tau.epsT~dgamma(0.001,0.001)
	sig.epsT<-sqrt(1/tau.epsT)	
	
	
	#likelihood priors
	#thawing
	tau.nT<-pow(sig.nT, -2)
	sig.nT~dunif(0,10)
	#freezing
	tau.nF<-pow(sig.nF, -2)
	sig.nF~dunif(0,10)

}