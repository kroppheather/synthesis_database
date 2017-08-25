model{
	#Thawing N factor model
	for(i in 1:NobsT){
	nT[i]~dnorm(mu.nT[i], tau.nT)
	mu.nT[i]<-betaT1[vegeID.T[i]]+ betaT2[vegeID.T[i]]*depth.T[i] + betaT3[vegeID.T[i]]*(Tmax[i]-TmaxAVE[vegeID.T[i]])

	nT.rep[i]~dnorm(mu.nT[i], tau.nT)
	}
	#Freezing N factor model
	for(i in 1:NobsF){
	nF[i]~dnorm(mu.nF[i], tau.nF)
	mu.nF[i]<-betaF1[vegeID.F[i]]+ betaF2[vegeID.F[i]]*depth.F[i] + betaF3[vegeID.F[i]]*(Tmin[i]-TminAVE[vegeID.F[i]])
	nF.rep[i]~dnorm(mu.nF[i], tau.nF)
	}
	
	#empirical mean model priors
	for(i in 1:Nvege){
		
		#priors for other regression
		#freezing
		betaF1[i]~dnorm(0,.001)
		betaF2[i]~dnorm(0,.001)
		betaF3[i]~dnorm(0,.001)
		#thawing
		betaT1[i]~dnorm(0,.001)
		betaT2[i]~dnorm(0,.001)
		betaT3[i]~dnorm(0,.001)
	}
	

	
	
	#likelihood priors
	#thawing
	tau.nT<-pow(sig.nT, -2)
	sig.nT~dunif(0,10)
	#freezing
	tau.nF<-pow(sig.nF, -2)
	sig.nF~dunif(0,10)

}