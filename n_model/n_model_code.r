model{
	#Thawing N factor model
	for(i in 1:NobsT){
	nT[i]~dnorm(mu.nT[i], tau.nT)
	mu.nT[i]<-betaT1[orgvegeID.T[i]]+ betaT2[orgvegeID.T[i]]*depth.T[i] + betaT3[orgvegeID.T[i]]*Tmax[i]

	nT.rep[i]~dnorm(mu.nT[i], tau.nT)
	}
	#Freezing N factor model
	for(i in 1:NobsF){
	nF[i]~dnorm(mu.nF[i], tau.nF)
	mu.nF[i]<-betaF1[orgvegeID.F[i]]+ betaF2[orgvegeID.F[i]]*depth.F[i] + betaF3[orgvegeID.F[i]]*Tmin[i]
	nF.rep[i]~dnorm(mu.nF[i], tau.nF)
	}
	
	#empirical mean model priors
	for(i in 1:Nvegeorg){
		
		#priors for other regression
		#freezing
		betaF1[i]~dnorm(mu.bF1[soilID[i]],tau.bF1[soilID[i]])
		betaF2[i]~dnorm(mu.bF2[soilID[i]],tau.bF2[soilID[i]])
		betaF3[i]~dnorm(mu.bF3[soilID[i]],tau.bF3[soilID[i]])
		#thawing
		betaT1[i]~dnorm(mu.bT1[soilID[i]],tau.bT1[soilID[i]])
		betaT2[i]~dnorm(mu.bT2[soilID[i]],tau.bT2[soilID[i]])
		betaT3[i]~dnorm(mu.bT3[soilID[i]],tau.bT2[soilID[i]])
	}
	#hyper priors for soil type
	#means
	for(i in 1:Nsoil){
	mu.bF1[i]~dnorm(0,.001)
	mu.bF2[i]~dnorm(0,.001)
	mu.bF3[i]~dnorm(0,.001)
	mu.bT1[i]~dnorm(0,.001)
	mu.bT2[i]~dnorm(0,.001)
	mu.bT3[i]~dnorm(0,.001)
	
	#variance terms
	tau.bF1[i]<-pow(sig.bF1[i],-2)
	sig.bF1[i]<-abs(t.bF1[i])
	t.bF1[i]~dt(0,B.F1[i],2)
	B.F1[i]<-1/(A.F1[i]*A.F1[i])
	A.F1[i]<-10
	#F2
	tau.bF2[i]<-pow(sig.bF2[i],-2)
	sig.bF2[i]<-abs(t.bF2[i])
	t.bF2[i]~dt(0,B.F2[i],2)
	B.F2[i]<-1/(A.F2[i]*A.F2[i])
	A.F2[i]<-10
	#F3
	tau.bF3[i]<-pow(sig.bF3[i],-2)
	sig.bF3[i]<-abs(t.bF3[i])
	t.bF3[i]~dt(0,B.F3[i],2)
	B.F3[i]<-1/(A.F3[i]*A.F3[i])
	A.F3[i]<-10
	#T1
	tau.bT1[i]<-pow(sig.bT1[i],-2)
	sig.bT1[i]<-abs(t.bT1[i])
	t.bT1[i]~dt(0,B.T1[i],2)
	B.T1[i]<-1/(A.T1[i]*A.T1[i])
	#T2
	A.T1[i]<-10
	tau.bT2[i]<-pow(sig.bT2[i],-2)
	sig.bT2[i]<-abs(t.bT2[i])
	t.bT2[i]~dt(0,B.T2[i],2)
	B.T2[i]<-1/(A.T2[i]*A.T2[i])
	A.T2[i]<-10
	#F3
	tau.bT3[i]<-pow(sig.bT3[i],-2)
	sig.bT3[i]<-abs(t.bT3[i])
	t.bT3[i]~dt(0,B.T3[i],2)
	B.T3[i]<-1/(A.T3[i]*A.T3[i])
	A.T3[i]<-10	
	
	
	
	}

	
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