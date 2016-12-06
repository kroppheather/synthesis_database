#this model looks at a subset of N factors 
#vegetation percent cover data

model{
###mean model for summer observations
	for(i in 1: NobsS){
		#model likelihood
		n.factS[i]~dnorm(mu.nS[i], tau.S)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nS[i]<-nbeta1S[regIDS[i]]+nbeta2S[regIDS[i]]*EVIS[i]+nbeta3S[regIDS[i]]*OLTS[i]+nbeta4S[regIDS[i]]*shrubCS[i]+nbeta5S[regIDS[i]]*mossCS[i]
		
	}

	
	###mean model for winter observations
	for(i in 1:NobsW){
		#model likelihood
		n.factW[i]~dnorm(mu.nW[i], tau.W)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nW[i]<-nbeta1W[regIDW[i]]+nbeta2W[regIDW[i]]*EVIW[i]+nbeta3W[regIDW[i]]*OLTW[i]+nbeta4W[regIDW[i]]*shrubCW[i]+nbeta5W[regIDW[i]]*mossCW[i]
		
	}


	
	###Priors
	##likelihood variance
	#summer
	tau.S<-pow(sig.S,-2)
	sig.S~dunif(0,3)
	#winter
	tau.W<-pow(sig.S,-2)
	sig.W~dunif(0,3)
	
	#regression parameters
	for(i in 1:Nreg){
			nbeta1S[i]~dnorm(0,.001)
			nbeta2S[i]~dnorm(0,.001)
			nbeta3S[i]~dnorm(0,.001)
			nbeta4S[i]~dnorm(0,.001)
			nbeta5S[i]~dnorm(0,.001)
			nbeta1W[i]~dnorm(0,.001)
			nbeta2W[i]~dnorm(0,.001)
			nbeta3W[i]~dnorm(0,.001)
			nbeta4W[i]~dnorm(0,.001)
			nbeta5W[i]~dnorm(0,.001)	
			
	}
	



}
