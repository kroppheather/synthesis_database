#this model looks at a subset of N factors 
#vegetation percent cover data

model{
###mean model for summer observations
	for(i in 1: NobsS){
		#model likelihood
		n.factS[i]~dnorm(mu.nS[i], tau.S)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nS[i]<-nbeta1S[regIDS[i]] +nbeta2S[regIDS[i]]*(EVIS[i]-EVISm[regIDS[i]])+ nbeta3S[regIDS[i]]*(OLTS[i]-OLTSm[regIDS[i]])+ nbeta4S[regIDS[i]]*(shrubCS[i]-shrubSm[regIDS[i]])+ nbeta5S[regIDS[i]]*(mossCS[i]-mossSm[regIDS[i]])+ eps[yearS[i]]
		
	}

	
	###mean model for winter observations
	for(i in 1:NobsW){
		#model likelihood
		n.factW[i]~dnorm(mu.nW[i], tau.W)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nW[i]<-nbeta1W[regIDW[i]]+ nbeta2W[regIDW[i]]*(EVIW[i]-EVIWm[regIDW[i]])+ nbeta3W[regIDW[i]]*(OLTW[i]-OLTWm[regIDW[i]])+ nbeta4W[regIDW[i]]*(shrubCW[i]-shrubWm[regIDW[i]])+ nbeta5W[regIDW[i]]*(mossCW[i]-mossWm[regIDW[i]])+ epsW[yearW[i]]
		
	}

		
		
	###Random Effects models
	##Year random effects
	#Summer
	eps[1:NyearS]~dmnorm(mu.eps[1:NyearS],Omega.eps[1:NyearS,1:NyearS])
	#set up mean
	for(i in 1:NyearS){
		#mean is zero for random effect
		mu.eps[i]<-0
		#calculate identifiable 
		eps.star[i]<-eps[i]-eps.mean
	
	}
	#calculate mean of eps
	eps.mean<-mean(eps[])

	### Temporal covariance structure for year
	Omega.eps[1:NyearS,1:NyearS]<-inverse(Sigma.eps[1:NyearS,1:NyearS])
	
	#now calculate standard deviation for covariance matrixx
	for(y in 1:NyearS){
		for(m in 1:NyearS){
			Sigma.eps[y,m]<-(1/tau.eps)*exp(phi.eps*D.Y[y,m])
		
		}
	}
	#calculate the distance component of Sigma
	for(y in 1:NyearS){
		for(m in 1:NyearS){
			D.Y[y,m]<-sqrt(pow(xS[y]-xS[m],2)+ pow(yS[y]-yS[m],2))
		}
	}
	
	#set up priors for year covariance
	#use a folded t
	tau.eps<-pow(sig.eps,-2)
	sig.eps<-abs(t.eps)
	t.eps~dt(0,B,2)
	B<-1/(A*A)
	A<-10
	#prior for measure of autocorrelation
	phi.eps<-log(rho.eps)
	rho.eps~dbeta(1,1)
	
	#Winter
	epsW[1:NyearW]~dmnorm(mu.epsW[1:NyearS],Omega.epsW[1:NyearW,1:NyearW])
	#set up mean
	for(i in 1:NyearW){
		#mean is zero for random effect
		mu.epsW[i]<-0
		#calculate identifiable 
		epsW.star[i]<-epsW[i]-epsW.mean
	
	}
	#calculate mean of eps
	epsW.mean<-mean(epsW[])

	### Temporal covariance structure for year
	Omega.epsW[1:NyearW,1:NyearW]<-inverse(Sigma.epsW[1:NyearW,1:NyearW])
	
	#now calculate standard deviation for covariance matrixx
	for(y in 1:NyearW){
		for(m in 1:NyearW){
			Sigma.epsW[y,m]<-(1/tau.epsW)*exp(phi.epsW*D.YW[y,m])
		
		}
	}
	#calculate the distance component of Sigma
	for(y in 1:NyearW){
		for(m in 1:NyearW){
			D.YW[y,m]<-sqrt(pow(xW[y]-xW[m],2)+ pow(yW[y]-yW[m],2))
		}
	}
	
	#set up priors for year covariance
	#use a folded t
	tau.epsW<-pow(sig.epsW,-2)
	sig.epsW<-abs(t.epsW)
	t.epsW~dt(0,BW,2)
	BW<-1/(AW*AW)
	AW<-10
	#prior for measure of autocorrelation
	phi.epsW<-log(rho.epsW)
	rho.epsW~dbeta(1,1)	
	
	
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

		#create identifiable intercept
		nbeta.star1S[i]<-nbeta1S[i]+eps.mean	
		nbeta.star1W[i]<-nbeta1W[i]+epsW.mean			
	}
	



}
