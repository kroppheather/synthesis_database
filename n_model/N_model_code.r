#Model for initial analysis of n factor
#################################################
#######Version 1: this is simply meant###########
#######  to explore how much variation ##########
#######  is explained by the site      ##########
#######  location, depth, and year     ##########
#################################################
model{
	
	###mean model for summer observations
	for(i in 1: NobsS){
		#model likelihood
		n.factS[i]~dnorm(mu.nS[i], tau.S)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nS[i]<-nbeta1[1]+nbeta1[2]*DistS[i]+eps[yearS[i]]+alpha[siteS[i]]
		
	}
	#create identifiable intercept
	nbeta.star1<-nbeta1[1]+eps.mean+alpha.mean
	
	###mean model for winter observations
	for(i in 1:NobsW){
		#model likelihood
		n.factW[i]~dnorm(mu.nW[i], tau.W)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nW[i]<-nbeta2[1]+nbeta2[2]*DistW[i]+epsW[yearW[i]]+alphaW[siteW[i]]
		
	}
	
	#create identifiable intercept
		nbeta.star2<-nbeta2[1]+epsW.mean+alphaW.mean	
		
		
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
	
	
	##Site random effects
	
	#Summer
	alphaW[1:NsiteW]~dmnorm(mu.alphaW[1:NsiteW],Omega.alphaW[1:NsiteW,1:NsiteW])
	#set up mean
	for(i in 1:NsiteW){
		#mean is zero for random effect
		mu.alphaW[i]<-0
		#calculate identifiable 
		alphaW.star[i]<-alphaW[i]-alphaW.mean
	
	}
	#calculate mean of alpha
	alphaW.mean<-mean(alphaW[])

	### Temporal covariance structure for site
	Omega.alphaW[1:NsiteW,1:NsiteW]<-inverse(Sigma.alphaW[1:NsiteW,1:NsiteW])
	
	#now calculate standard deviation for covariance matrix
	for(y in 1:NsiteW){
		for(m in 1:NsiteW){
			Sigma.alphaW[y,m]<-(1/tau.alphaW)*exp(phi.alphaW*D.W[y,m])
		
		}
	}

	
	#set up priors for year covariance
	#use a folded t
	tau.alphaW<-pow(sig.alphaW,-2)
	sig.alphaW<-abs(t.alphaW)
	t.alphaW~dt(0,D,2)
	DW<-1/(CW*CW)
	CW<-10
	#prior for measure of autocorrelation
	phi.alphaW<-log(rho.alphaW)
	rho.alphaW~dbeta(1,1)
	
	
	#summer
	alpha[1:NsiteS]~dmnorm(mu.alpha[1:NsiteS],Omega.alpha[1:NsiteS,1:NsiteS])
	#set up mean
	for(i in 1:NsiteS){
		#mean is zero for random effect
		mu.alpha[i]<-0
		#calculate identifiable 
		alpha.star[i]<-alpha[i]-alpha.mean
	
	}
	#calculate mean of alpha
	alpha.mean<-mean(alpha[])

	### Temporal covariance structure for site
	Omega.alpha[1:NsiteS,1:NsiteS]<-inverse(Sigma.alpha[1:NsiteS,1:NsiteS])
	
	#now calculate standard deviation for covariance matrix
	for(y in 1:NsiteS){
		for(m in 1:NsiteS){
			Sigma.alpha[y,m]<-(1/tau.alpha)*exp(phi.alpha*D.S[y,m])
		
		}
	}

	
	#set up priors for year covariance
	#use a folded t
	tau.alpha<-pow(sig.alpha,-2)
	sig.alpha<-abs(t.alpha)
	t.alpha~dt(0,D,2)
	D<-1/(C*C)
	C<-10
	#prior for measure of autocorrelation
	phi.alpha<-log(rho.alpha)
	rho.alpha~dbeta(1,1)
	
	###Priors
	##likelihood variance
	#summer
	tau.S<-pow(sig.S,-2)
	sig.S~dunif(0,3)
	#winter
	tau.W<-pow(sig.S,-2)
	sig.W~dunif(0,3)
	
	#regression parameters
	for(i in 1:2){
			nbeta1[i]~dnorm(0,.001)
			nbeta2[i]~dnorm(0,.001)
	}
	
	
}
