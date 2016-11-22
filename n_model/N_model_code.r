#Model for initial analysis of n factor
model{
	
	###mean model for summer observations
	for(i in 1: NobsS){
		#model likelihood
		n.factS[i]~dnorm(mu.nS[i], tau.S)
		#mean n factor function of distance from sensor and location and
		#year random effects and location random effects
		mu.nS[i]<-nbeta[1]+nbeta[2]*DistS[i]+eps[yearS[i]]+alpha[siteS[i]]
		
	}
	#create identifiable intercept
	nbeta.star<-nbeta[1]+eps.mean+alpha.mean
	
	###Random Effects models
	
	#year random effects
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
	
	#site random effects
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
	#likelihood variance
	tau.S<-pow(sig.S,-2)
	sig.S~dunif(0,3)
	#regression parameters
	for(i in 1:2){
	nbeta[i]~dnorm(0,.001)
	}
	
	
}
