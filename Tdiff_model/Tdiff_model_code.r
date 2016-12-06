###############################################################################
########## Model for looking at the growing season ############################
########## air temperature difference              ############################
###############################################################################

model{
	###Likelihood
	for(i in 1:Nobs){
	SeasL[i]<-SeasX[i]-.5
	Tdiff[i]~dnorm(mu.Td[i],tau.Td)
	end.seas[i]<-step(SeasL[i])* beta4[siteid[i]]*SeasL[i]+beta5[siteid[i]]*pow(SeasL[i],2)
	beg.seas[i]<-(1-step(SeasL[i]))* (beta2[siteid[i]]*SeasL[i])+beta3[siteid[i]]*pow(SeasL[i],2)
	mu.Td[i]<-beta1[siteid[i]]+end.seas[i]+beg.seas[i]+eps[yearid[i]]
	Tdiff.rep[i]~dnorm(mu.Td[i],tau.Td)
	}	
	#calculate idenfiable intercept
	for(i in 1: Nsites){
		beta1star[i]<-beta1[i]+eps.mean
	}
	#define Seas.mid
	Seas.mid<-0.5
	
	##Year random effects

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
	
	###Define likelihood priors
	for(i in 1:Nsites){
		beta1[i]~dnorm(mu.beta1[regionid[i]],tau.beta1[regionid[i]])
		beta2[i]~dnorm(mu.beta2[regionid[i]],tau.beta2[regionid[i]])
		beta3[i]~dnorm(mu.beta3[regionid[i]],tau.beta3[regionid[i]])
		beta4[i]~dnorm(mu.beta4[regionid[i]],tau.beta4[regionid[i]])
		beta5[i]~dnorm(mu.beta5[regionid[i]],tau.beta5[regionid[i]])	
	}
	#define hyper priors for region
	for(i in 1:Nreg){
	mu.beta1[i]~dnorm(0,.001)
	mu.beta2[i]~dnorm(0,.001)
	mu.beta3[i]~dnorm(0,.001)
	mu.beta4[i]~dnorm(0,.001)
	mu.beta5[i]~dnorm(0,.001)
	#most group sizes are large enough to assume this. may need to explore further though
	tau.beta1[i]<-pow(sig.beta1[i],-2)
	tau.beta2[i]<-pow(sig.beta2[i],-2)
	tau.beta3[i]<-pow(sig.beta3[i],-2)
	tau.beta4[i]<-pow(sig.beta4[i],-2)
	tau.beta5[i]<-pow(sig.beta5[i],-2)
	sig.beta1[i]~dunif(0,200)
	sig.beta2[i]~dunif(0,200)
	sig.beta3[i]~dunif(0,200)
	sig.beta4[i]~dunif(0,200)
	sig.beta5[i]~dunif(0,200)
	}
	
	#variance for T diff
	tau.Td<-pow(sig.Td,-2)
	sig.Td~dunif(0,40)

}
