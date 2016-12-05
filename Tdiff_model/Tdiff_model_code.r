###############################################################################
########## Model for looking at the growing season ############################
########## air temperature difference              ############################
###############################################################################

model{
	###Likelihood
	for(i in 1:Nobs){
	Tdiff[i]~dnorm(mu.Td[i],tau.Td)
	mu.Td[i]<-beta1[siteid[i]]+(step(Seas.mid-SeasX[i])*beta2[siteid[i]]*SeasX[i])+((1-step(Seas.mid-SeasX[i]))*beta3[siteid[i]]*SeasX[i])+
		eps[yearid[i]]
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
		beta1[i]~dnorm(0,0.001)
		beta2[i]~dnorm(0,0.001)
		beta3[i]~dnorm(0,0.001)
	}
	#variance for T diff
	tau.Td<-pow(sig.Td,-2)
	sig.Td~dunif(0,40)

}
