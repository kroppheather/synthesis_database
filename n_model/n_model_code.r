model{
	#Thawing N factor model
	for(i in 1:NobsT){
	nT[i]~dnorm(mu.nT[i], tau.nT)
	mu.nT[i]<-betaT1[biomeID.T[i]]+ betaT2[biomeID.T[i]]*EVI.T[i] + betaT3[biomeID.T[i]]*depth.T[i]
				+ betaT4[biomeID.T[i]]*OLT.T[i] + epsT[yearid.T[i]]
	nT.rep[i]~dnorm(mu.nT[i], tau.nT)
	}
	#Freezing N factor model
	for(i in 1:NobsF){
	nF[i]~dnorm(mu.nF[i], tau.nF)
	mu.nF[i]<-betaF1[biomeID.F[i]]+ betaF2[biomeID.F[i]]*EVI.F[i] + betaF3[biomeID.F[i]]*depth.F[i]
			+ betaF4[biomeID.F[i]]*OLT.F[i] + epsF[yearid.F[i]]
	nF.rep[i]~dnorm(mu.nT[i], tau.nT)
	}
	
	#empirical mean model priors
	for(i in 1:Nbiome){
		#nonidentifiable intercept
		betaF1[i]~dnorm(0,.0001)
		betaT1[i]~dnorm(0,.0001)
		#identifiable intercept
		betaF1star[i]<-betaF1[i]+epsF.mean
		betaT1start[i]<-betaT1[i]+epsT.mean
		
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
	
	
	#Time random effect model
		#freezing
	epsF[1:NyearF]~dmnorm(mu.epsF[1:NyearF],Omega.epsF[1:NyearF,1:NyearF])
	#set up mean
	for(i in 1:NyearF){
		#mean is zero for random effect
		mu.epsF[i]<-0
		#calculate identifiable 
		epsF.star[i]<-epsF[i]-epsF.mean
	
	}
	#calculate mean of eps
	epsF.mean<-mean(epsF[])

	### Temporal covariance structure for year
	Omega.epsF[1:NyearF,1:NyearF]<-inverse(Sigma.epsF[1:NyearF,1:NyearF])
	
	#now calculate standard deviation for covariance matrixx
	for(y in 1:NyearF){
		for(m in 1:NyearF){
			Sigma.epsF[y,m]<-(1/tau.epsF)*exp(phi.epsF*D.YF[y,m])
		
		}
	}
	#calculate the distance component of Sigma
	for(y in 1:NyearF){
		for(m in 1:NyearF){
			D.YF[y,m]<-sqrt(pow(xF[y]-xF[m],2)+ pow(yF[y]-yF[m],2))
		}
	}
	
	#set up priors for year covariance
	#use a folded t
	tau.epsF<-pow(sig.epsF,-2)
	sig.epsF<-abs(t.epsF)
	t.epsF~dt(0,BF,2)
	BF<-1/(AF*AF)
	AF<-10
	#prior for measure of autocorrelation
	phi.epsF<-log(rho.epsF)
	rho.epsF~dbeta(1,1)	
	
	
	#thawing
	epsT[1:NyearF]~dmnorm(mu.epsT[1:NyearT],Omega.epsT[1:NyearT,1:NyearT])
	#set up mean
	for(i in 1:NyearT){
		#mean is zero for random effect
		mu.epsT[i]<-0
		#calculate identifiable 
		epsT.star[i]<-epsT[i]-epsT.mean
	
	}
	#calculate mean of eps
	epsT.mean<-mean(epsT[])

	### Temporal covariance structure for year
	Omega.epsT[1:NyearT,1:NyearT]<-inverse(Sigma.epsT[1:NyearT,1:NyearT])
	
	#now calculate standard deviation for covariance matrixx
	for(y in 1:NyearT){
		for(m in 1:NyearT){
			Sigma.epsT[y,m]<-(1/tau.epsT)*exp(phi.epsT*D.YT[y,m])
		
		}
	}
	#calculate the distance component of Sigma
	for(y in 1:NyearT){
		for(m in 1:NyearT){
			D.YT[y,m]<-sqrt(pow(xT[y]-xT[m],2)+ pow(yT[y]-yT[m],2))
		}
	}
	
	#set up priors for year covariance
	#use a folded t
	tau.epsT<-pow(sig.epsT,-2)
	sig.epsT<-abs(t.epsT)
	t.epsT~dt(0,BT,2)
	BT<-1/(AT*AT)
	AT<-10
	#prior for measure of autocorrelation
	phi.epsT<-log(rho.epsT)
	rho.epsT~dbeta(1,1)	
	
	#likelihood priors
	#thawing
	tau.nT<-pow(sig.nT, -2)
	sig.nT~dunif(0,10)
	#freezing
	tau.nF<-pow(sig.nF, -2)
	sig.nF~dunif(0,10)

}