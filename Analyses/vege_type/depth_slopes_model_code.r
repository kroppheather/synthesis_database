##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
##########################################################
##########################################################
#simple hierarchical model for sites with 3 or more obs
model{
	#likelihood
	for(i in 1:Nobs){
		tempParm[i] ~ dnorm(mu.tempParm[i], tauParm[i])
		mu.tempParm[i] <- beta0[SYP[i]] + (beta1[SYP[i]]*depth[i])
		tauParm[i] <- pow(sigParm[i],-2)
		sigParm[i] <- sig.mod[i] + sigP[parmID[i]]
	}
	#priors for mean model for site, water year, and parm type
	for(i in 1:NSYP){	
	beta0[i] ~ dnorm(mu.beta0[vegeParm[i]],tau.beta0[vegeParm[i]])
	beta1[i] ~ dnorm(mu.beta1[vegeParm[i]],tau.beta1[vegeParm[i]])	
	}
	for(i in 1:NvegeParm){
	
	mu.beta0[i] ~ dnorm(mu.beta0C[Parm[i]],tau.beta0C[Parm[i]])
	mu.beta1[i] ~ dnorm(mu.beta1C[Parm[i]],tau.beta1C[Parm[i]])
	tau.beta0[i] <- pow(sig.beta0[i],-2)
	tau.beta1[i] <- pow(sig.beta1[i],-2)
	sig.beta0[i] ~ dunif(0,100)
	sig.beta1[i] ~ dunif(0,100)
	
	}
	
	#priors for standard deviation
	for(i in 1:Nparm){
	sigP[i] ~ dunif(0,100)
	mu.beta0C[i] ~ dnorm(0,.0001)
	mu.beta1C[i] ~ dnorm(0,.0001)
	tau.beta0C[i] <- pow(sig.beta0C[i],-2)
	sig.beta0C[i] ~ dunif(0,100)
	tau.beta1C[i] <- pow(sig.beta1C[i],-2)
	sig.beta1C[i] ~ dunif(0,100)
	}
}

