##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
##########################################################
##########################################################
#simple non-hierarchical model for sites with 3 or more obs
model{
	#likelihood
	for(i in 1:Nobs){
		tempParm[i] ~ dnorm(mu.tempParm[i], tauParm[i])
		mu.tempParm[i] <- beta0[SYP[i]] + (beta1[SYP[i]]*depth[i])
		tauParm[i] <- pow(sigParm[i],-2)
		sigParm[i] <- sig.mod[i] + sigP[parmID[i]]
	}
	#priors for mean model
	for(i in 1:NSYP){	
	beta0[i] ~ dnorm(0,.0001)
	beta1[i] ~ dnorm(0,0.0001)	
	}
	#priors for standard deviation
	for(i in 1:Nparm){
	sigP[i] ~ dunif(0,100)
	
	}
}

