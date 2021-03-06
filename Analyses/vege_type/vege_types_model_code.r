##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started May 2018         ###########
##########################################################
##########################################################
#start with simple non-hierarchical model
model{
	#likelihood
	for(i in 1:Nobs){
		SoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#mean model
		mu.Soil[i] <- beta0[regVege[i]]+ beta1[regVege[i]]*depth[i] + beta2[regVege[i]]*(mu.AirP[i]-AirPbar[regVege[i]]) 
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regVege[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error model for air parameter 
		mu.AirP[i] <- ifelse(sig.Air[i] == 0, AirP[i], stoch.AirP[i])
		stoch.AirP[i] ~ dnorm(AirP[i], tau.Air[i])
		
	}
	#priors for parameters
	for(i in 1:NregVege){
		beta0[i] ~ dnorm(mu.beta0[regV[i]],tau.beta0[regV[i]])
		beta1[i] ~ dnorm(mu.beta1[regV[i]],tau.beta1[regV[i]])
		beta2[i] ~ dnorm(mu.beta2[regV[i]],tau.beta2[regV[i]])

		sigSoilV[i] ~ dunif(0,100)
		meanComp[i] <- beta0[i]+ beta2[i]*(Acomp[reg[i]]-AirPbar[i]) 
	}
	
	#hyper priors
	for(i in 1:Nreg){
	mu.beta0[i] ~ dnorm(0,0.00001)
	mu.beta1[i] ~ dnorm(0,0.00001)
	mu.beta2[i] ~ dnorm(0,0.00001)

	
	tau.beta0[i] <- pow(sig.beta0[i],-2)
	tau.beta1[i] <- pow(sig.beta1[i],-2)
	tau.beta2[i] <- pow(sig.beta2[i],-2)
	
	
	sig.beta0[i] ~ dunif(0,100)
	sig.beta1[i] ~ dunif(0,100)
	sig.beta2[i] ~ dunif(0,100)

	}
	
	#monitor regression means for plotting
	for(i in 1:NregVege){
		for(j in 1:200){
			plotRegA[j,i] <- beta0[i]+  beta2[i]*(AcompPlot[j,i]-AirPbar[i])
		}
	
	
	}
}	