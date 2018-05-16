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
		mu.Soil[i] <- beta0[regVege[i]]+ beta1[regVege[i]]*depth[i] + beta2[regVege[i]]*(mu.AirP[i]-AirPbar[reg[i]]) + beta3[regVege[i]]*(precip[i]-precipbar[reg[i]])
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regVege[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error model for air parameter 
		mu.AirP[i] ~ dnorm(AirP[i], tau.Air[i])
		tau.Air[i] <- pow(sig.Air[i],-2)
		
	}
	#priors for parameters
	for(i in 1:NregVege){
		beta0[i] ~ dnorm(0,.0001)
		beta1[i] ~ dnorm(0,.0001)
		beta2[i] ~ dnorm(0,.0001)
		beta3[i] ~ dnorm(0,.0001)
		sigSoilV[i] ~ dunif(0,100)
	}
}	