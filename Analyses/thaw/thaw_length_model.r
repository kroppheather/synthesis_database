model{
	#likelihood
	for(i in 1:Nobs){
		SoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#mean model
		mu.Soil[i] <- beta0[Vege[i]]+ beta1[Vege[i]]*depth[i]
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[Vege[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
	}		
	#priors for parameters
	for(i in 1:NVege){
		beta0[i] ~ dnorm(mu.beta0,tau.beta0)
		beta1[i] ~ dnorm(mu.beta1,tau.beta1)	
		sigSoilV[i] ~ dunif(0,100)
		#calculate N at 20 cm
		n.20[i] <- beta0[i]+(beta1[i]*20)
	}
	#hyper priors

		mu.beta0 ~ dnorm(0,0.00001)
		mu.beta1 ~ dnorm(0,0.00001)	
		
		tau.beta0 <- pow(sig.beta0,-2)
		tau.beta1 <- pow(sig.beta1,-2)
		
		sig.beta0 ~ dunif(0,100)
		sig.beta1 ~ dunif(0,100)	
	
}	