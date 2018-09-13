##########################################################
########Vegetation interannual soil temp       ###########
########Heather Kropp started August 2018      ###########
##########################################################
##########################################################

model{
	############################
	###likelihood            ###
	############################
	for(i in 1:Nobs){
		SoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#mean model
		mu.Soil[i] <- beta0[regVege[i]]+ beta1[regVege[i]]*depth[i] + beta2[regVege[i]]*(mu.AirP[i]-AirPbar[reg[i]]) 
						+ beta3[regVege[i]]*(mu.past.air[i]-pastairbar[reg[i]])
						#+beta4[regVege[i]]+(pastSoil[i]-meanpastSoilT[regVege[i]])
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regVege[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error model for air parameter 
		mu.AirP[i] ~ dnorm(AirP[i], tau.Air[i])
		tau.Air[i] <- pow(sig.Air[i],-2)
		#error model for previous season air parameter
		mu.past.air[i] ~ dnorm(pastAir[i], tau.pastAir[i])
		tau.pastAir[i] <- pow(sig.pastAir[i],-2)
	
	}
	
	############################
	###priors                ###
	############################
	for(j in 1:NregVege){
	beta0[j] ~ dunif(lower[regV[j]],upper[regV[j]])
	beta1[j] ~ dnorm(0,.0001)
	beta2[j] ~ dnorm(0,.0001)
	beta3[j] ~ dnorm(0,.0001)
	#beta4[j] ~ dnorm(0,.0001)
	sigSoilV[j] ~ dunif(0,100)
	}


}