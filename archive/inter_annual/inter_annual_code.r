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
		mu.Soil[i] <- beta0[regVege[i]]+ beta1[regVege[i]]*depth[i] + beta2[regVege[i]]*(mu.AirP[i]-SiteAirMean[reg[i]]) 
						+ beta3[regVege[i]]*(mu.pastMax[i]-SiteMaxMean[i])+
						beta4[regVege[i]]*(mu.pastMin[i]-SiteMinMean[i])
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regVege[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error models
		mu.AirP[i] ~ dnorm(AirP[i],tau.AirP[i])
		tau.AirP[i] <- pow(sig.AirP[i], -2)
		mu.pastMax[i] ~ dnorm(pastMax[i],tau.pastMax[i])
		tau.pastMax[i] <- pow(sig.pastMax[i],-2)
		mu.pastMin[i] ~ dnorm(pastMin[i],tau.pastMin[i])
		tau.pastMin[i] <- pow(sig.pastMin[i],-2)
		
	
	}
	
	############################
	###monitor means for plot###
	############################
	for(i in 1:NmuPlot){
		for(j in 1:NregVege){
			plotDepth[i,j] <- beta0[j] + beta1[j] * seqDepth[i]
			plotAir[i,j] <- beta0[j] + beta2[j] * (seqAir[i,regV[j]]-SiteAirMean[regV[j]])
			plotMax[i,j] <- beta0[j] + beta3[j] * (seqMax[i,regV[j]])
			plotMin[i,j] <- beta0[j] + beta4[j] * (seqMin[i,regV[j]])
		}
	
	}
	
	############################
	###priors                ###
	############################
	for(j in 1:NregVege){
	beta0[j] ~ dnorm(mu.beta0[regV[j]],tau.beta0[regV[j]])
	beta1[j] ~ dnorm(mu.beta1[regV[j]],tau.beta1[regV[j]])
	beta2[j] ~ dnorm(mu.beta2[regV[j]],tau.beta2[regV[j]])
	beta3[j] ~ dnorm(mu.beta3[regV[j]],tau.beta3[regV[j]])
	beta4[j] ~ dnorm(mu.beta4[regV[j]],tau.beta4[regV[j]])
	sigSoilV[j] ~ dunif(0,100)
	}
	#hyperpriors
	for(k in 1:3){
	mu.beta0[k] ~ dnorm(0,.0001)
	mu.beta1[k] ~ dnorm(0,.0001)
	mu.beta2[k] ~ dnorm(0,.0001)
	mu.beta3[k] ~ dnorm(0,.0001)
	mu.beta4[k] ~ dnorm(0,.0001)
	
	tau.beta0[k] <- pow(sig.beta0[k],-2)
	tau.beta1[k] <- pow(sig.beta1[k],-2)
	tau.beta2[k] <- pow(sig.beta2[k],-2)
	tau.beta3[k] <- pow(sig.beta3[k],-2)
	tau.beta4[k] <- pow(sig.beta4[k],-2)
	sig.beta0[k] ~ dunif(0,100)
	sig.beta1[k] ~ dunif(0,100)
	sig.beta2[k] ~ dunif(0,100)
	sig.beta3[k] ~ dunif(0,100)
	sig.beta4[k] ~ dunif(0,100)
	}
}