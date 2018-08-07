##########################################################
########Vegetation soil temp                   ###########
########Heather Kropp started July 2018         ###########
##########################################################
##########################################################
#start with simple non-hierarchical model
model{
	#likelihood
	for(i in 1:Nobs){
		SoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#mean model
		mu.Soil[i] <- beta0[regvegeID[i]]+ beta1[regvegeID[i]]*depth[i] + beta2[regvegeID[i]]*(mu.AirP[i]-AirPbar[regID[i]]) + 
						beta3[regvegeID[i]]*shrubC[i] + beta4[regvegeID[i]]*mossC[i]
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regID[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error model for air parameter 
		mu.AirP[i] ~ dnorm(AirP[i], tau.Air[i])
		tau.Air[i] <- pow(sig.Air[i],-2)
		
		
	}

	#monitor mean regression value across range of x values
	for(i in 1:Nmonitor){
		for(j in 1:Nregvege){
			mu.site.air[i,j] <- beta0[j] + beta2[j]*(monitorAir[i]-AirPbar[EregID[i]])
			mu.site.depth[i,j] <- beta0[j]+ beta1[j]*monitordepth[i] 
			mu.site.shrub[i,j] <- beta0[j] + beta3[j]*monitorShrub[i]
			mu.site.moss[i,j] <- beta0[j] + beta4[j]*monitorMoss[i]
		}	
	}

	#priors
	for(i in 1:Nregvege){
		beta0[i] ~ dnorm(0,0.0001)
		beta1[i] ~ dnorm(0,0.0001)
		beta2[i] ~ dnorm(0,0.0001)
		beta3[i] ~ dnorm(0,0.0001)
		beta4[i] ~ dnorm(0,0.0001)
		sigSoilV[i] ~dunif(0,1000)
	}
	
	
	
}	