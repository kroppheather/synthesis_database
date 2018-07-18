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
		mu.Soil[i] <- beta0[regsiteID[i]]+ beta1[regsiteID[i]]*depth[i] + beta2[regsiteID[i]]*(mu.AirP[i]-AirPbar[regID[i]])
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regID[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error model for air parameter 
		mu.AirP[i] ~ dnorm(AirP[i], tau.Air[i])
		tau.Air[i] <- pow(sig.Air[i],-2)
		
		
	}
	#site regression model
	for(i in 1:Nregsite){
	beta0[i] <- a0[regS[i]] + a1[regS[i]]*shrubC[i] + a2[regS[i]]*mossC[i]
	beta1[i] <- b0[regS[i]] + b1[regS[i]]*shrubC[i] + b2[regS[i]]*mossC[i]
	beta2[i] <- c0[regS[i]] + c1[regS[i]]*shrubC[i] + c2[regS[i]]*mossC[i]
	
	}
	for(i in 1:Nmonitor){
		for(j in 1:Nregsite){
			mu.site.air[i,j] <- beta0[j] + beta2[j]*(monitorAir[i]-AirPbar[EregID[i]])
			mu.site.depth[i,j] <- beta0[j]+ beta1[j]*monitordepth[i] 
			
		}	
	}

	#priors
	for(i in 1:3){
		a0[i] ~ dnorm(0,0.0001)
		a1[i] ~ dnorm(0,0.0001)
		a2[i] ~ dnorm(0,0.0001)
		b0[i] ~ dnorm(0,0.0001)
		b1[i] ~ dnorm(0,0.0001)
		b2[i] ~ dnorm(0,0.0001)
		c0[i] ~ dnorm(0,0.0001)
		c1[i] ~ dnorm(0,0.0001)
		c2[i] ~ dnorm(0,0.0001)
		sigSoilV[i] ~dunif(0,1000)
	}
}	