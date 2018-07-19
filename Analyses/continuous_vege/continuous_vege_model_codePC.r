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
	#monitor mean regression value across range of x values
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
	#monitor the slopes of two extremes
	for(i in 1:3){
		#zero cover of both types
		Xbeta0N[i] <- a0[i]
		Xbeta0S[i] <- a0[i] + a1[i]*100
		Xbeta0M[i] <- a0[i] + a2[i]*80
		Xbeta1N[i] <- b0[i]
		Xbeta1S[i] <- b0[i] + b1[i]*100
		Xbeta1M[i] <- b0[i] + b2[i]*80		
		Xbeta2N[i] <- c0[i]
		Xbeta2S[i] <- c0[i] + c1[i]*100
		Xbeta2M[i] <- c0[i] + c2[i]*80
	}	
	#monitor mean slopes
	for(i in 1:Nmonitor){
		for(j in 1:Nreg){
			mu.X0.depth[i,j] <- Xbeta0N[j] + Xbeta1N[j]*monitordepth[i]
			mu.X0.air[i,j] <- Xbeta0N[j] + Xbeta2N[j]*(monitorAir[i]-AirPbar[EregID[i]])
			mu.Xshrub100.depth[i,j] <- Xbeta0S[j] + Xbeta1S[j]*monitordepth[i]
			mu.Xshrub100.air[i,j] <- Xbeta0S[j] + Xbeta2S[j]*(monitorAir[i]-AirPbar[EregID[i]])
			mu.Xshrub80.depth[i,j] <- Xbeta0M[j] + Xbeta1M[j]*monitordepth[i]
			mu.Xshrub80.air[i,j] <- Xbeta0M[j] + Xbeta2M[j]*(monitorAir[i]-AirPbar[EregID[i]])
		}
	}
	
	
}	