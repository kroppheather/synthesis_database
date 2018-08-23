##########################################################
########Vegetation interannual soil temp       ###########
########Heather Kropp started August 2018      ###########
##########################################################
##########################################################

model{
	for(i in 1:Nobs){
		SoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#mean model
		mu.Soil[i] <- beta0[regVege[i]]+ beta1[regVege[i]]*depth[i] + beta2[regVege[i]]*(mu.AirP[i]-AirPbar[reg[i]]) 
						+ beta3[regVege[i]]*(past.air[i]-pastairbar[reg[i]])+beta4[regVege[i]]+antSoil[i]
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regVege[i]]
	
	
	}


}