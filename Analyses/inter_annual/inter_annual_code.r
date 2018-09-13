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
						+ beta3[regVege[i]]*(antAirMax[i]-meanpastMax[reg[i]])
		#standard deviation that accounts for parameter standard deviation
		tau.Soil[i] <- pow(sig.Soil[i],-2)
		sig.Soil[i] <- sigMod[i] + sigSoilV[regVege[i]]
		#replicated data
		repSoilP[i] ~ dnorm(mu.Soil[i],tau.Soil[i])
		#error model for air parameter 
		mu.AirP[i] ~ dnorm(AirP[i], tau.Air[i])
		tau.Air[i] <- pow(sig.Air[i],-2)

	
	}
	
	############################
	###antecedent            ###
	############################
	#antecedent model for past soil temperature
	for(j in 1:NregVege){
		for(m in 1:Nlag){
			deltaTmax[m,j]~dgamma(1,1)
			wTmax[m,j]<-deltaTmax[m,j]/sumTmax[j]

			
		}
	}
	#calculate sums of unweighted delta values


	for(j in 1:NregVege){
		sumTmax[j]<-sum(deltaTmax[,j])


	}	
	#calculate weighted temperature for each year in the past in the past
	for(i in 1:Nobs){
		for(m in 1:Nlag){

			T.tempmax[i,m]<-wTmax[m,regVege[i]]*a.Tmax[i,m]
		}

	}	


	#final antecedent calculations for soil values

	for(i in 1:Nobs){
		antAirMax[i]<-sum(T.tempmax[i,])

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