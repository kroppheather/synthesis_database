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
						+ beta3[regVege[i]]*(mu.past.air[i]-pastairbar[reg[i]])+beta4[regVege[i]]+(antSoil[i]-antSoilbar[regVege[i]])
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
	###antecedent            ###
	###with mixing tricks    ###
	############################
	#antecedent model for past soil temperature
	for(j in 1:NregVege){
		for(m in 1:Nlag){
			deltaT[m,j]~dgamma(1,1)
			wT[m,j]<-deltaT[m,j]/sumT[j]

			
		}
	}
	#calculate sums of unweighted delta values


	for(j in 1:NregVege){
		sumT[j]<-sum(deltaT[,j])


	}	
	
	#calculate mean on unnormalized scale
	for(j in 1:NregVege){
	
		antSoilbar[j] <- meanSoilT[regVegeA[j]]*sumT[j]
	
	}
	
	
	#calculate weighted temperature for each year in the past in the past
	for(i in 1:Nobs){
		for(m in 1:Nlag){

			T.temp[i,m]<-deltaT[m,regVege[i]]*a.T[i,m]
		}

	}	
	

	#final antecedent calculations for soil values

	for(i in 1:Nobs){
		antSoil[i]<-sum(T.temp[i,])

	}
	#calculate identifiable regression estimator
	for(j in 1:NregVege){
		beta4star[j] <- beta4[j]*sumT[j]
	}
	
	############################
	###priors                ###
	############################
	for(j in 1:NregVege){
	beta0[j] ~ dnorm(0,.0001)
	beta1[j] ~ dnorm(0,.0001)
	beta2[j] ~ dnorm(0,.0001)
	beta3[j] ~ dnorm(0,.0001)
	beta4[j] ~ dnorm(0,.0001)
	sigSoilV[j] ~ dunif(0,100)
	}


}