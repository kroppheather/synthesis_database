##########################################################
########Soil patterns                          ###########
########Heather Kropp started April 2018       ###########
##########################################################

model{

	for(i in 1:Nobs){
		#likelihood
		yvar[i] ~ dnorm(mu.yvar[i], tau.yvar[i])
		mu.yvar[i] <- beta0[compVege[i]]+ beta1[compVege[i]]*(mu.xvar[i]-xvarCenter[compX[i]])
		#replicated data
		rep.yvar[i] ~ dnorm(mu.yvar[i], tau.yvar[compVege[i]])
		#variance model includes variation from group mean and standard deviation from model runs
		tau.yvar[i] <- pow(sig.yvar[i],-2)
		sig.yvar[i] <- sig.mod[i] + sig.compVege[compVege[i]]
		mu.xvar[i] ~ dnorm(xvar[i], tau.xvar[i])
		tau.xvar[i] <- pow(sig.xvar[i],-2)
		
	}
	#monitor regression line for plotting
	for(i in 1:Nplot){
	
			mu.plot[i] <- beta0[compVegeP[i]]+ beta1[compVegeP[i]]*(xplot[i]-xvarCenter[compP[i]])

	}
	for(i in 1:Nhhplot){
	
			mu.hhplot[i] <- mu.beta0[comphh[i]]+ mu.beta1[comphh[i]]*(xplothh[i]-xvarCenter[comphh[i]])

	}
		
	
	#hierarchical priors for regression coefficients
	for(i in 1:NcompVege){
		beta0[i]~dnorm(mu.beta0[comp[i]], tau.beta0[comp[i]])
		beta1[i]~dnorm(mu.beta1[comp[i]], tau.beta1[comp[i]])
		#variance for observations from mean
		sig.compVege[i] ~ dunif(0,10000)
	}
	#hyper priors for regression coefficients
	for(i in 1:Ncomp){
		mu.beta0[i] ~dnorm(0,.00001)
		mu.beta1[i] ~dnorm(0,.00001)
		tau.beta0[i] <- pow(sig.beta0[i],-2)
		sig.beta0[i] ~dunif(0,1000)
		tau.beta1[i] <- pow(sig.beta1[i],-2)
		sig.beta1[i] ~dunif(0,1000)	
	}
}