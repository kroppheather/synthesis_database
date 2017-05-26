#############################################
####Simplest vegetation model ###############
####with continuous vegetation###############
####measures and biome/region ###############
#############################################


#model for simple vegetation
model{
#likelihood
	for(i in 1:Nobs){
	Xobs[i]~dnorm(mu[i], tauM[i])
	#regression for depth, air extreme and vege
	#in next step airM needs error model to account for uncertainty
	#in the air parameters
	mu[i]<-b1[vegeC[i]]+b2[vegeC[i]]*depth[i]+b3[vegeC[i]]*(airM[i]-airM.bar)+b4[vegeC[i]]*(shrubC[i]-25)
			+b5[vegeC[i]]*(mossC[i]-25)
	#standard deviation model to include uncertainty from
	#temperature missing data and variation in nf for 
	#each vegetation type
	tauM[i]<-pow(sigM[i],-2)
	sigM[i]<-meas.sig[i]+sigV[vegeC[i]]
	rep.Xobs[i]~dnorm(mu[i], tauM[i])
	}
	
	#define priors for the model
	for(i in 1:Nvege){
	
		b1[i]~dnorm(0,0.0001)
		b2[i]~dnorm(0,0.0001)
		b3[i]~dnorm(0,0.0001)
		b4[i]~dnorm(0,0.0001)
		b5[i]~dnorm(0,0.0001)
		sigV[i]~dunif(0,100)
	}




}