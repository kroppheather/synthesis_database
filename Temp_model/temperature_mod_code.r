model{
	#likelihood for air temperature observations
	for(i in 1:NobsA){
	TempA[i]~dnorm(muA[i], tau.muA)
	muA[i]<-T.ave[site.depthid[i]]+ Amp[site.depthid[i]]*
	
	}



}