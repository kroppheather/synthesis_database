model{
#likelihood for air temperature observations
	for(i in 1:NobsA){
		TempA[i]~dnorm(muA[i], tau.muA)
		Tstep1[i]<-ifelse(T.yrA[i]<=peakWA[SDWA[i]],1,0)
		Tstep2[i]<-ifelse(T.yrA[i]>peakWA[SDWA[i]],ifelse(T.yrA[i]<peakSA[SDWA[i]],1,0),0)
		Tstep3[i]<-ifelse(T.yrA[i]>=peakSA[SDWA[i]],1,0)
		muA[i]<-(Tstep1[i]*(T.aveA1[SDWA[i]]-((T.aveA1[SDWA[i]]-TminA[SDWA[i]])*sin(2*3.14159265*((.25/peakWA[SDWA[i]])*T.yrA[i])))))+
				(Tstep2[i]*((TminA[SDWA[i]]+((TmaxA[SDWA[i]]-TminA[SDWA[i]])/2))-
					(((TmaxA[SDWA[i]]-TminA[SDWA[i]])/2)*sin(2*3.14159265*((.5/(peakSA[SDWA[i]]-peakWA[SDWA[i]]))*(T.yrA[i]-peakWA[SDWA[i]])+.25)))))+
				(Tstep3[i]*(T.aveA2[SDWA[i]]-((TmaxA[SDWA[i]]-T.aveA2[SDWA[i]])*sin(2*3.14159265*((.25/(1-peakSA[SDWA[i]]))*(T.yrA[i]-peakSA[SDWA[i]])+.75)))))
		#cacluation for freezing degree day
		#set to zero if not freezing
		#FreezeA[i]<-(1-step(TempA[i]))*TempA[i]
		#cacluation for thawing degree day
		#set to zero if not above zero
		#ThawA[i]<-step(TempA[i])*TempA[i]		
	}
	
	#likelihood for soil temperature observations
	for(i in 2:NobsS){
		TempS[i]~dnorm(muS[i], tau.muS)
		TstepS1[i]<-ifelse(T.yrS[i]<=peakWS[SDWS[i]],1,0)
		TstepS2[i]<-ifelse(T.yrS[i]>peakWS[SDWS[i]],ifelse(T.yrS[i]<peakSS[SDWS[i]],1,0),0)
		TstepS3[i]<-ifelse(T.yrS[i]>=peakSS[SDWS[i]],1,0)
		muS[i]<-X[i]*zeroC+ ((1-X[i])*sineS[i])
		sineS[i]<-(TstepS1[i]*(T.aveS1[SDWS[i]]-((T.aveS1[SDWS[i]]-TminS[SDWS[i]])*sin(2*3.14159265*((.25/peakWS[SDWS[i]])*T.yrS[i])))))+
				(TstepS2[i]*((TminS[SDWS[i]]+((TmaxS[SDWS[i]]-TminS[SDWS[i]])/2))-(((TmaxS[SDWS[i]]-TminS[SDWS[i]])/2)*sin(2*3.14159265*((.5/(peakSS[SDWS[i]]-peakWS[SDWS[i]]))*(T.yrS[i]-peakWS[SDWS[i]])+.25)))))+
				(TstepS3[i]*(T.aveS2[SDWS[i]]-((TmaxS[SDWS[i]]-T.aveS2[SDWS[i]])*sin(2*3.14159265*((.25/(1-peakSS[SDWS[i]]))*(T.yrS[i]-peakSS[SDWS[i]])+.75)))))
				
		X[i]~dbern(p[i])
		p[i]<-aZero[SDWS[i]]*exp(-bZero[SDWS[i]]*abs(TempS[i-1]-0))
		
		#cacluation for freezing degree day
		#set to zero if not freezing
		#FreezeS[i]<-(1-step(TempS[i]))*TempS[i]
		#cacluation for thawing degree day
		#set to zero if not above zero		
		#ThawS[i]<-step(TempS[i])*TempS[i]
	}
		
		TempS[1]~dnorm(muS[1], tau.muS)
		TstepS1[1]<-ifelse(T.yrS[1]<=peakWS[SDWS[1]],1,0)
		TstepS2[1]<-ifelse(T.yrS[1]>peakWS[SDWS[1]],ifelse(T.yrS[1]<peakSS[SDWS[1]],1,0),0)
		TstepS3[1]<-ifelse(T.yrS[1]>=peakSS[SDWS[1]],1,0)
		muS[1]<-X[1]*zeroC+ ((1-X[1])*sineS[1])
		sineS[1]<-(TstepS1[1]*(T.aveS1[SDWS[1]]-((T.aveS1[SDWS[1]]-TminS[SDWS[1]])*sin(2*3.14159265*((.25/peakWS[SDWS[1]])*T.yrS[1])))))+
				(TstepS2[1]*((TminS[SDWS[1]]+((TmaxS[SDWS[1]]-TminS[SDWS[1]])/2))-(((TmaxS[SDWS[1]]-TminS[SDWS[1]])/2)*sin(2*3.14159265*((.5/(peakSS[SDWS[1]]-peakWS[SDWS[1]]))*(T.yrS[1]-peakWS[SDWS[1]])+.25)))))+
				(TstepS3[1]*(T.aveS2[SDWS[1]]-((TmaxS[SDWS[1]]-T.aveS2[SDWS[1]])*sin(2*3.14159265*((.25/(1-peakSS[SDWS[1]]))*(T.yrS[1]-peakSS[SDWS[1]])+.75)))))
				
		X[1]~dbern(p[1])
		p[1]~dunif(0,1)
					
				
				
	#look at a subset of replicated data since there is too much temp data to monitor
	#for(i in 1:NrepS){
	#	TempS.rep[i]~dnorm(muS[SrepSub[i]], tau.muS)
	
	#}
	
	#for(i in 1:NrepA){
	#	TempA.rep[i]~dnorm(muA[ArepSub[i]], tau.muA)
	
	#}
	
	#prior for likelihood
	for(i in 1:NSDWA){
		T.aveA1[i]~dnorm(-10,10)
		T.aveA2[i]~dnorm(-10,10)
		TminA[i]~dunif(-60,0)
		TmaxA[i]~dunif(0,35)
		peakWA[i]~dunif(0.1,.5)
		peakSA[i]~dunif(.55,.9)
	}
	
	#prior for likelihood
	for(i in 1:NSDWS){
		T.aveS1[i]~dnorm(-10,10)
		T.aveS2[i]~dnorm(-10,10)
		TmaxS[i]~dunif(0,35)	
		TminS[i]~dunif(-60,0)
		aZero[i]~dunif(0,1)
		bZero[i]~dunif(1,10)
		peakWS[i]~dunif(0.1,.59)
		peakSS[i]~dunif(.6,.95)
	
	}
	
	
	#mean for a zero curtain
	
	zeroC~dnorm(0,16)
	#now need to calculate the predicted temperature for all air observations
	#this is going to vary by site, wyear, and depth
	#get the predicted temperature
	
	#add up thawing and freezing degree days
	#for(i in 1:NSDWA){
	#	FDDA[i]<-sum(FreezeA[ASY[i]:AEY[i]])
	#	TDDA[i]<-sum(ThawA[ASY[i]:AEY[i]])
	#}		
	#for(i in 1:NSDWS){
	#	FDDS[i]<-sum(FreezeS[SSY[i]:SEY[i]])
	#	TDDS[i]<-sum(ThawS[SSY[i]:SEY[i]])
	#}	
	
	#now calculate N factors
	#for(i in 1:Ncombo){
	#	Fn[i]<-FDDS[SoilIND[i]]/FDDA[AirIND[i]]
	#	Tn[i]<-TDDS[SoilIND[i]]/TDDA[AirIND[i]]
	#}
	
	#prior for variance term
	tau.muA<-pow(sig.muA,-2)
	sig.muA~dunif(0,100)
	#prior for variance term
	tau.muS<-pow(sig.muS,-2)
	sig.muS~dunif(0,100)
}