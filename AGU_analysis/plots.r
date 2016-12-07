###################################################################
#########plot the results from the Nvege model for AGU  ###########
###################################################################

#read in datafiles
#quantiles
datQ<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_nvege_quant.csv")
#means
datS<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\model_nvege_stats.csv")
#data
datSN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\organized_vegesN_for_model.csv")
datWN<-read.csv("c:\\Users\\hkropp\\Google Drive\\raw_data\\analysis_u6\\organized_vegewN_for_model.csv")

dexps<-"\\[*[[:digit:]]*\\]"
datS$parms<-gsub(dexps,"",datS$X)

datA<-data.frame(parms=datS$parms, means=datS$Mean,ci2.5=datQ[,2], ci97.5=datQ[,6])

#get a list of parameter names
parmS<-unique(datA$parms)



#look at model fit
plot(datWN$n, datA$means[datA$parms=="rep.nW"], pch=19 )
abline(0,1)
fitwn<-lm(datA$means[datA$parms=="rep.nW"]~datWN$n)
summary(fitwn)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.125126   0.009876   12.67   <2e-16 ***
#datWN$n     0.721801   0.020013   36.07   <2e-16 ***

#Residual standard error: 0.08717 on 462 degrees of freedom
#Multiple R-squared:  0.7379,    Adjusted R-squared:  0.7374 
plot(datSN$n, datA$means[datA$parms=="rep.nS"], pch=19 )
abline(0,1)
fitsn<-lm(datA$means[datA$parms=="rep.nS"]~datSN$n)
summary(fitsn)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.51530    0.02382   21.64   <2e-16 ***
#datSN$n      0.45373    0.02466   18.40   <2e-16 ***

#Residual standard error: 0.1009 on 388 degrees of freedom
#Multiple R-squared:  0.4659,    Adjusted R-squared:  0.4645 

#now look at regression parameters
#make a panel for the regressions
#where the winter and summer
#parameters are shown for each region in 
#a panel

#each season parameter gets a plot

a<-layout(matrix(c(1,2), ncol=2), widths=c(lcm(15),lcm(15)),heights=c(lcm(15),lcm(15)))
layout.show(a)

#now set up sequence for bars
xW<-c(1,3.5,6)
xS<-c(1,3.5,6)
xu<-7.5
xl<--.5
yuE<-1
ylE<--12

#regions are as follows
#1=interior AK
#2= western CA
#3= Greenland + svalbard
layout.show(a)
#plot slope EVI 
#winter
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylE,yuE), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)
for(i in 1:3){
	polygon(c(xW[i]-1,xW[i]-1,xW[i]+1,xW[i]+1), 
		c(0,datA$means[datA$parms=="nbeta2W"][i],datA$means[datA$parms=="nbeta2W"][i],0),
		col="deepskyblue4")
		}
		
arrows(xW,datA$ci2.5[datA$parms=="nbeta2W"],xW,datA$ci97.5[datA$parms=="nbeta2W"],code=0)		
axis(2,seq(-12,10, by=2), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Winter EVI slope parameter", side=2, line=5, cex=2)
#summer

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylE,yuE), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)		
for(i in 1:3){		
	polygon(c(xS[i]-1,xS[i]-1,xS[i]+1,xS[i]+1), 
		c(0,datA$means[datA$parms=="nbeta2S"][i],datA$means[datA$parms=="nbeta2S"][i],0),
		col="mediumseagreen")	
		
}

arrows(xS,datA$ci2.5[datA$parms=="nbeta2S"],xS,datA$ci97.5[datA$parms=="nbeta2S"],code=0)		


axis(4,seq(-12,10, by=2), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Summer EVI slope parameter", side=4, line=5, cex=2)




#plot slope OLT
ylO<--.1
yuO<-.1
layout.show(a)
#winter
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylO,yuO), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)
for(i in 1:3){
	polygon(c(xW[i]-1,xW[i]-1,xW[i]+1,xW[i]+1), 
		c(0,datA$means[datA$parms=="nbeta3W"][i],datA$means[datA$parms=="nbeta3W"][i],0),
		col="deepskyblue4")
		}
		
arrows(xW,datA$ci2.5[datA$parms=="nbeta3W"],xW,datA$ci97.5[datA$parms=="nbeta3W"],code=0)		
axis(2,seq(-.1,.1, by=.01), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Winter organic layer slope parameter", side=2, line=5, cex=2)
#summer

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylO,yuO), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)		
for(i in 1:3){		
	polygon(c(xS[i]-1,xS[i]-1,xS[i]+1,xS[i]+1), 
		c(0,datA$means[datA$parms=="nbeta3S"][i],datA$means[datA$parms=="nbeta3S"][i],0),
		col="mediumseagreen")	
		
}

arrows(xS,datA$ci2.5[datA$parms=="nbeta3S"],xS,datA$ci97.5[datA$parms=="nbeta3S"],code=0)		


axis(4,seq(-.1,.1, by=.01), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Summer organic layer slope parameter", side=4, line=5, cex=2)

#plot slope Shrub
ylS<--.1
yuS<-.1
layout.show(a)
#winter
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylS,yuS), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)
for(i in 1:3){
	polygon(c(xW[i]-1,xW[i]-1,xW[i]+1,xW[i]+1), 
		c(0,datA$means[datA$parms=="nbeta4W"][i],datA$means[datA$parms=="nbeta4W"][i],0),
		col="deepskyblue4")
		}
		
arrows(xW,datA$ci2.5[datA$parms=="nbeta4W"],xW,datA$ci97.5[datA$parms=="nbeta4W"],code=0)		
axis(2,seq(-.1,.1, by=.01), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Winter % shrub cover slope parameter", side=2, line=5, cex=2)
#summer

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylS,yuS), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)		
for(i in 1:3){		
	polygon(c(xS[i]-1,xS[i]-1,xS[i]+1,xS[i]+1), 
		c(0,datA$means[datA$parms=="nbeta4S"][i],datA$means[datA$parms=="nbeta4S"][i],0),
		col="mediumseagreen")	
		
}

arrows(xS,datA$ci2.5[datA$parms=="nbeta4S"],xS,datA$ci97.5[datA$parms=="nbeta4S"],code=0)		


axis(4,seq(-.1,.1, by=.01), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Summer % shrub cover slope parameter", side=4, line=5, cex=2)

#plot slope moss
ylM<--.05
yuM<-.05
layout.show(a)
#winter
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylM,yuM), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)
for(i in 1:3){
	polygon(c(xW[i]-1,xW[i]-1,xW[i]+1,xW[i]+1), 
		c(0,datA$means[datA$parms=="nbeta5W"][i],datA$means[datA$parms=="nbeta5W"][i],0),
		col="deepskyblue4")
		}
		
arrows(xW,datA$ci2.5[datA$parms=="nbeta5W"],xW,datA$ci97.5[datA$parms=="nbeta5W"],code=0)		
axis(2,seq(-.1,.1, by=.01), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Winter % moss cover slope parameter", side=2, line=5, cex=2)
#summer

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu),ylim=c(ylM,yuM), xlab=" ", ylab=" ", xaxs="i", yaxs="i",
		axes=FALSE)
abline(h=0)		
for(i in 1:3){		
	polygon(c(xS[i]-1,xS[i]-1,xS[i]+1,xS[i]+1), 
		c(0,datA$means[datA$parms=="nbeta5S"][i],datA$means[datA$parms=="nbeta5S"][i],0),
		col="mediumseagreen")	
		
}

arrows(xS,datA$ci2.5[datA$parms=="nbeta5S"],xS,datA$ci97.5[datA$parms=="nbeta5S"],code=0)		


axis(4,seq(-.1,.1, by=.01), cex.axis=1.5,las=2)
axis(1, xW, c("Interior AK","Western Canada", "Greenland"), cex.axis=1.5)
box(which="plot")
mtext("Summer % moss cover slope parameter", side=4, line=5, cex=2)


###############################################################################
###############################################################################
#make a plot for year random effects in the model
b<-layout(matrix(c(1,2),ncol=1), widths=c(lcm(33),lcm(33)), height=c(lcm(8), lcm(8)))
layout.show(b)

Syear<-data.frame(yearID=seq(1,26), year=seq(1991,2016))
Wyear<-Syear
#plot the random effect for year
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,27),ylim=c(-.15,.15), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Syear)[1]){
	polygon(c(Syear$yearID[i]-.5,Syear$yearID[i]-.5,Syear$yearID[i]+.5,Syear$yearID[i]+.5), 
			c(0,datA$means[datA$parms=="eps.star"][i],datA$means[datA$parms=="eps.star"][i],0),
			col="mediumseagreen")
}
arrows(Syear$yearID,datA$ci2.5[datA$parms=="eps.star"],Syear$yearID,datA$ci97.5[datA$parms=="eps.star"],code=0)
box(which="plot")
axis(2, seq(-.12,.12,by=.04),las=2,cex.axis=1.25)
mtext("Random effect", side=2,cex=2,line=5)
text(3,-.13, "Summer", cex=3)
#now for winter
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,27),ylim=c(-.25,.25), axes=FALSE, xlab=" ", ylab=" ", xaxs="i",
	yaxs="i")
for(i in 1:dim(Wyear)[1]){
	polygon(c(Wyear$yearID[i]-.5,Wyear$yearID[i]-.5,Wyear$yearID[i]+.5,Wyear$yearID[i]+.5), 
			c(0,datA$means[datA$parms=="epsW.star"][i],datA$means[datA$parms=="epsW.star"][i],0),
			col="deepskyblue4")
}
arrows(Wyear$yearID,datA$ci2.5[datA$parms=="epsW.star"],Wyear$yearID,datA$ci97.5[datA$parms=="epsW.star"],code=0)
axis(1, Wyear$yearID, Wyear$year, cex.axis=1.25)
axis(2, seq(-.2,.2,by=.05),las=2,cex.axis=1.25)
box(which="plot")
mtext("Year", side=1,cex=2,line=3)
mtext("Random effect", side=2,cex=2,line=5)
text(3,-.21, "Winter", cex=3)


