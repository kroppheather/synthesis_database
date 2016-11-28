# N factor Model
This code is for a Bayesian analysis of N factor availabilty across summer and winter growing seasons in shallow soil layers (0-10 cm). 

## Version 1:

_N.factor_ ~Normal( _$$\mu$$N_ , $$\tau$$ )

_$$\mu$$N_= $$\beta_1$$+$$\beta_2$$*_Sens.dist_ + $$\alpha$$ + $$\epsilon$$
 
 where $$\alpha$$ is a site level random effect with a Gaussian spatial distribution accounting for the coordinates of the site in the covariance matrix and $$\epsilon$$ is the year random effect that also accounts for temporal correlation in the covariance matrix. _Sens.dist_ is the distance (in m) between the air and ground temperature sensors.

###### _This model version examines how much variability is simply explained by site location (via random effect), year (random effect), and if distance between air and temperature sensors can be accounted for in a linear relationship. This model is meant to help explore variability in N factors and examine how much variability might be explained by site specific factors vs arctic wide temporal variables._  
_Commits: 66c2639-present_ 
