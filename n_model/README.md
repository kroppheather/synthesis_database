
# N factor Model
This code is for a Bayesian analysis of N factor availabilty across summer and winter growing seasons. 

## Version 1:

 _N.fact_~ Normal( _mu_ , _tau_)
 
 _mu_ = _beta1_ +_beta2_* _Sens.dist_ + _alpha_ + _epsilon_
 
 where alpha is a site level random effect with a Gaussian spatial distribution accounting for the coordinates of the site in the covariance matrix _alpha_ and _epsilon_ is the year random effect that also accounts for temporal correlation in the covariance matrix. _Sens.dist_ is the distance (in m) between the air and ground temperature sensors.

#### _This model version examines how much variability is simply explained by site location (via random effect), year (random effect), and if distance between air and temperature sensors can be accounted for in a linear relationship. This model is meant to help explore variability in N factors and examine how much variability might be explained by site specific factors vs arctic wide temporal variables._  
_Commits: 66c2639-9545346_ 


## Version 2:

 _N.fact_~ Normal( _mu_ , _tau_)
 
 _mu_ = _beta1_ +_beta2_* _EVI_ + _alpha_ + _epsilon_
 
  where alpha is a site level random effect with a Gaussian spatial distribution accounting for the coordinates of the site in the covariance matrix _alpha_ and _epsilon_ is the year random effect that also accounts for temporal correlation in the covariance matrix. _EVI_ is the multi-year average EVI.
  
#### _Version 2 focuses only on soil temperature observations taken between 0-10cm in the soil. Until soil depths can be dealt with in a more sophisticated manner, very shallow depths are more readily compared across sites. Air temperature sensor height is currently not dealt with, and may be problematic in the winter_
  
#### _This model version examines how much variability is simply explained by a remotely sensed index of vegetation (EVI), and site location (via random effect), year (random effect). This model is meant to help explore variability in N factors and examine how much variability might be explained by a broad classification of vegetation as well as site specific factors vs arctic wide temporal variables. The maximum EVI band value over multiple years is used. The maximum EVI is expected to represent differernces in vegetation canopy structure is expected to be less subject to artifacts assoicated with growing season changes that can be in the average EVI_  
_Commits: f4e5622-3c16b76_ 

## Version 3 (Nvege model)

 _N.fact_~ Normal( _mu_ , _tau_)
 
 _mu_ = _beta1_ +_beta2_* _EVI_ + _beta3_ * _OLT_  + _beta4_ * _Shrub % cov_ + _beta5_ * _moss % cov_ + _eps_
 
 
#### _This model focuses only on n factors (for summer and winter seperately) with common vegetation data. It is a good start at looking at patterns with N factor and vegetation, but it excludes a lot of sites. A better way of incorporating vegetation is needed.  It is being used for preliminary data analysis for AGU but more sophisticated vegetation metrics are needed to incorporate more types of data._

_Commits: 7ca9ebf-present_ 