# Tdiff factor Model
This code is for a Bayesian analysis ofthe air and soil temperature difference in the summer growing seasons. 

## Version 1:

 _T.diff_~ Normal( _mu_ , _tau_)
 
 _mu_ = _beta1_ +if(SeasL<=0.5,  _beta2_* _SeasL_ + _beta3_ * _SeasL^2_)+ if(SeasL>0.5,  _beta4_* _SeasL_ + _beta5* _SeasL_) + _eps_
 
 where _SeasL_ is between zero and one and marks the proportion into the growing season for the temperature difference. The _beta_ parameters vary for each site and are nested hierarchically in region. _eps_ is a random effect for year and includes a covariance matrix to account for correlation between years. 

#### This model version examines if the temperature difference between the air and soil (_T.diff_ ) can be accounted for with a change point regression model that considers  _T.diff_ to vary across the growing season in a quadradric relationship with the relativized proportion of time into the growing season. The regression parameters vary by site with a year random effect. 
_Commits: c33c569-present_ 

_Note: this model had covergence issues for the beta parameters associated with SeasL. This model will be revisted post-AGU analysis._