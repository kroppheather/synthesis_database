# Temperature Model
This code implements a Bayesian model on air and soil temperature based on established sinusoidal variation across years.

## Version 1

##### The first version fits the simplest possible model. It assumes that temperature observations for each depth (soil) and height (air) vary with an established sine function. It assumes all parameters are independent and thus does not account for the relationship between amplitude and depth in a given site. It also assumes that the min and max occur between the half of the water year.

_Temp ~ Normal(mu, tau)_
_mu= Tave + A * sin( 2 * pi * Time)_

Here _Time_ is the proportion into a water year ranging from 1991 to 2016. _Tave_ is the temperature at the inflection points and _A_ is the amplitude. Each parameter is estimated for site and depth. 
