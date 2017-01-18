# Temperature Model
This code implements a Bayesian model on air and soil temperature based on established sinusoidal variation across years.

## Version 1

##### The first version fits the simplest possible model. It assumes that temperature observations for each depth (soil) and height (air) vary with an established sine function. It assumes all parameters are independent and thus does not account for the relationship between amplitude and depth in a given site. It also assumes that the min and max occur between the half of the water year.

_Temp ~ Normal(mu, tau)_

_mu= Tave + A * sin( 2 * pi * Time)_

Here _Time_ is the proportion into a water year ranging from 1991 to 2016. _Tave_ is the temperature at the inflection points and _A_ is the amplitude. Each parameter is estimated for site and depth. 

_Commits: 1618019 : fcc86c6_

## Version 2

##### The second version adds an offeset day so that max and min do not necessarily occur between the middle of the growing season. It still assumes that all parameters are independent and does not account for the relationship between depth.

_Temp ~ Normal(mu, tau)_

_mu= Tave + A * sin( 2 * pi * Time - Start)_

Here _Time_ is the proportion into a water year ranging from 1991 to 2016. _Tave_ is the temperature at the inflection points and _A_ is the amplitude. Each parameter is estimated for site and depth. _Start_ is the offset of the peaks from the start of the water year, and is given a non-informative parameter between zero and one.

_Commits: 69d394b :  790799f_

## Version 3

##### The third version includes an offeset day so that max and min do not necessarily occur between the middle of the growing season. It also accounts for the relationship between depth.

_Temp ~ Normal(mu, tau)_

_mu= Tave + A*exp(- depthF * b) * sin( 2 * pi * Time - Start - b * depthF)_

Here _Time_ is the proportion into a water year ranging from 1991 to 2016. _Tave_ is the temperature at the inflection points and _A_ is the amplitude. Each parameter is estimated for site and depth. _Start_ is the offset of the peaks from the start of the water year, and is given a non-informative parameter between zero and one. _depthF_ is the depth in cm, but is set to zero if there is only one depth. This is becasue there is not enough information in the model as is to account for the change from the surface for sites with a single depth. _b_ describes the change in amplitude across depth. 

_Commits: depthmod 93fe7ae:  present_