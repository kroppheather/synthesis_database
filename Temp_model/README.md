# Temperature Model
This code implements a Bayesian model on air and soil temperature based on established sinusoidal variation across years. This model describes annual temperature patterns, calculates n-factors, and can account for sites with 25% or less of the year with missing temperature observations. 

## Soil temperature model description

##### This model describes shallow soil temperature (< 20 cm depth) in Arctic regions. The soil temperature model describes daily mean soil temperature throughout a water year. Soil temperature is consdiered to follow a sinusoidal pattern throughout the year. The model accounts for the zero curtain phenomenon that results from latent heat effects on temperature during liquid-solid water phase changes in the soil.  The sinusoidal pattern is paramaterized such that a symmetrical timing and magnitude of soil temperature minima and maxima that arises from freeze/thaw dynamics.


_Tsoil(day) ~ Normal(mu, tau)_

_mu= (1-X) mu.sine + X mu.zero_

_X ~ bernouli(p.zero)_

_p.zero = a exp(-b abs(Tsoil(day-1) - 0))_

_mu.zero ~ Normal(0, .025)_

_Tsoil(day) =_
- _Ta - (Ta-Tmin) sin(2 pi pTime)_ if _Time <= Time.min_
- _Ta - (Ta-Tmin) sin(2 pi pTime)_ if _Time.min > Time < Time.max_
- _Ta+1 - (Tmax - Ta+1) sin(2 pi pTime)_ if _Time >= Time.max_

_pTime =_
- _Time* (0.25/Time.min)_ if _Time<=Time.min_
- _0.25 + (.5/Time.max-Time.min) (Time - Time.min)_ if _Time.min > Time < Time.max_
-  _0.75 + (.25/1-Time.max) (Time - Time.min)_

Here _X_ represents whether a daily temperature observation falls into the zero curtian (_X =1_) or sinisoudal model (_X=0_). The probability that the temperature is in the zero curtain (_p.zero_) is considered to vary non-linearly depending on how close the previous day's temperature was to zero. The sinisuodal description of temperature variation across the year is constrained to allow for asymetrical amplitudes between the the minimum and maximum temperatures that also considers the sinusoidal periods to be completely contained within the water year. Thus _pTime_ represents the sinusoidal normalized time and  _Time_ is the proportion into a water year ranging from 1991 to 2016. _Ta_ is the temperature at the inflection points and _Tmax_ is the maximum temperature and _Tmin_ is the minimum temperature occuring in the year. _Time.min_ is the proportion in the water year that the minimum temperature occurs, and _Time.max_ is the proportoin into the water year that the maximum temperature occurs. Each parameter is estimated for site and depth. 


## Air temperature model description

##### This model describes shallow air temperaturein Arctic regions, and has similar parameterizatoin to the above soil temperature model. However, the zero curtain model is not necessary since air temperature is not subject to the phase change dynamics of liquid water. The asymetrical parameterization is maintained for consistency and the parameterization also allows for symetrical sinisudoial patterns more consistent with air temperature dynamics.

_Tair(day) ~ Normal(mu.air(day), tau)_

_mu.air(day) =_
- _Ta - (Ta-Tmin) sin(2 pi pTime)_ if _Time <= Time.min_
- _Ta - (Ta-Tmin) sin(2 pi pTime)_ if _Time.min > Time < Time.max_
- _Ta+1 - (Tmax - Ta+1) sin(2 pi pTime)_ if _Time >= Time.max_

_pTime =_
- _Time* (0.25/Time.min)_ if _Time<=Time.min_
- _0.25 + (.5/Time.max-Time.min) (Time - Time.min)_ if _Time.min > Time < Time.max_
-  _0.75 + (.25/1-Time.max) (Time - Time.min)_


The sinisuodal description of air temperature variation across the year is similar to the above soil temperature sinusoidal model, but all parameters are estimated seperately for air temperature. Thus _pTime_ represents the sinusoidal normalized time and  _Time_ is the proportion into a water year ranging from 1991 to 2016. _Ta_ is the temperature at the inflection points and _Tmax_ is the maximum temperature and _Tmin_ is the minimum temperature occuring in the year. _Time.min_ is the proportion in the water year that the minimum temperature occurs, and _Time.max_ is the proportoin into the water year that the maximum temperature occurs. Each parameter is estimated for site and height. 


