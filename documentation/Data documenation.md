# Synthesis Database Notes

This file describes protocols and data for the vegetation and permafrost data 
synthesis. All incoming data must have paired air and vegetation data with 
data pertaining to vegetation around the measurements. Below is a description of
each data table and any relevant data protocols related to them. A graphic representation of the database schema is included in the database structure.jpg file.

## contributor
Information pertaining to contributor contact information
| Column | Description |
| ------ | ----------- |
|ctbr_id | Primary key |
|ctbr_name| Contributor's name |
|institution | Contributor's institution |
|email | Contributor's email |

## siteinfo
Information pertaining to site location, description, and data date range. Data is considered to occur at a site when there is unique soil temperature and vegetation data that can occur at it, and thus sites may be in close proximity to each other (meters away) as long as they each have a distinct data set. 
| Column | Description |
| ------ | ----------- |
|site_id | Primary key |
|site_name| location name|
|lat | Latitude |
|long | Longitude |
|MAAT | Mean annual temperature in degrees C |
|Dist_type | Disturbance description if applicable|
| Vege_z | Vegetation zone: boreal or tundra |
|PF_Zone | Permafrost zone: dc=discontinuous, c=continuous,  s=sporadic|
|ctbr_id| Foreign key from contributor|
|elev| Site elevation in m |
|year_beg| Year that temperature data begins |
|year_end | Year that temperature data ends |
|doy_beg | Day of year that temperature data begins |
|doy_end | Day of year that temperature data ends|
|dist_hist | Years since disturbance occurred |

## soil
This table describes any relevant soil information.
| Column | Description |
| ------ | ----------- |
|soil_id | Primary key |
|soil_class | Name of the soil classification type |
|soil_drain| Indication of soil drainage (e.g. poorly drained)|
|sand | Percent of sand in mineral soil |
|clay | Percent of clay in mineral soil |
|silt | Percent of silt in mineral soil |
|organic_thick| Thickness of the organic layer in cm |
|site_id | Foreign key relating to siteinfo |

## soilc
Table contains soil carbon content.
| Column | Description |
| ------ | ----------- |
|soilc_id | Primary key |
|soilc | Soil carbon content in g m^-2| 
|soilc_dep| Depth in cm that soil C is measured to|
|site_id | Foreign key relating to siteinfo |
|c_year | Year measured if recoreded |
|c_doy | Day of year measured if recorded |

## root_bio
Table contains information on root biomass.
| Column | Description |
| ------ | ----------- |
|root_id | Primary key |
|mean_root | Mean root biomass in g m^-2 |
|root_depth| Depth in cm that the soil that root biomass was measured to|
|doy_r| Day of year that roots were measured |
|year_r| Year that roots were measured |
|site_id | Foreign key relating to siteinfo |
|root_unit| Indicates units if root units could not be converted |

## moss
This table has data on the moss thickness and identification
| Column | Description |
| ------ | ----------- |
|moss_id | Primary key |
|mlt| Moss layer thickness in cm |
|moss_name| Name of moss if included |
|doy_m | Day of year that the moss layer was measured |
|year_m | Year that the moss layer was measured |
|site_id| Foreign key relating to siteinfo |

## aboveground_bio
This table contains the mean aboveground biomass for all plants at the site level.
| Column | Description |
| ------ | ----------- |
|bio_id | Primary key |
|ABG_mean | Aboveground biomass in g m^-2|
|year_a | Year that biomass was measured |
|site_info | Foreign key relating to siteinfo |

## species_bio
This contains biomass measurements specific to species, functional type, or under/overstory (listed as such under sp_funcT)
| Column | Description |
| ------ | ----------- |
|spbio_id | Primary key |
|sp_bio | Species biomass in g m^-2 |
|sp_name | Species name |
|sp_funcT | Functional type of species |
|doy_sp | Day of year of measurements |
|year_sp | Year of measurements |
|site_id | Foreign key relating to siteinfo |
|sp_unit | Indicates if units differ and couldn't be converted |

## LAI
Table contains leaf area index and/or NDVI measurements of the for the site. Some measurements had species or functional type listed associated with the measurements and other simply gave the measurements for the site. 
| Column | Description |
| ------ | ----------- |
|LAI_id | Primary Key |
|LAI | Leaf area index in m^2 of leaf m^-2 of ground|
|NDVI | NDVI measurement |
|species | Species name |
|funct_Type | Functional type |
|DOY_m | Day of year of measurement |
|Year_m | Year of measurement |
|site_id | Foreign key relating to site info |

## spec_list 
Table has a list of species that was provided for the site. 
| Column | Description |
| ------ | ----------- |
|specl_id | Primary key |
|spec_name | Species name |
|site_id | Foreign key relating to siteinfo |
|spl_funct | Functional type of species |

## soil_moist 
Table contains soil moisture measurements averaged over daily timescales. Some data may contain NaN and other site data may just have irregular day increments due to missing data. If data was aggregated from sub-daily increments provided by contributors, no gap-filling measures were taken if data was missing at any time point. Days will be NaN if any missing data occurred during them.
| Column | Description |
| ------ | ----------- |
|sm_id | Primary key |
|doy_sm | Day of year of measurement |
|year_sm | Year of measurement |
|SWC | Soil water content |
|sw_depth | Depth or depth midpoint of measurement in cm |
|sm_type | v=volumetric measurement and g=gravimetric measurement|
|site_id | Foreign key relating to siteinfo |

## soil_temp
Table containing soil temperature measurements. Some data may contain NaN and other site data may just have irregular day increments due to missing data. If data was aggregated from sub-daily increments provided by contributors, no gap-filling measures were taken if data was missing at any time point. Days will be NaN if any missing data occurred during them.
| Column | Description |
| ------ | ----------- |
|stemp_id | Primary key |
|doy_st | Day of year of measurement |
|year_st | Year of measurement |
|soil_t | Soil temperature in degrees C| 
|st_depth | Depth in the soil of measurement in cm |
|site_id | Foreign key relating to siteinfo |

## air_temp
Table containing air temperature measurements. Some data may contain NaN and other site data may just have irregular day increments due to missing data. If data was aggregated from sub-daily increments provided by contributors, no gap-filling measures were taken if data was missing at any time point. Days will be NaN if any missing data occurred during them.
| Column | Description |
| ------ | ----------- |
|air_id | Primary key |
|doy_ai | Day of year of measurement |
|year_ai| Year of measurement |
|air_t | Air temperature in degrees C |
|air_height | Height taken at in meters |
|site_id | Foreign key realting to siteinfo |
