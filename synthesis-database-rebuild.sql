

--set up the synthesis database for daily data
CREATE DATABASE synth2 WITH OWNER hkropp;

--connect to database
\c synth2

--create the contributor table
CREATE TABLE contributor (ctbr_id INTEGER PRIMARY KEY, ctbr_name text, institution text,email text);



--create site table
CREATE TABLE siteinfo (site_id INTEGER PRIMARY KEY, site_name TEXT, lat NUMERIC, lon NUMERIC, loc TEXT,
MAAT numeric, Dist_type TEXT, Vege_z TEXT, PF_Zone TEXT, ctbr_id INTEGER, elev NUMERIC, year_beg NUMERIC, year_end NUMERIC,day_beg NUMERIC,day_end NUMERIC, dist_hist NUMERIC );


--alter to make ctbr_id a forgien key
ALTER TABLE siteinfo ADD CONSTRAINT ctbr_id FOREIGN KEY (ctbr_id) REFERENCES contributor;

--Make a table for the reference info
CREATE TABLE refinfo (ref_id INTEGER PRIMARY KEY, authors TEXT, title TEXT, journal TEXT, dateid TEXT, pages text,
	ctbr_id INTEGER REFERENCES contributor (ctbr_id));
	
--Make the table that links a reference to a site 
CREATE TABLE refrel (refrel_id INTEGER PRIMARY KEY, site_id INTEGER REFERENCES siteinfo (site_id), ref_id INTEGER REFERENCES refinfo (ref_id));

--Make the table with soil properties
CREATE TABLE soil (soil_id INTEGER PRIMARY KEY, soil_class TEXT, soil_drain TEXT, sand NUMERIC, clay NUMERIC, silt NUMERIC,
	organic_thick NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));
	
--create table with species cover
CREATE TABLE spcov (cover_id INTEGER PRIMARY KEY, perc_cover NUMERIC, species TEXT, func_type TEXT, time_info TEXT, site_id INTEGER REFERENCES siteinfo (site_id));
ALTER TABLE spcov DROP COLUMN time_info; 

ALTER TABLE spcov ADD COLUMN year_m NUMERIC;

ALTER TABLE spcov ADD COLUMN doy_m NUMERIC;

--Create Soil carbon table for soil carbon in units g/m2
CREATE TABLE soilc (soilc_id INTEGER PRIMARY KEY, soilc NUMERIC, soilc_dep NUMERIC, c_time TEXT, site_id INTEGER REFERENCES siteinfo (site_id));
	
ALTER TABLE soilc DROP COLUMN c_time;

ALTER TABLE soilc ADD COLUMN c_year NUMERIC;
ALTER TABLE soilc ADD COLUMN c_doy NUMERIC;
	
--Create snow table
CREATE TABLE snow (snow_id INTEGER PRIMARY KEY, snowD NUMERIC, doy_s NUMERIC, year_s NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));
	
--Create aboveground biomass table
CREATE TABLE aboveground_bio (bio_id INTEGER PRIMARY KEY, ABG_mean NUMERIC, year_a NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));

--Create species biomass table 
CREATE TABLE spec_bio (spbio_id INTEGER PRIMARY KEY, sp_bio NUMERIC, sp_name TEXT, sp_functT TEXT, year_sp NUMERIC, doy_sp NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));

ALTER TABLE spec_bio ADD COLUMN sp_unit TEXT;
--Create the root biomass table

CREATE TABLE root_bio (root_id INTEGER PRIMARY KEY, mean_root NUMERIC, root_depth NUMERIC, doy_r NUMERIC, year_r NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));


ALTER TABLE root_bio ADD COLUMN root_unit TEXT;
--create the moss table

CREATE TABLE moss (moss_id INTEGER PRIMARY KEY, mlt NUMERIC, moss_name TEXT, doy_m NUMERIC, year_m NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));

CREATE TABLE LAI (LAI_id INTEGER PRIMARY KEY, LAI NUMERIC, NDVI NUMERIC, Species TEXT, Funct_type TEXT, DOY_m NUMERIC, Year_m NUMERIC, Siteid INTEGER REFERENCES siteinfo (site_id));
--create table for species list

CREATE TABLE spec_list (specl_id INTEGER PRIMARY KEY, spec_name TEXT, site_id INTEGER REFERENCES siteinfo (site_id));
ALTER TABLE spec_list ADD COLUMN spl_funcT TEXT;

--create table for soil moisture

CREATE TABLE soil_moist (sm_id INTEGER PRIMARY KEY,doy_sm NUMERIC, year_sm NUMERIC, SWC NUMERIC, sw_depth NUMERIC, sm_type TEXT, sensor_type TEXT, site_id INTEGER REFERENCES siteinfo (site_id));

--to DO:
--
--alter table to get rid of sensor type
--


--create table for air temperature

CREATE TABLE air_temp (air_id INTEGER PRIMARY KEY, doy_ai NUMERIC, year_ai NUMERIC, air_t NUMERIC, air_height NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));

--create table for soil temperature
CREATE TABLE soil_temp (stemp_id INTEGER PRIMARY KEY, doy_st NUMERIC, year_st NUMERIC, soil_t NUMERIC, st_depth NUMERIC, site_id INTEGER REFERENCES siteinfo (site_id));


--add data
\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\contributor.csv' DELIMITER ',' CSV HEADER 
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\siteinfo.csv' DELIMITER ',' CSV HEADER
\copy refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\refinfo.csv'  DELIMITER ',' CSV HEADER 
\copy refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\refrel.csv'  DELIMITER ',' CSV HEADER 
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\soil.csv'  DELIMITER ',' CSV HEADER
\copy soilc(soilc_id,soilc,soilc_dep,site_id,c_year,c_doy) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\soilc.csv'  DELIMITER ',' CSV HEADER 
\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\moss.csv'  DELIMITER ',' CSV HEADER 
\copy aboveground_bio(bio_id,abg_mean,year_a,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\aboveground_bio.csv'  DELIMITER ',' CSV HEADER
\copy spec_bio(spbio_id,sp_bio,sp_name,sp_functt,year_sp,doy_sp,site_id,sp_unit) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\spec_bio.csv'  DELIMITER ',' CSV HEADER
\copy spcov(cover_id,perc_cover,species,func_type,site_id, year_m, doy_m)FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\spcov.csv'  DELIMITER ',' CSV HEADER
\copy LAI(LAI_id, LAI, NDVI, Species, Funct_type, DOY_m, Year_m, Siteid) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\LAI.csv'  DELIMITER ',' CSV HEADER
\copy spec_list(specl_id,spec_name,site_id,spl_funcT) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\spec_list.csv' DELIMITER ',' CSV HEADER
\copy soil_moist(sm_id,doy_sm,year_sm,SWC,sw_depth,sm_type,sensor_type,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\soil_moist.csv'  DELIMITER ',' CSV HEADER
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\soil_temp.csv'  DELIMITER ',' CSV HEADER 
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\backup_4\air_temp.csv'  DELIMITER ',' CSV HEADER 

--db upload 8

\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\contributor.csv' DELIMITER ',' CSV HEADER  QUOTE '"'
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\siteinfo.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\refinfo.csv'  DELIMITER ',' CSV HEADER  QUOTE '"'
\copy refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\refrel.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\soil.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\moss.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\soil_temp.csv'  DELIMITER ',' CSV HEADER 
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\air.csv'  DELIMITER ',' CSV HEADER 

--copy entire database dump
--now export updated tables to csv
\copy contributor TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\contributor.csv' DELIMITER ',' CSV  HEADER;
\copy siteinfo TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\siteinfo.csv' DELIMITER ',' CSV  HEADER;
\copy refinfo TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\refinfo.csv' DELIMITER ',' CSV  HEADER;
\copy refrel TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\refrel.csv' DELIMITER ',' CSV  HEADER;
\copy soil TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\soil.csv' DELIMITER ',' CSV  HEADER;
\copy soilc TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\soilc.csv' DELIMITER ',' CSV  HEADER;
\copy moss TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\moss.csv' DELIMITER ',' CSV  HEADER;
\copy aboveground_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\aboveground_bio.csv' DELIMITER ',' CSV  HEADER;
\copy spec_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\spec_bio.csv' DELIMITER ',' CSV  HEADER;
\copy spcov TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\spcov.csv' DELIMITER ',' CSV  HEADER;
\copy LAI TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\LAI.csv' DELIMITER ',' CSV  HEADER;
\copy spec_list TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\spec_list.csv' DELIMITER ',' CSV  HEADER;
\copy soil_moist TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\soil_moist.csv' DELIMITER ',' CSV  HEADER;
\copy soil_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\soil_temp.csv' DELIMITER ',' CSV  HEADER;
\copy air_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\air_temp.csv' DELIMITER ',' CSV  HEADER;
\copy root_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\root_bio.csv' DELIMITER ',' CSV  HEADER;

--upload soil organic layer files found in breen
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\breen_soil.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_5\soil.csv' DELIMITER ',' CSV  HEADER;


--db upload 9
\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\contributor.csv' DELIMITER ',' CSV HEADER  QUOTE '"'
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\siteinfo.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u8\soil.csv'  DELIMITER ',' CSV HEADER QUOTE '"'

\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\soil2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'


\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\moss.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\soil_data.csv'  DELIMITER ',' CSV HEADER 
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\air_data.csv'  DELIMITER ',' CSV HEADER 
\copy spec_list(specl_id,spec_name,site_id,spl_funcT) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u9\splist.csv' DELIMITER ',' CSV HEADER QUOTE '"'

--copy entire database dump
--now export updated tables to csv
\copy contributor TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\contributor.csv' DELIMITER ',' CSV  HEADER;
\copy siteinfo TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\siteinfo.csv' DELIMITER ',' CSV  HEADER;
\copy refinfo TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\refinfo.csv' DELIMITER ',' CSV  HEADER;
\copy refrel TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\refrel.csv' DELIMITER ',' CSV  HEADER;
\copy soil TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\soil.csv' DELIMITER ',' CSV  HEADER;
\copy soilc TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\soilc.csv' DELIMITER ',' CSV  HEADER;
\copy moss TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\moss.csv' DELIMITER ',' CSV  HEADER;
\copy aboveground_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\aboveground_bio.csv' DELIMITER ',' CSV  HEADER;
\copy spec_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\spec_bio.csv' DELIMITER ',' CSV  HEADER;
\copy spcov TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\spcov.csv' DELIMITER ',' CSV  HEADER;
\copy LAI TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\LAI.csv' DELIMITER ',' CSV  HEADER;
\copy spec_list TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\spec_list.csv' DELIMITER ',' CSV  HEADER;
\copy soil_moist TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\soil_moist.csv' DELIMITER ',' CSV  HEADER;
\copy soil_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\soil_temp.csv' DELIMITER ',' CSV  HEADER;
\copy air_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\air_temp.csv' DELIMITER ',' CSV  HEADER;
\copy root_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_6\root_bio.csv' DELIMITER ',' CSV  HEADER;