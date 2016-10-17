

--set up the synthesis database for daily data
CREATE DATABASE synth WITH OWNER hkropp;

--connect to database
\c synth

--create the contributor table
CREATE TABLE contributor (ctbr_id INTEGER PRIMARY KEY, ctbr_name text, institution text,email text);



--create site table
CREATE TABLE siteinfo (site_id INTEGER PRIMARY KEY, site_name TEXT, lat NUMERIC, lon NUMERIC, loc TEXT,
MAAT numeric, Dist_hist TEXT, Dist_type TEXT, Vege_z TEXT, PF_Zone TEXT, ctbr_id INTEGER, date_range TEXT );

ALTER TABLE siteinfo DROP COLUMN date_range;

--add a elevation column
ALTER TABLE siteinfo ADD COLUMN elev NUMERIC;

ALTER TABLE siteinfo ADD COLUMN year_beg NUMERIC;

ALTER TABLE siteinfo ADD COLUMN year_end NUMERIC;

ALTER TABLE siteinfo ADD COLUMN day_beg NUMERIC;
ALTER TABLE siteinfo ADD COLUMN day_end NUMERIC;

ALTER TABLE siteinfo DROP COLUMN Dist_hist;

ALTER TABLE siteinfo ADD COLUMN dist_hist NUMERIC;

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

---------------------------------------------	
--populate tables with first dataset
------------------------------------------------


--contributor
INSERT INTO contributor(ctbr_id, ctbr_name,institution,email) VALUES (1, 'Britta Sannel','Stockholm University','britta.sannel@natgeo.su.se'); 
--site infor
INSERT INTO siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_hist,Dist_type,Vege_z,PF_Zone,ctbr_id,date_range,elev) VALUES
(1,'Tavvavuoma T2', 68.4621,20.90205,'Tavvavuoma Sweden',-2.1,'NA','NA','tundra','sporadic',1,'daily 2006 -2013',555);
UPDATE siteinfo SET year_beg=2006 WHERE site_id=1;
UPDATE siteinfo SET year_end=2013 WHERE site_id=1;
UPDATE siteinfo SET day_beg=1 WHERE site_id=1;
UPDATE siteinfo SET day_end=365 WHERE site_id=1;



--reference info
INSERT INTO refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) VALUES (1,'Sannel, A.B.K.,Hugelius, G., Jansson, P., Kuhry, P.',
'Permafrost warming in a subarctic peatland – which meteorological controls are most important?','Permafrost and Periglacial Processes','2015','NA',1);
--ref link table
INSERT INTO refrel(refrel_id,site_id,ref_id) VALUES (1,1,1);
--soil table
INSERT INTO soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) VALUES
(1,'histosol','NA',0,0,0,200,1);

INSERT INTO spcov(cover_id,perc_cover,species,func_type,time_info,site_id) VALUES(1, 65, 'Dicranum spp', 'moss','NA',1);
INSERT INTO spcov(cover_id,perc_cover,species,func_type,time_info,site_id) VALUES (2, 15, 'Betula nana', 'shrub','NA',1);
INSERT INTO spcov(cover_id,perc_cover,species,func_type,time_info,site_id) VALUES (3, 10, 'Cladonia sp', 'lichen','NA',1);

--soil C table
INSERT INTO soilc (soilc_id,soilc,soilc_dep,c_time,site_id) VALUES(1,129000, 200,'NA',1);

UPDATE soilc SET c_year='NaN' WHERE soilc_id=1;
UPDATE soilc SET c_doy='NaN' WHERE soilc_id=1;
UPDATE siteinfo SET dist_hist='NaN' WHERE site_id=1;

--add more contributor
--(ctbr_id, ctbr_name,institution,email)
\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\contributor_u1.csv' DELIMITER ',' CSV HEADER

UPDATE contributor SET ctbr_name='Jon O Donnell' WHERE ctbr_id=2;
UPDATE contributor SET institution='National Park Service Arctic Network' WHERE ctbr_id=2;
UPDATE contributor SET email='jaodonnell@nps.gov' WHERE ctbr_id=2;

UPDATE contributor SET ctbr_name='Elena Blanc-Betes' WHERE ctbr_id=3;
UPDATE contributor SET institution='Univ. of Illinois at Chicago' WHERE ctbr_id=3;
UPDATE contributor SET email='blanc.elena@gmail.com' WHERE ctbr_id=3;

\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\site_info.csv' DELIMITER ',' CSV HEADER QUOTE '"'

\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\soil_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'
DELETE FROM soil WHERE soil_id=2;
DELETE FROM soil WHERE soil_id=3;
DELETE FROM soil WHERE soil_id=4;
DELETE FROM soil WHERE soil_id=5;

\copy  refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\refinfo_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'

\copy  refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\refrel_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'

\copy  soilc(soilc_id,soilc,soilc_dep,site_id,c_year,c_doy) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\soilc_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'

\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id)FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\moss_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy spec_list(specl_id,spec_name,site_id,spl_funcT) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\splist_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_moist(sm_id,doy_sm,year_sm,SWC,sw_depth,sm_type,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\soil_moist_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\soilTu1.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\airT_u1.csv' DELIMITER ',' CSV HEADER QUOTE '"'

\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\contrib_u2.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\site_in_u2.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\refinfo_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\refel_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\soil_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soilc(soilc_id,soilc,soilc_dep,site_id,c_year,c_doy) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\soilc_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'

\copy spcov(cover_id,perc_cover,species,func_type,site_id, year_m, doy_m)FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\sp_cov_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'


\copy LAI(LAI_id, LAI, NDVI, Species, Funct_type, DOY_m, Year_m, Siteid) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\LAI_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'

\copy soil_moist(sm_id,doy_sm,year_sm,SWC,sw_depth,sm_type,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\soil_moist_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\soilT_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u2\airT_u2.csv'  DELIMITER ',' CSV HEADER QUOTE '"'

--upload number 3
\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\contrib_u3.csv' DELIMITER ',' CSV HEADER
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\siteinfo_u3.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\refinfo_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\refrel_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\soil_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soilc(soilc_id,soilc,soilc_dep,site_id,c_year,c_doy) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\soilc_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy root_bio(root_id,mean_root,root_depth,doy_r,year_r,site_id,root_unit) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\root_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\moss_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy aboveground_bio(bio_id,abg_mean,year_a,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\aboveground_bio_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spec_bio(spbio_id,sp_bio,sp_name,sp_functt,year_sp,doy_sp,site_id,sp_unit) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\sp_bio_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spcov(cover_id,perc_cover,species,func_type,site_id, year_m, doy_m)FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\spcov_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy LAI(LAI_id, LAI, NDVI, Species, Funct_type, DOY_m, Year_m, Siteid) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\LAI_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_moist(sm_id,doy_sm,year_sm,SWC,sw_depth,sm_type,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\soil_moist_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\soilT_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u3\airT_u3.csv'  DELIMITER ',' CSV HEADER QUOTE '"'


--upload number 4
\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\contributor_u4.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\siteinfo_u4.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\refinfo_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\refrel_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\soil_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soilc(soilc_id,soilc,soilc_dep,site_id,c_year,c_doy) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\soilc_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\moss_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy aboveground_bio(bio_id,abg_mean,year_a,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\aboveground_bio_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spec_bio(spbio_id,sp_bio,sp_name,sp_functt,year_sp,doy_sp,site_id,sp_unit) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\sp_bio_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spcov(cover_id,perc_cover,species,func_type,site_id, year_m, doy_m)FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\spcov_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy LAI(LAI_id, LAI, NDVI, Species, Funct_type, DOY_m, Year_m, Siteid) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\LAI_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spec_list(specl_id,spec_name,site_id,spl_funcT) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\spec_list_u4.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_moist(sm_id,doy_sm,year_sm,SWC,sw_depth,sm_type,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\soil_moist_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\soilT_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u4\airT_u4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'

--after upload 4 issues with air temp and soil temp data were found
--the data files were fixed and now need to be deleted and re-added to the database
--soil
DELETE FROM soil_temp;
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\soil_temp_fix_a_U4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
--air
DELETE FROM air_temp;
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\air_temp_fix_a4.csv'  DELIMITER ',' CSV HEADER QUOTE '"'

--upload number 5
\copy contributor(ctbr_id, ctbr_name,institution,email) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\contributer_u5.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy siteinfo(site_id,site_name,lat,lon,loc,MAAT,Dist_type,Vege_z,PF_Zone,ctbr_id,elev,year_beg,year_end,day_beg,day_end,dist_hist) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\siteinfo_u5.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy refinfo(ref_id,authors,title,journal,dateid,pages,ctbr_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\refinfo_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy refrel(refrel_id,site_id,ref_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\refrel_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil(soil_id,soil_class,soil_drain,sand,clay,silt,organic_thick,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\soil_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soilc(soilc_id,soilc,soilc_dep,site_id,c_year,c_doy) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\soilc_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy moss(moss_id,mlt,moss_name,doy_m,year_m,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\moss_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spec_bio(spbio_id,sp_bio,sp_name,sp_functt,year_sp,doy_sp,site_id,sp_unit) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\spbio_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spcov(cover_id,perc_cover,species,func_type,site_id, year_m, doy_m)FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\spcov_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy LAI(LAI_id, LAI, NDVI, Species, Funct_type, DOY_m, Year_m, Siteid) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\LAI_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy spec_list(specl_id,spec_name,site_id,spl_funcT) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\sp_list_u5.csv' DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_moist(sm_id,doy_sm,year_sm,SWC,sw_depth,sm_type,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\soil_moist_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy soil_temp(stemp_id,doy_st,year_st,soil_t,st_depth,site_id)  FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\soilT_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\u5\air_T_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
--export tables as csv for R
\copy contributor TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\contributor.csv' DELIMITER ',' CSV  HEADER;
\copy siteinfo TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\siteinfo.csv' DELIMITER ',' CSV  HEADER;
\copy refinfo TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\refinfo.csv' DELIMITER ',' CSV  HEADER;
\copy refrel TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\refrel.csv' DELIMITER ',' CSV  HEADER;
\copy soil TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\soil.csv' DELIMITER ',' CSV  HEADER;
\copy soilc TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\soilc.csv' DELIMITER ',' CSV  HEADER;
\copy moss TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\moss.csv' DELIMITER ',' CSV  HEADER;
\copy aboveground_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\aboveground_bio.csv' DELIMITER ',' CSV  HEADER;
\copy spec_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\spec_bio.csv' DELIMITER ',' CSV  HEADER;
\copy spcov TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\spcov.csv' DELIMITER ',' CSV  HEADER;
\copy LAI TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\LAI.csv' DELIMITER ',' CSV  HEADER;
\copy spec_list TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\spec_list.csv' DELIMITER ',' CSV  HEADER;
\copy soil_moist TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\soil_moist.csv' DELIMITER ',' CSV  HEADER;
\copy soil_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\soil_temp.csv' DELIMITER ',' CSV  HEADER;
\copy air_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\air_temp.csv' DELIMITER ',' CSV  HEADER;
\copy root_bio TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\root_bio.csv' DELIMITER ',' CSV  HEADER;


--fix duplicate air
DELETE FROM air_temp;
\copy air_temp(air_id,doy_ai,year_ai,air_t,air_height,site_id) FROM 'c:\\Users\hkropp\Google Drive\raw_data\db_upload_files\air_temp_fix_u5.csv'  DELIMITER ',' CSV HEADER QUOTE '"'
\copy air_temp TO 'c:\\Users\hkropp\Google Drive\raw_data\backup_2\air_temp_fix.csv' DELIMITER ',' CSV  HEADER;


