EcoCAT Assessment tool - data download
===============================================================

 - These files are downloaded from the EcoCAT Assessment Tool @
   assess.ecocatproject.org

 - The download contains only the datasets that were selected and available at
   the time of export, so not every file described below will be present. For
   example, Criterion A files need maps from more than one year.

 - CSV files can be opened in any spreadsheet program or in R (read.csv).
   GeoPackage (.gpkg) files are spatial data and can be opened in QGIS, ArcGIS,
   or R (terra::vect, sf::st_read).


1. CRITERION A - REDUCTION IN GEOGRAPHIC DISTRIBUTION
==================================================

1.1 […]a_area_change_data[…].csv
—————————————
Mapped ecosystem extent for each uploaded year, and the change in extent
between years.

Columns:			Description:
year  				classification year	
area  				ecosystem area (km2)	
dt  				time between classification years	
area_change  			area change betweeen classification years	
proportional_rate_change 	proportional rate change since previous classification year
absolute_rate_change 		absolute rate change since previous classification year
	

1.2 […]a_area_prediction_data[…].csv
--------------------------
Area extent values from the fitted area-change trend, used to estimate the
proportional decline over the Criterion A time frames.

Columns:	Description:
year		year
area		area (km2)
type		observed or projected area
method		method of area calculation (observed, proportional rate of change (PRC) or absolute rate of change (ARC))		
scenario	scenario used to calculate trends (observed, median: across all observed data, recent: only most recent period of change)
segment		period of data (observed, past, future)


2. CRITERION B - RESTRICTED GEOGRAPHIC DISTRIBUTION
================================================

2.1 […]b_eoo_data[…].csv
—————————
Extent of occurrence (EOO, Subcriterion B1) for most recent ecosystem year

Columns:	Description:
year		year
area_km2	the area of the minimum convex polygon (MCP enclosing all mapped occurrences of the ecosystem.
assessment	IUCN red list of ecosystem assessments outcome (B1)


2.2 […]b_eoo_spatial_data[…].gpkg
---------------------------
Spatial vector - the minimum convex polygon(s) used to calculate EOO.


2.3 […]b_aoo_data[…].csv
—————————
the number of
10 x 10 km grid cells occupied by the ecosystem.

Columns:	Description:
year		Year
n		number of 10 x 10 km grid cells occupied by the ecosystem
n_exceed	number of 10 x 10 km grid cells occupied by the ecosystem where the ecosystem covers more than 1% of the cell area
assessment	IUCN red list of ecosystem assessments outcome (B2)



2.4 […]b_aoo_spatial_data[…].gpkg
---------------------------
Spatial vector - the 10 x 10 km grid cells used to calculate AOO.

Columns:	Description:
type		cell classification: all: a 10km grid cell overlappoing the ecosystem, assessment: 10km grid cells containing >1% of the ecosystem 
