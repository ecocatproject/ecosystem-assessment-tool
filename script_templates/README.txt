EcoCAT - reproducible Red List of Ecosystems assessment
Exported from EcoCAT {{app_version}} on {{export_date}}
=======================================================

Contents
  run_assessment.R        The script to run. Settings are at the top.
  R/ecocat_functions.R    The EcoCAT functions used by the app (no need to edit).
  data/                   {{data_note}}

How to run
  1. Unzip this folder to location of choice
  2. In R or RStudio there are 2 options:
	a) source("[source file location]/run_assessment.R", chdir = TRUE)
  	b) set the working directory to this folder
     (RStudio: Session > Set Working Directory > To Source File Location).
  	   the run:  source("run_assessment.R")

Tables, figures and a record of package versions are written to outputs/.
At the end the script reports whether each result matches the app.

Required packages: terra, sf, dplyr, purrr, tidyr, ggplot2, ggspatial, exactextractr
Optional (interactive HTML figures): plotly, leaflet, htmlwidgets
