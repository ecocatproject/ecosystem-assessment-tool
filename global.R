# global.R
# 
# renv::snapshot(exclude=c("climateR", "RNetCDF", "rnz", "ncmeta")) # creates lockfile
# # # renv::snapshot(exclude = "rosm") # creates lockfile
# # # renv::dependencies()
# # # renv::restore() # this would restores after cloning - useful to provide in .Rprofile and .Rproj? - https://www.casualinf.com/post/2024-12-27-sharing-shiny-app-without-hosting/
# rsconnect::writeManifest() #captures dependencies from lockfile
# manifest <- jsonlite::fromJSON("manifest.json")
# # # orig.manifest <- jsonlite::fromJSON("C:/Users/rcr11kg/Downloads/manifest.json")
# # # names(orig.manifest$packages)
# names(manifest$packages)
# renv::install("DT")
# remove.packages("tidyterra")
suppressPackageStartupMessages({
  
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(shinyWidgets)
library(elevatr)
library(terra)
library(sf)
library(leaflet)
library(rnaturalearth) # ne_countries
library(rnaturalearthdata)
library(stringr) # str_extract
library(dplyr) # rename
library(leaflet.extras)
library(DT)
library(bslib)
library(bsicons)
library(leaflet.extras2)
library(RColorBrewer)
library(exactextractr)
library(tidyr) # pivot_longer
#library(climateR)
library(lubridate) # dates
library(purrr) # map
library(mgcv) # gams
library(plotly)
library(base64enc)
library(leafem) # updatelayerscontrol
library(shinycssloaders)
library(zip) # for data download
library(progress)
library(promises)
library(future)
library(rstac)
# library(rmdformats)
library(flextable)
# library(mapview)
library(xml2)
library(stars)
library(quarto)
library(ggspatial)
library(tinytex)
})

# memory options
options(shiny.maxRequestSize = 45 * 1024^2)
terraOptions(progress = 1, memfrac = 0.1, tempdir = tempdir())

# logos
rle_logo <- dataURI(file = "www/logo_RLE.png", mime = "image/png")
get_logo <- dataURI(file = "www/logo_GET.png", mime = "image/png")
cat_logo <- dataURI(file = "www/cat1.png", mime = "image/png")
# kew_logo <- dataURI(file = "www/logo_RLE.png", mime = "image/png")
#default_files <- list.files("www/data/example", full.names = TRUE)

# links
kew_ecocat_url <- tags$a(
  href = "https://www.kew.org/science/our-science/projects/EcoCAT",
  target = "_blank",
  style = "display:flex; align-items:center; gap:8px;",
  img(src = "logo.png", height = "40px"),
  "RBG Kew EcoCAT project"
)

get_url <- tags$a(
  href = "https://global-ecosystems.org/page/typology",
  target = "_blank",
  style = "display:flex; align-items:center; gap:8px;",
  img(src = get_logo, height = "50px"),
  "Global Ecosystem Typology"
)

rle_url <- tags$a(
  href = "https://www.iucnrle.org/",
  target = "_blank",
  style = "display:flex; align-items:center; gap:8px;",
  img(src = rle_logo, height = "50px"),
  "IUCN Red List of Ecosystems"
)

 # Load static data
efg_lookup <- read.csv("data/ecosystem_typology_lookup_table.csv")
# #ecor <- terra::vect("data/Ecoregions/Ecoregions2017.shp")
# realms <- terra::vect("data/realms/RL_Realms.shp")
countries <- vect(ne_countries(scale = "medium", returnclass = "sf"))

# theme
custom_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#669900", # darkgreen - #275e37 - see https://kewnet.kew.org/wp-content/uploads/2010/07/brand-guidelines-1017.pdf
  bg = "#f5f5f5",
  fg = "#333333",
  base_font = font_google("Roboto")
)
