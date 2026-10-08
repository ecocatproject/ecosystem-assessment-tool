# global.R



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



# 1. packages -------------------------------------------------------------
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
library(flextable)
library(xml2)
library(stars)
library(quarto)
library(ggspatial)
library(tinytex)
})


# 2. memory options -------------------------------------------------------
options(shiny.maxRequestSize = 45 * 1024^2)
terraOptions(progress = 1, memfrac = 0.1, tempdir = tempdir(), # or session_tmp()??
             memmax = 4, todisk=FALSE)
Sys.setenv(GDAL_CACHEMAX = "256")  # MB — GDAL's cache is separate from memfrac

# 3. tool theme -----------------------------------------------------------
custom_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#669900", # darkgreen - #275e37 - see https://kewnet.kew.org/wp-content/uploads/2010/07/brand-guidelines-1017.pdf
  bg = "#f5f5f5",
  fg = "#333333",
  base_font = font_google("Roboto")
)

# 4. logos ----------------------------------------------------------------
rle_logo <- dataURI(file = "www/logo_RLE.png", mime = "image/png")
get_logo <- dataURI(file = "www/logo_GET.png", mime = "image/png")
cat_logo <- dataURI(file = "www/ecocat-icon-b3.png", mime = "image/png")

# 5. links ----------------------------------------------------------------
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


# 6. static map data ------------------------------------------------------
# currently from Natural Earth
# download
# world  <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
# lakes  <- rnaturalearth::ne_download(scale = 10, type = "lakes",
#                                      category = "physical",
#                                      returnclass = "sf", destdir = "data")
# rivers  <- rnaturalearth::ne_download(scale = 10, type = "rivers_lake_centerlines",
#                                       category = "physical",
#                                       returnclass = "sf", destdir = "data")
# relief <- terra::rast("data/SR_HR.tif")
 
# NE data then checked and and saved to relevant location manually (save not shown)
# world <- sf::st_make_valid(world)
# lakes <- sf::st_make_valid(lakes)
# rivers <- sf::st_make_valid(rivers)


# 7. load static data -------------------------------------------------
efg_lookup <- read.csv("data/ecosystem_typology_lookup_table.csv")
countries <- vect("data/naturalearth/ne_10m_admin_0_countries")
countries.sf <- st_read("data/naturalearth/ne_10m_admin_0_countries")
lakes.sf <- st_read("data/naturalearth/ne_10m_lakes")
rivers.sf <- st_read("data/naturalearth/ne_10m_rivers_lake_centerlines")
relief <- rast("data/naturalearth/relief/SR_HR.tif")



