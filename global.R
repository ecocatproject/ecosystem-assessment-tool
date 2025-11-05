# global.R


# renv::snapshot() # creates lockfile
# # # renv::restore() # this would restores after cloning - useful to provide in .Rprofile and .Rproj? - https://www.casualinf.com/post/2024-12-27-sharing-shiny-app-without-hosting/
# rsconnect::writeManifest() #captures dependencies from lockfile
# manifest <- jsonlite::fromJSON("manifest.json")
# names(manifest$packages)
# # renv::install("DT")
# # renv::snapshot()
# remove.packages("tidyterra")

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
library(progress)

# memory options
options(shiny.maxRequestSize = 15 * 1024^2)

# logos
rle_logo <- dataURI(file = "www/logo_RLE.png", mime = "image/png")
get_logo <- dataURI(file = "www/logo_GET.png", mime = "image/png")
# kew_logo <- dataURI(file = "www/logo_RLE.png", mime = "image/png")

# links
link_get <- tags$a(div(img(src=get_logo, height = "40px")),
                   "GET", href = "https://global-ecosystems.org/page/typology", target = "_blank"
)

link_rle <- tags$a(div(img(src=rle_logo, height = "40px")),
                   "IUCN Red List", href = "https://www.iucnrle.org/", target = "_blank"
)


# Load static data
efg_lookup <- read.csv("data/ecosystem_typology_lookup_table.csv")
# ecor <- terra::vect("data/Ecoregions/Ecoregions2017.shp")
realms <- terra::vect("data/realms/RL_Realms.shp")
countries <- vect(ne_countries(scale = "medium", returnclass = "sf"))

# theme
custom_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#669900",
  bg = "#f5f5f5",
  fg = "#333333",
  base_font = font_google("Roboto")
)

