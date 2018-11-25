# This script is to read in NDVI rasters from USGS Earth Explorer (https://lta.cr.usgs.gov/avhrr_phen)
# This phrenology dataset is at 1km resolution from 1989-present. 
# There is a NDVI dataset dating back to 1981 but it's at ~100km resolution (useless in this application).


# Clear environment of temporary files
rm(list=ls())

#### Loading Packages ####

packages <- c('devtools', 'caret', 'car', 'raster', 'leaflet', 'leaflet.minicharts', 'AICcmodavg',
              'htmltools','rgdal', 'sp', 'sf', 'methods', 'tidyverse', 'lwgeom', 'arm', 'mapview', 'spex')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

NDVI_path <- "/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Rasters/ndvi/PHAVHRR1989V01_TIF" 
ndvi <- list.files(NDVI_path,
                            full.names = TRUE,
                            pattern = "TOTND1989v4.tif$")

ndvi <- raster(ndvi)

crs_raster <- "+proj=tmerc +lat_0=31 +lon_0=-111.9166666666667 +k=0.9999 +x_0=213360 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs"
ndvi_proj <- projectRaster(ndvi, crs=crs_raster)


setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
pima <- st_read("Pima_1.shp", stringsAsFactors = F)
pima <- pima  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(pima, crs = 2868)


ndvi_crop <- crop(ndvi_proj, as(pima,"Spatial"))
plot(ndvi_crop)

ndvi_poly <- spex::polygonize(ndvi_crop)


