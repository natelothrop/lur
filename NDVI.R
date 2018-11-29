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

NDVI_path <- "/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Rasters/ndvi/PHAVHRR2013V01_TIF" 
ndvi <- list.files(NDVI_path,
                            full.names = TRUE,
                            pattern = "av_TOTND2013v4.tif$")

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

ndvi_poly <- ndvi_poly  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(ndvi_poly, crs = 2868)

#Time-Integrated NDVI:TIN	
#Canopy photosynthetic activity across the entire growing season (interpolated NDVI).

abuffdists <- c(100, 300, 500, 1000, 5000)

intx_tin <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(intx_area = st_area(intx),
             tin_intx_area = av_TOTND2013v4 * intx_area) %>%
      group_by(hhid_x) %>%
      mutate(full_area = sum(intx_area),
             full_tin_wtd = sum(tin_intx_area),
             tin_wtd = full_tin_wtd/full_area)
    
    tin <- data.frame(intx)
    
    tin <- tin %>% 
      distinct(hhid_x, tin_wtd)
    
    names(tin)[2] <- paste0("tin_",as.character(abuffdists))
    write.csv(tin, paste0("tin_",as.character(abuffdists),".csv"),row.names = F)
    
  } else {
    print("tin_",as.character(abuffdists)," had no intersections")
  }
}

predictors <- list("ndvi" = ndvi_poly)
mapply(FUN = intx_tin, predictors, abuffdists)


