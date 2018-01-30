
# Clear environment of temporary files
rm(list=ls())

#### Loading Packages ####

packages <- c('devtools', 'caret', 'car', 'raster', 'leaflet', 'leaflet.minicharts', 'htmltools','rgdal', 'sp', 'sf', 'methods', 'tidyverse')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#### Load and clean addresses ####
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
results <- read.csv("TAPSdata.csv")

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("Sites.shp", stringsAsFactors = F)
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords

addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID

addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"))


#### Create grid for prediction raster from LUR ####

# Create a grid over Tucson, AZ metro area
grid <- st_centroid(st_make_grid(addrs, 
                                 cellsize = 10000, # Cell size of 1000 results in 25608 points; 10k cell size = 280 points
                                 offset = st_bbox(addrs)[1:2], 
                                 #             n = c(100, 100), 
                                 crs = if (missing(addrs)) NA_crs_ else st_crs(addrs), 
                                 what = "centers"))
# Reproject grid
grid <- grid  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(grid, crs = 2868)


# Create ID
grid$hhid_x <- seq.int(nrow(grid))
grid$hhid_x <- grid$hhid_x + 10000

# Transform for mapping the grid
grid.spdf <- as(grid, "Spatial")
grid.spdf <- spTransform(grid.spdf, CRS("+proj=longlat +datum=WGS84"))

# Change grid into dataframe
grid.spdf$long <- grid.spdf@coords[,1]
grid.spdf$lat <- grid.spdf@coords[,2]
grid.df <- data.frame(grid.spdf)

# Drop excess coordinate fields
grid.df <- select(grid.df, hhid, long, lat)

# Map grid
map <- leaflet(data = grid.df) %>% addTiles() 

map %>%
  addCircleMarkers(stroke = F, radius=3)


#predict air pollution levels
pred_no2 <- predict(no2adj, griddata)
pred_nox <- predict(noxadj, griddata)
pred_pm25 <- predict(pm25adj, griddata)
pred_pm10 <- predict(pm10adj, griddata)



