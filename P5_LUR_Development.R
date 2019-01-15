
# Aim of script is to create spatial analysis variables for PCWS using ESCAPE methods in R
# Be aware polygon-based analyses will take <1 hour to run
# Coding is done for Mac-based paths

# Clear environment of temporary files
rm(list=ls())

#### Loading Packages ####

packages <- c('devtools', 'caret', 'car', 'raster', 'leaflet', 'leaflet.minicharts', 'AICcmodavg',
              'htmltools','rgdal', 'sp', 'sf', 'methods', 'tidyverse', 'lwgeom', 'arm', 'mapview', 
              'ggmap', 'lme4', 'lmerTest', 'lctools')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# This was used in attempt to get geom_sf to work, but it still DOES NOT
# devtools::install_github("tidyverse/ggplot2")
# require(ggplot2)

#### 
#### Load and clean addresses ####

read_addrs <- function(addrs){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/Results")
  results <- read.csv("P5data.csv")
  
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/Shapefiles")
  addrs <- st_read("P5_Addresses.shp", stringsAsFactors = F)
  addrs$hhid_x <- paste(addrs$hhid, addrs$hhidx, sep="_") #make unique address ID

  addrs <- subset(addrs, select = c(hhid, hhidx, hhid_x))
  addrs$hhid_x <- paste(addrs$hhid, addrs$hhidx, sep="_") #make unique address ID
  
  addrs <- left_join(addrs,results, by = c("hhid", "hhidx"))
  
  addrs <- addrs  %>% 
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(addrs, crs = 2868)
}

addrs <- read_addrs()



#### Load and prep predictor shapfiles ####

# Geocode cement plant
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
cementplants <- read.csv("CementPlants.csv")

cementplants$AddressFull <- paste(cementplants$Address, 
                                  cementplants$City, 
                                  cementplants$State,
                                  cementplants$Zip, sep=', ')

cmntplant <- mutate_geocode(cementplants, location=AddressFull, output="latlona" , source="dsk")

cmntplant <- SpatialPointsDataFrame(coords = cmntplant[,c("lon","lat")], data = cmntplant,
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))

writeOGR(cmntplant, dsn = ".", layer = "CementPlants" ,driver = "ESRI Shapefile")

shape_input <- function(shp_name) {
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
  st_read(shp_name, stringsAsFactors = F) %>%
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(crs = 2868)
}

air <- shape_input("Airrunway_1.shp")
busrt <- shape_input("busroute_1.shp")
busstops <- shape_input("busstops.shp")
census <- shape_input("Tracts2010.shp")
mines <- shape_input("mines_1.shp")
rail <- shape_input("railroad_1.shp")
railyard <- shape_input("railroad_yardcentroid_1.shp")
roads <- shape_input("Roads_v20170728.shp")
lu <- shape_input("nlcd2011_pimaclippolygon.shp")
landfills <- shape_input("lfil_ex.shp")
cmntplant <- shape_input("CementPlants.shp")
tepplant <- shape_input("tep_station.shp")
stspeed <- shape_input("stspeed.shp")
histdev <- shape_input("dev_hist.shp")
vehtype <- shape_input("Roads_VehicleType.shp")

# Drop developments with no units listed
histdev <- filter(histdev, UNITS>0)

# Drop bus stops that are inactive
busstops <- filter(busstops, InService==1)

trnscenters <- filter(busstops, 
                      StopName=="SS/Laos Transit Center" |
                        StopName=="SS/Downtown Ronstadt Center" |
                        StopName=="SS/Tohono Transit Center")

#define major roads as >5000 vehicles/day as per ESCAPE
mroads <- subset(roads, roads$VD15>5000)

# Create Davis Monthan AFB and Tucson Airport predictors
dmafb <- filter(air, NAME == "DAVIS-MONTHAN AIR FORCE BASE")
tia <- filter(air, NAME == "TUCSON INTERNATIONAL AIRPORT")

# Drop fields from merging by Melissa Furlong other than those than contain vehicles per day data 
roads <- roads[ , grepl( "VD" , names( roads ) ) ]

# Remove vehicle type fields that don't start with F or VD
vehtype <- vehtype[ , grepl( "^F" , names( vehtype ) ) | grepl( "VD" , names( vehtype ) )  ]

# Create truck vehicle loading counts in vehicle type based on ADOT truck %s, PAG traffic counts
# vehtype$TD80 <- (vehtype$F80__T + vehtype$F80__Truck) * vehtype$VD80
# vehtype$TD81 <- (vehtype$F81__T + vehtype$F81__Truck) * vehtype$VD81
# vehtype$TD82 <- (vehtype$F82__T + vehtype$F82__Truck) * vehtype$VD82
# vehtype$TD83 <- (vehtype$F83__T + vehtype$F83__Truck) * vehtype$VD83
# vehtype$TD84 <- (vehtype$F84__T + vehtype$F84__Truck) * vehtype$VD84
# vehtype$TD85 <- (vehtype$F85__T + vehtype$F85__Truck) * vehtype$VD85
# vehtype$TD86 <- (vehtype$F86__T + vehtype$F86__Truck) * vehtype$VD86
# vehtype$TD87 <- (vehtype$F87__T + vehtype$F87__Truck) * vehtype$VD87
# vehtype$TD88 <- (vehtype$F88__T + vehtype$F88__Truck) * vehtype$VD88
# vehtype$TD89 <- (vehtype$F89__T) * vehtype$VD89
# vehtype$TD90 <- (vehtype$F90__T) * vehtype$VD90
# vehtype$TD91 <- (vehtype$F91__T) * vehtype$VD91
# vehtype$TD92 <- (vehtype$F92__T) * vehtype$VD92

vehtype$TD15 <- (vehtype$F10__S + vehtype$F10__C)/100 * vehtype$VD15


# Drop inactive landfills or other wildcat dump sites
landfills <- subset(landfills, STATUS == "OPEN" & TYPE == "LANDFILL")



#### Load and transform predictor rasters ####
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Rasters")
elev_rast <- raster("dem.tif")
crs_raster <- "+proj=tmerc +lat_0=31 +lon_0=-111.9166666666667 +k=0.9999 +x_0=213360 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs"

#### Elevation raster at point ####

elev_rast <- projectRaster(elev_rast, crs=crs_raster)

addrs$elev <- raster::extract(elev_rast, as(addrs, "Spatial"), method='bilinear', df = F)

addrs$elev <- sqrt(addrs$elev)

elev <- subset(addrs, select = c(hhid_x, elev))

elev <- filter(elev, !is.na(elev))

st_geometry(elev) <- NULL

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

write.csv(elev, "elev.csv",row.names = F)

addrs$elev <- NULL



#### Polygon areal predictors (census, land use, housing development history) ####

#Land uses weighted to area
abuffdists <- c(100, 300, 500, 1000, 5000)

intx_lu <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  intx <- intx %>%
    mutate(Shape_Area = as.numeric(st_area(intx)) * 0.092903)
  
  intx$GRIDCODE<-as.character(intx$GRIDCODE)
  
  intx$luc <- ifelse(intx$GRIDCODE=='22' | intx$GRIDCODE=='23', "lr",
                     ifelse(intx$GRIDCODE=='24', "hr",
                            ifelse(intx$GRIDCODE=='21', "ug",
                                   ifelse(intx$GRIDCODE=='11' |
                                            intx$GRIDCODE=='12' |
                                            intx$GRIDCODE=='31' |
                                            intx$GRIDCODE=='41' |
                                            intx$GRIDCODE=='42' |
                                            intx$GRIDCODE=='43' |
                                            intx$GRIDCODE=='51' |
                                            intx$GRIDCODE=='52' |
                                            intx$GRIDCODE=='71' |
                                            intx$GRIDCODE=='43' |
                                            intx$GRIDCODE=='90' |
                                            intx$GRIDCODE=='95', "nt",
                                          ifelse(intx$GRIDCODE=='81' |
                                                   intx$GRIDCODE=='82', "ag", "xx")))))
  
  intx <- intx %>%
    group_by(hhid_x, luc) %>%
    summarise(area = sum(Shape_Area))
  
  st_geometry(intx) <- NULL
  
  intx <- spread(intx, luc, area)
  
  names(intx)[names(intx)=="lr"] <- paste0("lu_lr_",as.character(abuffdists))
  names(intx)[names(intx)=="hr"] <- paste0("lu_hr_",as.character(abuffdists))
  names(intx)[names(intx)=="ug"] <- paste0("lu_ug_",as.character(abuffdists))
  names(intx)[names(intx)=="nt"] <- paste0("lu_nt_",as.character(abuffdists))
  names(intx)[names(intx)=="ag"] <- paste0("lu_ag_",as.character(abuffdists))
  
  write.csv(intx, paste0("lu_",as.character(abuffdists),".csv"),row.names = F)
}

predictors <- list("lu" = lu)
mapply(FUN = intx_lu, predictors, abuffdists)


#Census population and households weighted to area
abuffdists <- c(100, 300, 500, 1000, 5000)

intx_census <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(intx_area = st_area(intx),
             hh_intx_area = HHOLD * intx_area,
             pop_intx_area = POP * intx_area) %>%
      group_by(hhid_x) %>%
      mutate(full_area = sum(intx_area),
             full_hh_wtd = sum(hh_intx_area),
             full_pop_wtd = sum(pop_intx_area),
             hh_wtd = full_hh_wtd/full_area,
             pop_wtd = full_pop_wtd/full_area)
    
    hh <- data.frame(intx)
    
    hh <- hh %>% 
      distinct(hhid_x, hh_wtd)
    
    names(hh)[2] <- paste0("hh_",as.character(abuffdists))
    write.csv(hh, paste0("hh_",as.character(abuffdists),".csv"),row.names = F)
    
    
    pop <- data.frame(intx)
    
    pop <- pop %>% 
      distinct(hhid_x, pop_wtd)
    
    names(pop)[2] <- paste0("pop_",as.character(abuffdists))
    write.csv(pop, paste0("pop_",as.character(abuffdists),".csv"),row.names = F)
    
    
  } else {
    print("hhpop_",as.character(abuffdists)," had no intersections")
  }
}

predictors <- list("census" = census)
mapply(FUN = intx_census, predictors, abuffdists)


#Historic housing development planned units weighted to buffer area
# only current to 2015 as of 10/12/18, so for TAPS, no need to cut by year
abuffdists <- c(100, 300, 500, 1000, 5000)

intx_histdev <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(histdev_sum = sum(UNITS))
    
    
    hd <- data.frame(rowsum(x = intx$histdev_sum, group = intx$hhid_x))
    hd$hhid_x <- row.names(hd)
    names(hd)[1] <- paste0("hd_",as.character(abuffdists))
    rownames(hd) <- c()
    write.csv(hd, paste0("hd_",as.character(abuffdists),".csv"),row.names = F)
    
    
  } else {
    print("histdev_",as.character(abuffdists)," had no intersections")
  }
}

predictors <- list("histdev" = histdev)
mapply(FUN = intx_histdev, predictors, abuffdists)

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

#### Point areal predictors (Bus stops) ####

abuffdists <- c(100, 300, 500, 1000, 5000)

intx_busstops <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      group_by(hhid_x) %>%
      mutate(busstop_sum = n())
    
    busstops <- data.frame(intx)
    
    busstops <- busstops %>% 
      distinct(hhid_x, busstop_sum)
    
    names(busstops)[2] <- paste0("busstops_",as.character(abuffdists))
    write.csv(busstops, paste0("busstops_",as.character(abuffdists),".csv"),row.names = F)
    
    
  } else {
    print("busstops_",as.character(abuffdists)," had no intersections")
  }
}

predictors <- list("busstops" = busstops)
mapply(FUN = intx_busstops, predictors, abuffdists)



#### Line length of sources without vehicle loading in buffers (bus routes, rail lines) ####

# line in area buffer distances (rail, bus route, road)
lbuffdists <- c(25, 50, 100, 300, 500, 1000)

#Intersecting line sources without vehicle loading (rail, bus routes)
intx_novehicles <- function(p,lbuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  if (nrow(intx)>0) {
    intx <- aggregate(x = intx$Shape_Leng, by = list(intx$hhid_x), FUN = sum, drop = T)
    names(intx)[1]<-"hhid_x"
    names(intx)[2]<-paste0(names(predictors)[i],"_l_",as.character(lbuffdists))
    intx[2] <- intx[2] * 0.3048
    write.csv(intx, paste0(names(predictors)[i],"_l_",as.character(lbuffdists),".csv"),row.names = F)
  } else {
    print(paste0(names(predictors)[i],"_l_",as.character(lbuffdists)," had no intersections"))
  }
}

predictors <- list("busrt" = busrt)
mapply(FUN = intx_novehicles, predictors, lbuffdists)
predictors <- list("rail" = rail)
mapply(FUN = intx_novehicles, predictors, lbuffdists)



#### Line length and traffic loading of sources with vehicle loading (major roads, roads)####

# line in area buffer distances (rail, bus route, road)
lbuffdists <- c(25, 50, 100, 300, 500, 1000)

intx_vehicles <- function(p,lbuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(roadlength = (st_length(intx) * 0.3048)) %>% #NOTE dividing to convert to length from ft to meters
      mutate(trafload = ifelse(VD15 == 0, 500, VD15) * (st_length(intx) * 0.3048)) #NOTE this is built from VD15 values and roads with no counts ("0") are subbed with 500 as per ESCAPE
    
    rl <- data.frame(rowsum(x = intx$roadlength, group = intx$hhid_x))
    rl$hhid_x <- row.names(rl)
    names(rl)[1] <- paste0(names(predictors)[i],"_rl_",as.character(lbuffdists))
    rownames(rl) <- c()
    write.csv(rl, paste0(names(predictors)[i],"_rl_",as.character(lbuffdists),".csv"),row.names = F)
    
    
    tl <- data.frame(rowsum(x = intx$trafload, group = intx$hhid_x))
    tl$hhid_x <- row.names(tl)
    names(tl)[1] <- paste0(names(predictors)[i],"_tl_",as.character(lbuffdists))
    rownames(tl) <- c()
    write.csv(tl, paste0(names(predictors)[i],"_tl_",as.character(lbuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_rltl_",as.character(lbuffdists)," had no intersections"))
  }
}

predictors <- list("mroads" = mroads)
mapply(FUN = intx_vehicles, predictors, lbuffdists)
predictors <- list("roads" = roads)
mapply(FUN = intx_vehicles, predictors, lbuffdists)

# line in area buffer distances (rail, bus route, road)
lbuffdists <- c(25, 50, 100, 300, 500, 1000)

intx_heavyvehicles <- function(p,lbuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(roadlength = (st_length(intx) * 0.3048)) %>% #NOTE dividing to convert to length from ft to meters
      mutate(trafload = TD15 * (st_length(intx) * 0.3048)) 
    
    rl <- data.frame(rowsum(x = intx$roadlength, group = intx$hhid_x))
    rl$hhid_x <- row.names(rl)
    names(rl)[1] <- paste0(names(predictors)[i],"_rl_",as.character(lbuffdists))
    rownames(rl) <- c()
    write.csv(rl, paste0(names(predictors)[i],"_rl_",as.character(lbuffdists),".csv"),row.names = F)
    
    
    tl <- data.frame(rowsum(x = intx$trafload, group = intx$hhid_x))
    tl$hhid_x <- row.names(tl)
    names(tl)[1] <- paste0(names(predictors)[i],"_tl_",as.character(lbuffdists))
    rownames(tl) <- c()
    write.csv(tl, paste0(names(predictors)[i],"_tl_",as.character(lbuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_rltl_",as.character(lbuffdists)," had no intersections"))
  }
}

predictors <- list("vehtype" = vehtype)
mapply(FUN = intx_heavyvehicles, predictors, lbuffdists)


#### Road length and speed loading ####

# line in area buffer distances (rail, bus route, road)
lbuffdists <- c(25, 50, 100, 300, 500, 1000)

intx_stspeed <- function(p,lbuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    intx <- intx %>%
      # mutate(roadlength = (st_length(intx) * 0.3048)) %>% #NOTE dividing to convert to length from ft to meters
      mutate(speedload = SPEED_CD * (st_length(intx) * 0.3048)) #NOTE this is built from VD15 values and roads with no counts ("0") are subbed with 500 as per ESCAPE
    
    # rl <- data.frame(rowsum(x = intx$roadlength, group = intx$hhid_x))
    # rl$hhid_x <- row.names(rl)
    # names(rl)[1] <- paste0(names(predictors)[i],"_rl_",as.character(lbuffdists))
    # rownames(rl) <- c()
    # write.csv(rl, paste0(names(predictors)[i],"_rl_",as.character(lbuffdists),".csv"),row.names = F)
    
    
    sl <- data.frame(rowsum(x = intx$speedload, group = intx$hhid_x))
    sl$hhid_x <- row.names(sl)
    names(sl)[1] <- paste0(names(predictors)[i],"_sl_",as.character(lbuffdists))
    rownames(sl) <- c()
    write.csv(sl, paste0(names(predictors)[i],"_sl_",as.character(lbuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_rlsl_",as.character(lbuffdists)," had no intersections"))
  }
}

predictors <- list("stspeed" = stspeed)
mapply(FUN = intx_stspeed, predictors, lbuffdists)


#### Nearest line sources with vehicle loading (e.g. roads) ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

#Nearest road and TD (truck density)
d <- st_distance(addrs, vehtype)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.td <- apply(d, 1, function(x) vehtype$TD15 [order(x)][1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df$td <- min.d.td
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
vehtypenr <- cbind(addrsdf, min.d_df)
vehtypenr <- subset(vehtypenr, select = c(hhid_x, dist, td)) #pull out hhid_x, distance to, and vd per day

#rename according to ESCAPE nomenclature
#names(roadnr)[1]<-"hhid"
names(vehtypenr)[2]<-"NRDistM"
names(vehtypenr)[3]<-"heavytrafnear"


#calculate predictors based on "vd" and "dist" fields
vehtypenr$heavydistinvnear1<-1/vehtypenr$NRDistM
vehtypenr$heavydistinvnear2<-(1/vehtypenr$NRDistM)^2
vehtypenr$heavyintinvnear1<-vehtypenr$heavytrafnear * (1/vehtypenr$NRDistM)
vehtypenr$heavyintinvnear2<-vehtypenr$heavytrafnear * ((1/vehtypenr$NRDistM)^2)


#drop the distance variable as this isn't a predictor
vehtypenr$NRDistM <- NULL
write.csv(vehtypenr, "vehtype_nr.csv", row.names = F)


#Nearest road and VD
d <- st_distance(addrs, roads)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.vd <- apply(d, 1, function(x) roads$VD15 [order(x)][1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df$vd <- min.d.vd
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
roadnr <- cbind(addrsdf, min.d_df)
roadnr <- subset(roadnr, select = c(hhid_x, dist, vd)) #pull out hhid_x, distance to, and vd per day

#rename according to ESCAPE nomenclature
#names(roadnr)[1]<-"hhid"
names(roadnr)[2]<-"NRDistM"
names(roadnr)[3]<-"trafnear"

# as per ESCAPE, put in 500 vehicles per day for streets with no counts
for (i in 1:length(roadnr[,3])){
  if (roadnr$trafnear[i]<=0)
    roadnr$trafnear[i]<-500
  else
    roadnr$trafnear[i]<-roadnr$trafnear[i]
} 

#calculate predictors based on "vd" and "dist" fields
roadnr$distinvnear1<-1/roadnr$NRDistM
roadnr$distinvnear2<-(1/roadnr$NRDistM)^2
roadnr$intinvnear1<-roadnr$trafnear * (1/roadnr$NRDistM)
roadnr$intinvnear2<-roadnr$trafnear * ((1/roadnr$NRDistM)^2)


#drop the distance variable as this isn't a predictor
roadnr$NRDistM <- NULL
write.csv(roadnr, "road_nr.csv", row.names = F)



#Nearest major road and VD
d <- st_distance(addrs, mroads)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.vd <- apply(d, 1, function(x) mroads$VD15 [order(x)][1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df$vd <- min.d.vd
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
mroadnr <- cbind(addrsdf, min.d_df)
mroadnr <- subset(mroadnr, select = c(hhid_x, dist, vd)) #pull out hhid_x, distance to, and vd per day


#rename according to ESCAPE nomenclature
#names(mroadnr)[1]<-"hhid"
names(mroadnr)[2]<-"NRDistM"
names(mroadnr)[3]<-"trafmajor"

#calculate predictors based on "vd" and "dist" fields
mroadnr$distintvmajor1<-1/mroadnr$NRDistM
mroadnr$distintvmajor2<-(1/mroadnr$NRDistM)^2
mroadnr$intmajorinv1<-mroadnr$trafmajor * (1/mroadnr$NRDistM)
mroadnr$intmajorinv2<-mroadnr$trafmajor * ((1/mroadnr$NRDistM)^2)

#drop the distance variable as this isn't a predictor
mroadnr$NRDistM <- NULL
write.csv(mroadnr, "mroad_nr.csv", row.names = F)


#Nearest road and speed limit (as per 2010 speed limits, last date of 2010)
d <- st_distance(addrs, stspeed)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.spdlim <- apply(d, 1, function(x) stspeed$SPEED_CD [order(x)][1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df$spdlim <- min.d.spdlim
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
spdlimnr <- cbind(addrsdf, min.d_df)
spdlimnr <- subset(spdlimnr, select = c(hhid_x, dist, spdlim)) #pull out hhid_x, distance to, and vd per day

#rename according to ESCAPE nomenclature
#names(roadnr)[1]<-"hhid"
names(spdlimnr)[2]<-"NRDistM"
names(spdlimnr)[3]<-"spdlimnear"


#calculate predictors based on "vd" and "dist" fields
spdlimnr$distinvspdlim1<-1/spdlimnr$NRDistM
spdlimnr$distinvspdlim2<-(1/spdlimnr$NRDistM)^2
# spdlimnr$intinvnear1<-roadnr$trafnear * (1/roadnr$NRDistM)
# spdlimnr$intinvnear2<-roadnr$trafnear * ((1/roadnr$NRDistM)^2)


#drop the distance variable as this isn't a predictor
spdlimnr$NRDistM <- NULL
write.csv(spdlimnr, "spdlim_nr.csv", row.names = F)


#### Nearest line sources without vehicle loading (e.g. bus routes) ####


setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

# Nearest bus route
d <- st_distance(addrs, busrt)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
busrtnr <- cbind(addrsdf, min.d_df)
busrtnr <- subset(busrtnr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(busrtnr)[1]<-"hhid"
names(busrtnr)[2]<-"NRDistM"

busrtnr$distintvbusrt1<-1/busrtnr$NRDistM
busrtnr$distintvbusrt2<-(1/busrtnr$NRDistM)^2

busrtnr$NRDistM<-NULL
write.csv(busrtnr, "busrt_nr.csv", row.names = F)


# Nearest rail line
d <- st_distance(addrs, rail)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
railnr <- cbind(addrsdf, min.d_df)
railnr <- subset(railnr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(railnr)[1]<-"hhid"
names(railnr)[2]<-"NRDistM"

railnr$distintvrail1<-1/railnr$NRDistM
railnr$distintvrail2<-(1/railnr$NRDistM)^2

railnr$NRDistM<-NULL
write.csv(railnr, "rail_nr.csv", row.names = F)


#### Nearest polygon sources with unit loading (historic housing development) ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

#Nearest development and planned density
d <- st_distance(addrs, histdev)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.hd <- apply(d, 1, function(x) histdev$UNITS [order(x)][1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df$hd <- min.d.hd
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
histdevnr <- cbind(addrsdf, min.d_df)
histdevnr <- subset(histdevnr, select = c(hhid_x, dist, hd)) #pull out hhid_x, distance to, and value field

#rename according to ESCAPE nomenclature
#names(roadnr)[1]<-"hhid"
names(histdevnr)[2]<-"NRDistM"
names(histdevnr)[3]<-"hdnear"

# In case any address is in the nearest development, add 0.1 m to near distance
histdevnr$NRDistM <- histdevnr$NRDistM + 0.1


#calculate predictors based on "hd" and "dist" fields
histdevnr$distinvhd1<-1/histdevnr$NRDistM
histdevnr$distinvhd2<-(1/histdevnr$NRDistM)^2
histdevnr$intinvhd1<-histdevnr$hdnear * (1/histdevnr$NRDistM)
histdevnr$intinvhd2<-histdevnr$hdnear * ((1/histdevnr$NRDistM)^2)


#drop the distance variable as this isn't a predictor
histdevnr$NRDistM <- NULL
write.csv(histdevnr, "histdev_nr.csv", row.names = F)



#### Nearest polygon (e.g. active surface mines) and point (e.g. rail yard) sources ####

nearest_poly_pt <- function(p,i){
  addrs <- read_addrs()
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  d <- st_distance(addrs, p)
  min.d <- apply(d, 1, function(x) sort(x)[1])
  min.d_df <- data.frame(numeric(nrow(addrs)))
  min.d_df$dist <- min.d * 0.3048
  min.d_df[1] <- NULL
  
  addrsdf <- as.data.frame(addrs)
  predictor_nr <- cbind(addrsdf, min.d_df)
  predictor_nr <- subset(predictor_nr, select = c(hhid_x, dist)) #pull out hhid_x, distance to
  
  names(predictor_nr)[2]<-"NRDistM"
  
  predictor_nr$distintvpred1<-1/predictor_nr$NRDistM
  predictor_nr$distintvpred2<-(1/predictor_nr$NRDistM)^2
  
  predictor_nr$NRDistM<-NULL
  
  names(predictor_nr)[2]<-paste0("distintv",names(predictors)[i],"1")
  names(predictor_nr)[3]<-paste0("distintv",names(predictors)[i],"2")
  
  write.csv(predictor_nr, paste0(names(predictors)[i],"_nr.csv"),row.names = F)
  
}

# Distance to nearest landfill
predictors <- list("landfills" = landfills)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to nearest airport runway
predictors <- list("air" = air)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to Davis-Monthan AFB
predictors <- list("dmafb" = dmafb)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to Tucson International Airport
predictors <- list("tia" = tia)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to nearest active surface mine
predictors <- list("mines" = mines)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to nearest active Sun Tran bus stop
predictors <- list("busstops" = busstops)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to nearest Sun Tran transit center
predictors <- list("trnscenters" = trnscenters)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to rail yard
predictors <- list("railyard" = railyard)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to TEP Generating Station off I10, Tucson
predictors <- list("tepplant" = tepplant)
mapply(FUN = nearest_poly_pt, predictors)

# # Distance to nearest cement plant
predictors <- list("cmntplant" = cmntplant)
mapply(FUN = nearest_poly_pt, predictors)

#### CALINE 2010 Average Annual PM2.5 Concentration ####
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/2010")
caline <- read.csv("ExposureForAllIndividuals.csv")

caline$hhid_x <- caline$CRSID

caline <- caline %>%
  dplyr::group_by(hhid_x) %>%
  dplyr::summarise(cal3_pm25 = mean(Total))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
write.csv(caline, "caline.csv", row.names = F)


#### Create coordinate XY predictors ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("Sites.shp", stringsAsFactors = F)
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords

addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID

addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)


addrs$Xcoord <- st_coordinates(addrs)[,1]
addrs$Ycoord <- st_coordinates(addrs)[,2]

addrs$XplusY <- addrs$Xcoord + addrs$Ycoord
addrs$XminusY <- addrs$Xcoord - addrs$Ycoord

st_geometry(addrs) <- NULL

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
write.csv(addrs, "coords.csv", row.names = F)

