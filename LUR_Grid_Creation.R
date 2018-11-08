
# Clear environment of temporary files
rm(list=ls())

#### Set Year for Prediction Grid ####
#Enter year of prediction grid
Prediction_Year <- 2010


#Enter year of prediction grid - for 2015, put 15, for 1980, put 80 - don't remove quotes!
Prediction_Year <- '80'

#### Loading Packages ####

packages <- c('devtools', 'caret', 'car', 'raster', 'leaflet', 'leaflet.minicharts', 
              'htmltools','rgdal', 'sp', 'sf', 'methods', 'tidyverse', 'plotKML')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#### Load and clean addresses ####
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
#results <- read.csv("TAPSdata.csv")

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("grid_5284ft_tucson.shp", stringsAsFactors = F)
addrs$hhid_x <- seq.int(nrow(addrs))  + 20000
str(addrs)

addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)

# Transform for mapping the addrs
addrs.spdf <- as(addrs, "Spatial")
addrs.spdf <- spTransform(addrs.spdf, CRS("+proj=longlat +datum=WGS84"))

# Change addrs into dataframe
addrs.spdf$longitude <- addrs.spdf@coords[,1]
addrs.spdf$latitude <- addrs.spdf@coords[,2]
addrs.df <- data.frame(addrs.spdf)

# Map addrs
leaflet(data = addrs.df) %>% 
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite")

# addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
# addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords
# 
# addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
# addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
# 
# addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"))


#### Load, transform if need, predictor rasters and shapfiles ####

#Rasters
#setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Rasters")
#elev_rast <- raster("dem.tif")

#Shapefiles
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
air <- st_read("Airrunway_1.shp", stringsAsFactors = F)
busrt <- st_read("busroute_1.shp", stringsAsFactors = F)


# Selects the appropriate census file year to read
census <- ifelse(Prediction_Year>1976 & Prediction_Year<1985, st_read("Tracts1980.shp", stringsAsFactors = F),
                      ifelse(Prediction_Year>1986 & Prediction_Year<1995, st_read("Tracts1990.shp", stringsAsFactors = F),
                             ifelse(Prediction_Year>1996 & Prediction_Year<2005, st_read("Tracts2000.shp", stringsAsFactors = F), 
                                    st_read("Tracts2010.shp", stringsAsFactors = F))))

#census <- st_read("Census2010_1.shp", stringsAsFactors = F)
#census <- st_read("Tracts1980.shp", stringsAsFactors = F)
mines <- st_read("mines_1.shp", stringsAsFactors = F)
rail <- st_read("railroad_1.shp", stringsAsFactors = F)
railyard <- st_read("railroad_yardcentroid_1.shp", stringsAsFactors = F)
roads <- st_read("Roads_v20170728.shp", stringsAsFactors = F)

ptldcmnt <- st_read("portlandcementplant.shp", stringsAsFactors = F)
tepplant <- st_read("tep_station.shp", stringsAsFactors = F)
stspeed <- st_read("stspeed.shp", stringsAsFactors = F)
histdev <- st_read("dev_hist.shp", stringsAsFactors = F)

lu <- st_read("nlcd2011_pimaclippolygon.shp", stringsAsFactors = F)
# lu <- st_read("nlcd1992.shp", stringsAsFactors = F)

# Drop fields from merging by Melissa Furlong other than those than contain vehicles per day data 
roads <- roads[ , grepl( "VD" , names( roads ) ) ]

#Update all CRS to those of addresses
addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
air <- air  %>% st_set_crs(NA) %>% st_set_crs(2868)
busrt <- busrt  %>% st_set_crs(NA) %>% st_set_crs(2868)
census <- census  %>% st_set_crs(NA) %>% st_set_crs(2868)
mines <- mines  %>% st_set_crs(NA) %>% st_set_crs(2868)
rail <- rail  %>% st_set_crs(NA) %>% st_set_crs(2868)
railyard <- railyard  %>% st_set_crs(NA) %>% st_set_crs(2868)
roads <- roads  %>% st_set_crs(NA) %>% st_set_crs(2868)
lu <- lu  %>% st_set_crs(NA) %>% st_set_crs(2868)
ptldcmnt <- ptldcmnt  %>% st_set_crs(NA) %>% st_set_crs(2868)
tepplant <- tepplant  %>% st_set_crs(NA) %>% st_set_crs(2868)
stspeed <- stspeed  %>% st_set_crs(NA) %>% st_set_crs(2868)
histdev <- histdev  %>% st_set_crs(NA) %>% st_set_crs(2868)

# crs_raster <- "+proj=tmerc +lat_0=31 +lon_0=-111.9166666666667 +k=0.9999 +x_0=213360 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs"


st_transform(addrs, crs = 2868)
st_transform(air, crs = 2868)
st_transform(busrt, crs = 2868)
st_transform(census, crs = 2868)
st_transform(mines, crs = 2868)
st_transform(rail, crs = 2868)
st_transform(railyard, crs = 2868)
st_transform(roads, crs = 2868)
st_transform(lu, crs = 2868)
st_transform(ptldcmnt, crs = 2868)
st_transform(tepplant, crs = 2868)
st_transform(stspeed, crs = 2868)
st_transform(histdev, crs = 2868)

# Drop developments with no units listed
histdev <- filter(histdev, UNITS>0)

# Create Davis Monthan AFB and Tucson Airport predictors
dmafb <- filter(air, NAME == "DAVIS-MONTHAN AIR FORCE BASE")
tia <- filter(air, NAME == "TUCSON INTERNATIONAL AIRPORT")


#elev_rast <- projectRaster(elev_rast, crs=crs_raster)



#### Update Road Traffic Counts to True Units from Thousands ####
roads$VD80 <- roads$VD80 * 1000

#### Define Roads and VD Field for Prediction Year ####

# Select the vehicle density prediction year
Prediction_Year <- substr(as.character(Prediction_Year), 3,4)


vd_field <- paste0('VD',as.numeric(Prediction_Year))

roads_field <- paste0('roads$VD',as.numeric(Prediction_Year))

mroads_field <- paste0('mroads$VD',as.numeric(Prediction_Year))


#### Define major roads as >5000 vehicles/day as per ESCAPE ####

mroads <- subset(roads, eval(parse(text=roads_field))>5000)



#### Elevation raster at point ####

# NOTE this section of code no longer works - extracted elevation in ArcGIS !!!!
# # Convert to SPDF for raster elevation 
# addrs_spdf <- readOGR("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles", layer="Sites")
# addrs_spdf$hhid_x <- paste(addrs_spdf$HHID, addrs_spdf$HHIDX, sep="_") #make unique address ID
# 
# proj4string(addrs_spdf) <- CRS("+init=epsg:2868")                   # set original projection
# addrs_spdf <- spTransform(addrs_spdf, CRS("+init=epsg:2868"))
# 
# elev_df <- extract(x = elev_rast, y = addrs_spdf, method = 'bilinear', df = F)
# addrsdf <- as.data.frame(addrs_spdf)
# elev <- cbind(addrsdf, elev_df)
# NOTE this section of code no longer works - extracted elevation in ArcGIS !!!!


setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
elev <- st_read("grid_5284ft_tucson_elev.shp", stringsAsFactors = F)

st_geometry(elev) <- NULL

elev$elev <- sqrt(elev$RASTERVALU)

elev$hhid_x <- seq.int(nrow(elev))  + 20000

elev <- subset(elev, select = c(hhid_x, elev))

elev <- filter(elev, !is.na(elev))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")

write.csv(elev, "elev.csv",row.names = F)



#### Polygon areal predictors - Land Use - 1992 NLCD ####

#Land uses weighted to area
abuffdists <- c(100, 300, 500, 1000, 5000)



intx_lu <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/LUPredictors/1992")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  intx <- intx %>%
    mutate(Shape_Area = as.numeric(st_area(intx)) * 0.092903)
  
  intx$GRIDCODE<-as.character(intx$GRIDCODE)
  
  intx$luc <- ifelse(intx$GRIDCODE=='21', "lr",
                     ifelse(intx$GRIDCODE=='22', "hr",
                            ifelse(intx$GRIDCODE=='23', "cm",
                                   ifelse(intx$GRIDCODE=='85', "ug",
                                         ifelse(intx$GRIDCODE=='11' |
                                                  intx$GRIDCODE=='12' |
                                                  intx$GRIDCODE=='31' |
                                                  intx$GRIDCODE=='33' |
                                                  intx$GRIDCODE=='41' |
                                                  intx$GRIDCODE=='42' |
                                                  intx$GRIDCODE=='43' |
                                                  intx$GRIDCODE=='51' |
                                                  intx$GRIDCODE=='61' |
                                                  intx$GRIDCODE=='71' |
                                                  intx$GRIDCODE=='91' |
                                                  intx$GRIDCODE=='92', "nt",
                                                      ifelse(intx$GRIDCODE=='61' |
                                                               intx$GRIDCODE=='81' |
                                                               intx$GRIDCODE=='82' |
                                                               intx$GRIDCODE=='83' |
                                                               intx$GRIDCODE=='84', "ag", "xx"))))))
  
  intx <- intx %>%
    group_by(hhid_x, luc) %>%
    summarise(area = sum(Shape_Area))
  
  st_geometry(intx) <- NULL
  
  intx <- spread(intx, luc, area)
  
  names(intx)[names(intx)=="lr"] <- paste0("lu_lr_",as.character(abuffdists))
  names(intx)[names(intx)=="hr"] <- paste0("lu_hr_",as.character(abuffdists))
  names(intx)[names(intx)=="cm"] <- paste0("lu_cm_",as.character(abuffdists))
  names(intx)[names(intx)=="ug"] <- paste0("lu_ug_",as.character(abuffdists))
  names(intx)[names(intx)=="nt"] <- paste0("lu_nt_",as.character(abuffdists))
  names(intx)[names(intx)=="ag"] <- paste0("lu_ag_",as.character(abuffdists))
  
  write.csv(intx, paste0("lu_1992_",as.character(abuffdists),".csv"),row.names = F)
}

predictors <- list("lu" = lu)
mapply(FUN = intx_lu, predictors, abuffdists)

#### Polygon areal predictors - Land Use - 2011 NLCD ####

#Land uses weighted to area
abuffdists <- c(100, 300, 500, 1000, 5000)



intx_lu_2011 <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/LUPredictors/2011")
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
  
  write.csv(intx, paste0("lu_2011_", as.character(abuffdists),".csv"),row.names = F)
}


predictors <- list("lu" = lu)
mapply(FUN = intx_lu_2011, predictors, abuffdists)


#### Polygon areal predictors - Census ####

abuffdists <- c(100, 300, 500, 1000, 5000)

#Census population and households weighted to area
intx_census <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(hhold_wtd = HHOLD * (st_area(intx)/Shape_Area)) %>%
      mutate(pop_wtd = POP * (st_area(intx)/Shape_Area))
    
    hh <- data.frame(rowsum(x = intx$hhold_wtd, group = intx$hhid_x))
    hh$hhid_x <- row.names(hh)
    names(hh)[1] <- paste0("hh_",as.character(abuffdists))
    rownames(hh) <- c()
    write.csv(hh, paste0("hh_",as.character(abuffdists),".csv"),row.names = F)
    
    pop <- data.frame(rowsum(x = intx$pop_wtd, group = intx$hhid_x))
    pop$hhid_x <- row.names(pop)
    names(pop)[1] <- paste0("pop_",as.character(abuffdists))
    rownames(pop) <- c()
    write.csv(pop, paste0("pop_",as.character(abuffdists),".csv"),row.names = F)
    
  } else {
    print("hhpop_",as.character(abuffdists)," had no intersections")
  }
}

predictors <- list("census" = census)
mapply(FUN = intx_census, predictors, abuffdists)

#### Polygon areal predictors - history of development plans - DOES NOT TAKE DATE INTO ACCOUNT ####

abuffdists <- c(100, 300, 500, 1000, 5000)

#Historic housing development planned units weighted to buffer area
# only current to 2015 as of 10/12/18, so for TAPS, no need to cut by year
intx_histdev <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
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
#### Line length of sources without vehicle loading in buffers (bus routes, rail lines) ####

# line in area buffer distances (rail, bus route, road)
lbuffdists <- c(25, 50, 100, 300, 500, 1000)

#Intersecting line sources without vehicle loading (rail, bus routes)
intx_novehicles <- function(p,lbuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
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
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(roadlength = (st_length(intx) * 0.3048)) %>% #NOTE dividing to convert to length from ft to meters
      mutate(trafload = ifelse(eval(parse(text=vd_field)) == 0, 500, eval(parse(text=vd_field))) * (st_length(intx) * 0.3048)) #NOTE this is built from VD## values and roads with no counts ("0") are subbed with 500 as per ESCAPE
    
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




#### Line length and speed loading ####

# line in area buffer distances (rail, bus route, road)
lbuffdists <- c(25, 50, 100, 300, 500, 1000)

intx_stspeed <- function(p,lbuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
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



#### Nearest line sources with vehicle loading (roads, major roads) ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")

#Nearest road and VD
d <- st_distance(addrs, roads)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.vd <- apply(d, 1, function(x) eval(parse(text=roads_field)) [order(x)][1])
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
min.d.vd <- apply(d, 1, function(x) eval(parse(text=mroads_field)) [order(x)][1])
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


#### Nearest line sources without vehicle loading (bus routes, rail lines) ####


setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")

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

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")

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




#### Nearest polygon (airports, active surface mines) and point (rail yard) sources ####

# Nearest airport runway
d <- st_distance(addrs, air)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
airnr <- cbind(addrsdf, min.d_df)
airnr <- subset(airnr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(airnr)[1]<-"hhid"
names(airnr)[2]<-"NRDistM"

airnr$distintvair1<-1/airnr$NRDistM
airnr$distintvair2<-(1/airnr$NRDistM)^2

airnr$NRDistM<-NULL
write.csv(airnr, "air_nr.csv", row.names = F)


# Nearest active surface mine
d <- st_distance(addrs, mines)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
minenr <- cbind(addrsdf, min.d_df)
minenr <- subset(minenr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(minenr)[1]<-"hhid"
names(minenr)[2]<-"NRDistM"

minenr$distintvmine1<-1/minenr$NRDistM
minenr$distintvmine2<-(1/minenr$NRDistM)^2

minenr$NRDistM<-NULL
write.csv(minenr, "mine_nr.csv", row.names = F)


# Nearest DMAFB
d <- st_distance(addrs, dmafb)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
dmafbnr <- cbind(addrsdf, min.d_df)
dmafbnr <- subset(dmafbnr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(airnr)[1]<-"hhid"
names(dmafbnr)[2]<-"NRDistM"

dmafbnr$distintvdmafb1<-1/dmafbnr$NRDistM
dmafbnr$distintvdmafb2<-(1/dmafbnr$NRDistM)^2

dmafbnr$NRDistM<-NULL
write.csv(dmafbnr, "dmafb_nr.csv", row.names = F)


# Nearest TIA (Tuc Intl Airport)
d <- st_distance(addrs, tia)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
tianr <- cbind(addrsdf, min.d_df)
tianr <- subset(tianr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(airnr)[1]<-"hhid"
names(tianr)[2]<-"NRDistM"

tianr$distintvtia1<-1/tianr$NRDistM
tianr$distintvtia2<-(1/tianr$NRDistM)^2

tianr$NRDistM<-NULL
write.csv(tianr, "tia_nr.csv", row.names = F)


# Nearest rail yard
d <- st_distance(addrs, railyard)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
railyardnr <- cbind(addrsdf, min.d_df)
railyardnr <- subset(railyardnr, select = c(hhid_x, dist)) #pull out hhid_x, distance to

#rename remaining fields
#names(railyardnr)[1]<-"hhid"
names(railyardnr)[2]<-"NRDistM"

railyardnr$distintvrailyard1<-1/railyardnr$NRDistM
railyardnr$distintvrailyard2<-(1/railyardnr$NRDistM)^2

railyardnr$NRDistM<-NULL
write.csv(railyardnr, "railyard_nr.csv", row.names = F)




#### CALINE 2010 Average Annual PM2.5 Concentration ####
# setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/2010")
# caline <- read.csv("ExposureForAllIndividuals.csv")
# 
# caline$hhid_x <- caline$CRSID
# 
# caline <- caline %>%
#   group_by(hhid_x) %>%
#   summarize(cal3_pm25 = mean(Total))
# 
# setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
# write.csv(caline, "caline.csv", row.names = F)


#### Create coordinate point predictors ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("grid_5284ft_tucson.shp", stringsAsFactors = F)
addrs$hhid_x <- seq.int(nrow(addrs))  + 20000
addrs$OID_ <- NULL

addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)


addrs$Xcoord <- st_coordinates(addrs)[,1]
addrs$Ycoord <- st_coordinates(addrs)[,2]

addrs$XplusY <- addrs$Xcoord + addrs$Ycoord
addrs$XminusY <- addrs$Xcoord - addrs$Ycoord

st_geometry(addrs) <- NULL

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")
write.csv(addrs, "coords.csv", row.names = F)


#### Combine all predictors ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("grid_5284ft_tucson.shp", stringsAsFactors = F)
addrs$hhid_x <- seq.int(nrow(addrs))  + 20000
addrs$OID_ <- NULL

addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)

#create a dataframe from the addrs feature by dropping the geometry fields
st_geometry(addrs) <- NULL

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors")

write.csv(addrs, "addrs.csv", row.names = F)

# Add something here that loads the correct LU year file based on "Prediction_Year"

# Selects the appropriate LU predictor files year to read
LU_Year <- ifelse(Prediction_Year<1997, 1992,
                 ifelse(Prediction_Year>=1997 & Prediction_Year<2003, 2001,
                        ifelse(Prediction_Year>=2003 & Prediction_Year<2008, 2006, 2011))) 

filename_lu=list.files(path=paste0("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/LUPredictors/", as.character(LU_Year)), full.names=TRUE)
gridlist_lu = lapply(filename_lu, function(x){read.csv(file=x,header=T)})


#combine all LU predictor files
griddata_lu <- gridlist_lu %>%
  Reduce(function(x,y) left_join(x,y,by="hhid_x"), .)

# Selects all other predictors to read
filenames=list.files(path="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Predictors", full.names=TRUE)
gridlist = lapply(filenames, function(x){read.csv(file=x,header=T)})

#combine the outcome and all predictor files
griddata <- gridlist %>%
  Reduce(function(x,y) left_join(x,y,by="hhid_x"), .)

griddata <- left_join(griddata, griddata_lu, by="hhid_x")

# Drop OID arcgis artifact
griddata$OID_ <- NULL

#correct outcomes by replacing NA to 0, starting with 2nd column (land use info)
griddata[, ][is.na(griddata[, ])] <- 0

# Create output file for year of prediction

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Years")

write.csv(griddata, paste0("griddata_",Prediction_Year,".csv"), row.names = F)



##### Predict air pollution levels ####
pred_no2 <- predict(lm_no2adj, griddata)
pred_nox <- predict(lm_noxadj, griddata)
pred_pm25 <- predict(lm_pm25adj, griddata)
pred_pm10 <- predict(lm_pm10adj, griddata)

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("grid_5284ft_tucson.shp", stringsAsFactors = F)
addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)

addrs$hhid_x <- seq.int(nrow(addrs))  + 20000

addrs$pred_no2 <- pred_no2
addrs$pred_nox <- pred_nox
addrs$pred_pm25 <- pred_pm25
addrs$pred_pm10 <- pred_pm10

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results")

st_write(addrs, dsn = paste0("pred_grid_", Prediction_Year, ".shp"), layer = paste0("pred_grid", Prediction_Year, ".shp", 
         driver = "ESRI Shapefile", delete_dsn = TRUE))




addrs <- st_centroid(addrs)

addrs.spdf <- as(addrs, "Spatial")

addrs.spdf <- spTransform(addrs.spdf, CRS("+proj=longlat +datum=WGS84"))

addrs.spdf$long <- addrs.spdf@coords[,1]
addrs.spdf$lat <- addrs.spdf@coords[,2]

addrs.df <- data.frame(addrs.spdf)

pred_no2.spdf <- dplyr::select(addrs.df, long, lat, pred_no2)

pred_no2.spdf <- transmute(pred_no2.spdf, 
                           x = lat,
                           y = long,
                           z = pred_no2)

coordinates(pred_no2.spdf) <- ~x+y 
gridded(pred_no2.spdf) <- TRUE
class(pred_no2.spdf)
str(pred_no2.spdf@data)


pred_no2.rast <- raster(pred_no2.spdf)

crs_raster <- "+proj=tmerc +lat_0=31 +lon_0=-111.9166666666667 +k=0.9999 +x_0=213360 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs"

pred_no2.rast <- raster::projectRaster(pred_no2.rast, from=, to=crs_raster)


# Set up the colors
val_no2 = as.numeric(addrs.spdf$z)
pal_no2 = colorNumeric("Reds", val_no2, na.color = "transparent")

# Made the map
leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(pred_no2.rast, colors = pal_no2, opacity = 0.5) %>%
  addLegend(pal = pal_no2, values = val_no2, title = "Number of Needs")





map <- leaflet(data = addrs.spdf) %>% addTiles()

pal_pm25 <- colorNumeric("Reds", addrs.spdf$pred_pm25, n = 5)

map %>%
  addCircleMarkers(stroke = F, fillOpacity = 0.50,
             color = ~pal_pm25(pred_pm25),
             label = ~hhid_x,
             popup = paste("PM2.5:", format(round(addrs.spdf$pred_pm25, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_pm25,
            values = ~pred_pm25,
            title = "PM2.5 Pred. Conc. (ug/m3)",
            opacity = 0.75)









addrs <- st_centroid(addrs)

addrs.spdf <- as(addrs, "Spatial")

addrs.spdf <- spTransform(addrs.spdf, CRS("+proj=longlat +datum=WGS84"))

addrs.spdf$long <- addrs.spdf@coords[,1]
addrs.spdf$lat <- addrs.spdf@coords[,2]

map <- leaflet(data = addrs.spdf) %>% addTiles()

pal_pm25 <- colorNumeric("Reds", addrs.spdf$pred_pm25, n = 5)

map %>%
  addMarkers(stroke = T, fillOpacity = 0.90,
                   color = ~pal_pm25(pred_pm25),
                   label = ~hhid_x,
                   popup = paste("PM2.5:", format(round(addrs.spdf$pred_pm25, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_pm25,
            values = ~pred_pm25,
            title = "PM2.5 Pred. Conc. (ug/m3)",
            opacity = 0.75)





map <- leaflet(data = addrs.spdf) %>% addTiles()

pal_pm25 <- colorNumeric("Reds", addrs.spdf$pred_pm25, n = 5)

map %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.90,
                   color = ~pal_pm25(pred_pm25),
                   label = ~hhid_x,
                   popup = paste("PM2.5:", format(round(addrs.spdf$pred_pm25, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_pm25,
            values = ~pred_pm25,
            title = "PM2.5 Pred. Conc. (ug/m3)",
            opacity = 0.75)







tmap_mode("view")

addrs <- filter(addrs, !is.na(pred_no2))

pred_no2 <- tm_shape(addrs) +
  tm_dots("pred_no2", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="NO2 Conc. (ppb)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

pred_nox <- tm_shape(addrs) +
  tm_dots("pred_nox", 
          size = .1,
          #style="kmeans",
          palette="Reds",
          title="NOx Conc. (ppb)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

pred_pm25 <- tm_shape(addrs) +
  tm_dots("pred_pm25", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="PM2.5 Conc. (ug/m^3)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

pred_pm10 <- tm_shape(addrs) +
  tm_dots("pred_pm10", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="PM10 Conc. (ug/m^3)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')


setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

tmap_save(pred_no2, "pred_no2.html")

tmap_save(pred_nox, "pred_nox.html")

tmap_save(pred_pm25, "pred_pm25.html")

tmap_save(pred_pm10, "pred_pm10.html")

#### Predict NO2 w/ PCWS 1987 Mixed Effects Model (Fall 2016) ####



# # # # # # REGRESSIONS # # # # # # 

#calculate NO2 exposure using best fitting ESCAPE model: logged outcome, >1 measurement
# griddata$no2 <- 6.621858 +
#   (-0.1639089*griddata$elev) +
#   ((4.596843 *10^-8) * griddata$cm_5000) +
#   ((1.051332 * 10^-5) * griddata$rl_1000) +
#   ((1.983009 * 10^-4) * griddata$rlm_300) +
#   (482.2285 * griddata$distintvmine1)

#calculate NO2 exposure using best fitting ESCAPE mixed effects model: logged outcome


#replace homes right against mining with max predictor values from original 1987 model done in Fall 2016
griddata$distintvmine1 <- ifelse( griddata$distintvmine1>2.952*10^-3,
                                  2.952*10^-3, 
                                  griddata$distintvmine1)

griddata$distintvair1 <- ifelse( griddata$distintvair1>0.0013267244,
                                 0.0013267244, 
                                 griddata$distintvair1)

griddata$distintvrail1 <- ifelse( griddata$distintvrail1>0.0076052224,
                                  0.0076052224, 
                                  griddata$distintvrail1)

griddata$no2me <- (6.176692 +
                     ((8.480870*10^-6)*griddata$pop_5000)+
                     (-0.1619694 * griddata$elev) +
                     (247.1642 * griddata$distintvair1) +
                     (67.58739 * griddata$distintvrail1))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Years")

write.csv(griddata, paste0("griddata_",Prediction_Year,".csv"), row.names = F)

ggplot(data=griddata, aes(x=griddata$no2me)) +
  geom_histogram()

# # # # # # REGRESSIONS # # # # # # 



#### Predict NO2 w/ TAPS 2015-2016 Model ####

# # Save model for production of raster grid
# lm_no2adj <- (lm(no2_adj~lu_hr_1000 +
#                    distintvrail1 +
#                    elev +
#                    roads_rl_1000
#                  ,data=lurdata))

# Cap predictor maxes at max from original model
griddata$lu_hr_1000 <- 0 #did not exist in Tucson in 1992

griddata$lu_hr_1000 <- ifelse( griddata$lu_hr_1000>max_lu_hr_1000, max_lu_hr_1000,
                               griddata$lu_hr_1000)

griddata$distintvrail1 <- ifelse( griddata$distintvrail1>max_distintvrail1, max_distintvrail1,
                                  griddata$distintvrail1)

griddata$elev <- ifelse( griddata$elev>max_elev, max_elev,
                         griddata$elev)

griddata$roads_rl_1000 <- ifelse( griddata$roads_rl_1000>max_roads_rl_1000, max_roads_rl_1000,
                                  griddata$roads_rl_1000)

# Predict NO2
griddata$no2 <- predict(lm_no2adj, griddata)

# Export results
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Grid/Years")

write.csv(griddata, paste0("griddata_",Prediction_Year,".csv"), row.names = F)




