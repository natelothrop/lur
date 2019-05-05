# Aim of script is to create temporally-matched (where data available and appropriate) 
# land use regression predictors for a cohort of interest using ESCAPE approaches

# Be aware polygon-based analyses will take <1 hour to run

# Coding is done for Mac-based paths

# Please update the following based on which dataset you're doing GIS analyses for:
# 1. - Set your cohort! (eg, Tucson Air Pollution Study = 'TAPS' -OR- Pima County Workers Study = 'P5')
# 2. - Set prediction year! (eg, for 2015, put 2015)


# Clear environment of temporary files
rm(list=ls())

# Set your cohort of interest by commenting out the one you DON'T want!
# Cohort <- 'TAPS'
Cohort <- 'P5'

# Set your prediction year of interest! 
#NOTE - the most recent year this is built for is 2015!
Prediction_Year <- 1991

#### Loading Packages ####

packages <- c('devtools', 'caret', 'car', 'raster', 'leaflet', 'leaflet.minicharts', 'AICcmodavg',
              'htmltools','rgdal', 'sp', 'sf', 'methods', 'tidyverse', 'lwgeom', 'arm', 'mapview', 
              'ggmap', 'lme4', 'lmerTest', 'lctools', 'maps', 'mapdata')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


#### Load P5/TAPS Addresses ####

P5_read_addrs <- function(addrs){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/Results")
  results <- read.csv("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/Results/P5_Master.csv")
  results$hhidx <- 'A'
  results$hhid_x <- paste(results$hhid, results$hhidx, sep="_") #make unique address ID
  
  
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/Shapefiles")
  addrs <- st_read("P5_Addresses.shp", stringsAsFactors = F)
  addrs$hhid_x <- paste(addrs$hhid, addrs$hhidx, sep="_") #make unique address ID
  
  addrs <- subset(addrs, select = c(hhid, hhidx, hhid_x))
  addrs$hhid_x <- paste(addrs$hhid, addrs$hhidx, sep="_") #make unique address ID
  
  addrs <- inner_join(addrs,results, by = c("hhid_x"))
  
  # This keeps only those unique household ids that start in the prediction year if during P5, otherwise, all unique household IDs are kept
  ifelse((Prediction_Year>=1987 & Prediction_Year<=1992), 
         addrs <- distinct(filter(addrs, (styear+1900) == Prediction_Year), hhid_x, .keep_all = T),
         print("Check your cohort names and prediction year!"))

  addrs <- addrs  %>% 
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(addrs, crs = 2868)
}

TAPS_read_addrs <- function(addrs){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
  results <- read.csv("TAPSdata.csv")
  
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
  addrs <- st_read("Sites.shp", stringsAsFactors = F)
  addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
  addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords
  
  addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
  addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
  
  addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"))
  
  addrs <- addrs  %>% 
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(addrs, crs = 2868)
}

# Read in the address based on the cohort above
ifelse(Cohort == "P5", addrs <- P5_read_addrs(),
       ifelse(Cohort == "TAPS", addrs <- TAPS_read_addrs(),
              print("Check your cohorts and that they are either 'P5' or 'TAPS'")))

#### Load/Prep Shapefiles (Geocoding Needed) ####
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

# Geocode bus maintainence depots
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
busdepots <- read.csv("BusDepots.csv")

busdepots$AddressFull <- paste(busdepots$Address, 
                               busdepots$City, 
                               busdepots$State,
                               busdepots$Zip, sep=', ')

busdepots <- mutate_geocode(busdepots, location=AddressFull, output="latlona" , source="dsk")

busdepots <- SpatialPointsDataFrame(coords = busdepots[,c("lon","lat")], data = busdepots,
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))

writeOGR(busdepots, dsn = ".", layer = "BusDepots" ,driver = "ESRI Shapefile")

#### Load/Prep Shapefiles ####

# Script to read and project shapefiles
shape_input <- function(shp_name) {
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
  st_read(shp_name, stringsAsFactors = F) %>%
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(crs = 2868)
}

# Selects the appropriate land use dataset file year to read
ifelse(Prediction_Year<=1997, lu <- shape_input("nlcd1992.shp"),
       ifelse(Prediction_Year>1997 & Prediction_Year<=2003, lu <- shape_input("nlcd2001.shp"),
              ifelse(Prediction_Year>2003 & Prediction_Year<=2008, lu <- shape_input("nlcd2006.shp"),
                     lu <- shape_input("nlcd2011_pimaclippolygon.shp"))))

# Selects the appropriate census file year to read
ifelse(Prediction_Year>1976 & Prediction_Year<1985, census <- shape_input("Tracts1980.shp"),
       ifelse(Prediction_Year>1986 & Prediction_Year<1995, census <- shape_input("Tracts1990.shp"),
              ifelse(Prediction_Year>1996 & Prediction_Year<2005, census <- shape_input("Tracts2000.shp"), 
                     census <- shape_input("Tracts2010.shp"))))

# Selects the appropriate Time-integrated NDVI (TIN) file year to read
# Note - as of Feb. 2019, TIN only available till 2013, so any later years default to 2013 values
ifelse(Prediction_Year<=2013, 
       tin <- shape_input(as.character(paste0("ndvi_tin_",Prediction_Year,".shp"))),
       tin <- shape_input("ndvi_tin_2013.shp"))

# Reads various shapefiles with any yearly changes denoted in file fields
air <- shape_input("Airrunway_1.shp")
busrt <- shape_input("busroute_1.shp")

# NOTE - busstops is built by doing 'Spatial Join' in ArcGIS 10.4.1; 
# with bus stops from Pima GIS as Target Feature and bus routes from Pima GIS as Join Feature;
# joining one to many (one point to many lines), within a distance of 250 feet
busstops <- shape_input("busstops_busroutes.shp") 

# Filter for only those features present in prediction year
# The bus route shapefile is VERY limited, with only to Sun Tran routes in Tucson
# Many bus stops are nowhere near routes, thus, these aren't given any joining values for routes
# As a result many have start years (styear) of 0, which we will assume began in 1977, the earliest verifiable value of any route
# Note - Many old stop points around the transit centers were manually deleted in ArcGIS
busstops <- busstops %>%
  mutate(styear = ifelse(styear == 0, 1977, styear)) %>%
  filter(Prediction_Year >= styear & Prediction_Year < endyear)

mines <- shape_input("mines_1.shp")
rail <- shape_input("railroad_1.shp")
railyard <- shape_input("railroad_yardcentroid_1.shp")
roads <- shape_input("Roads_v20170728.shp")
landfills <- shape_input("lfil_ex.shp")
cmntplant <- shape_input("CementPlants.shp")
tepplant <- shape_input("tep_station.shp")
stspeed <- shape_input("stspeed.shp")
histdev <- shape_input("dev_hist.shp")
vehtype <- shape_input("Roads_VehicleType.shp")
schools <- shape_input("schools.shp")
busdepots <- shape_input("BusDepots.shp")

# Drop developments with no units listed
histdev <- filter(histdev, UNITS>0)

# Create a transit center predictor
trnscenters <- filter(busstops, 
                      StopName=="SS/Laos Transit Center" |
                      StopName=="SS/Downtown Ronstadt Center" |
                      StopName=="SS/Tohono Transit Center")

# Create Davis Monthan AFB and Tucson Airport predictors
dmafb <- filter(air, NAME == "DAVIS-MONTHAN AIR FORCE BASE")
tia <- filter(air, NAME == "TUCSON INTERNATIONAL AIRPORT")

#Remove vehicle type fields that don't start with F or VD
vehtype <- vehtype[ , grepl( "^F" , names( vehtype ) ) | grepl( "VD" , names( vehtype ) )  ]

# Create truck vehicle loading counts in vehicle type based on ADOT truck %s, PAG traffic counts
# This includes changing truck counts into percents; updating older road counts into true vehicle counts (not in thousands)
vehtype <- vehtype %>%
  mutate(TD80=((F80__T + F80__Truck)/100) * VD80 * 1000,
         TD81=((F81__T + F81__Truck)/100) * VD81 * 1000,
         TD82=((F82__T + F82__Truck)/100) * VD82 * 1000,
         TD83=((F83__T + F83__Truck)/100) * VD83 * 1000,
         TD84=((F84__T + F84__Truck)/100) * VD84 * 1000,
         TD85=((F85__T + F85__Truck)/100) * VD85 * 1000,
         TD86=((F86__T + F86__Truck)/100) * VD86 * 1000,
         TD87=((F87__T + F87__Truck)/100) * VD87 * 1000,
         TD88=((F88__T + F88__Truck)/100) * VD88 * 1000,
         TD89=(F89__T/100) * VD89 * 1000,
         TD90=(F90__T/100) * VD90 * 1000,
         TD91=(F91__T/100) * VD91 * 1000,
         TD92=(F92__T/100) * VD92 * 1000,
         TD15=((F10__S + F10__C)/100) * VD15) %>%
  dplyr::select(starts_with('TD')) # Drop all other fields except for the trucks/day fields (TD##)

# Remove schools from list that are not part of district that provides bus services
schools <- subset(schools, !is.na(SDISTNAME))

# Drop inactive landfills or other wildcat dump sites
landfills <- subset(landfills, STATUS == "OPEN" & TYPE == "LANDFILL")

# Drop extra fields from roads
roads <- roads[ , grepl( "VD" , names( roads ) ) ]

# Update older road counts
roads <- roads %>%
  mutate(VD80=VD80*1000,
         VD81=VD81*1000,
         VD82=VD82*1000,
         VD83=VD83*1000,
         VD84=VD84*1000,
         VD85=VD85*1000,
         VD86=VD86*1000,
         VD87=VD87*1000,
         VD88=VD88*1000,
         VD89=VD89*1000,
         VD90=VD90*1000,
         VD91=VD91*1000,
         VD92=VD92*1000,
         VD93=VD93*1000,
         VD94=VD94*1000)

# Update fields for 'missing' historic traffic maps for 1982 and 1989
# Done by averaging counts on either side of the missing year, as 
# traffic count maps often use previous years, not all roads are counted the same year


# Define Roads and VD (vehicles/day) Field from Prediction Year
vd_field <- paste0('VD',as.numeric(substr(as.character(Prediction_Year), 3,4)))

roads_field <- as.character(paste0('roads$', vd_field))

# Define Major Roads
mroads_field <- as.character(paste0('mroads$', vd_field))

mroads <- subset(roads, eval(parse(text=roads_field))>5000) #5000 roads is based on ESCAPE

# Define Vehicle Type TD (trucks/day) Field from Prediction Year
td_field <- paste0('TD',as.numeric(substr(as.character(Prediction_Year), 3,4)))

vehtype_field <- as.character(paste0('vehtype$', td_field))


# Sets up directory to save predictors into using cohort name and prediction year
mainDir <- "/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP"
subDir <- "Data/LUR/Predictors"
Prediction_Year_Folder <- as.character(Prediction_Year)

if (file.exists(Prediction_Year_Folder)){
  setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
} else {
  dir.create(file.path(mainDir, Cohort, subDir, Prediction_Year))
  setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
}


#### Elevation Predictor ####
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Rasters")
elev_rast <- raster("dem.tif")
crs_raster <- "+proj=tmerc +lat_0=31 +lon_0=-111.9166666666667 +k=0.9999 +x_0=213360 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs"


elev_rast <- projectRaster(elev_rast, crs=crs_raster)

addrs$elev <- raster::extract(elev_rast, as(addrs, "Spatial"), method='bilinear', df = F)

elev <- addrs %>%
  filter(!is.na(elev)) %>%
  mutate(evel = sqrt(elev),
         styear = eval(Prediction_Year)) %>%
  dplyr::select(hhid_x, elev, styear)

st_geometry(elev) <- NULL

addrs$elev <- NULL

if (file.exists(Prediction_Year_Folder)){
  setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
} else {
  dir.create(file.path(mainDir, Cohort, subDir, Prediction_Year))
  setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
}

write.csv(elev, "elev.csv",row.names = F)

#### CALINE3-Modeled Average Annual PM2.5 Predictor ####

# Based on cohort and prediction year, read in correct ExposureForAllIndividuals.csv
ifelse((Cohort == "P5" & Prediction_Year>=1987 & Prediction_Year<=1992), 
       caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/CALINE3/1987_1992/ExposureForAllIndividuals.csv", header = T),
       ifelse((Cohort == "TAPS" & Prediction_Year>=1987 & Prediction_Year<=1992),
              caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/1987_1992/ExposureForAllIndividuals.csv", header = T),
              ifelse((Cohort == "P5" & Prediction_Year>=2010),
                     caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Data/LUR/CALINE3/2010/ExposureForAllIndividuals.csv", header = T),
                     ifelse((Cohort == "TAPS" & Prediction_Year>=2010),
                            caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/2010/ExposureForAllIndividuals.csv", header = T),
                            print("Dataset or CALINE3 year not found")))))

# Update CALINE output file
caline <- caline %>%
  filter(!is.na(CRSID)) %>%
  group_by(CRSID, Year) %>% # Average total PM2.5 by year
  summarise(caline_pm25 = mean(Total)) %>% 
  filter(Year == ifelse(eval(Prediction_Year)>=2010, 2010, eval(Prediction_Year))) # Filter for only those that match prediction year

# Merge to addrs, keeping only matching CALINE values
caline <- caline %>%
  mutate(hhid_x = ifelse(Cohort == 'P5', paste0(CRSID,"_A"), as.character(CRSID)))# create hhid_x merge field

caline <- caline %>%
  ungroup() %>%
  inner_join(addrs, caline, by=c("hhid_x")) %>%
  mutate(styear = eval(Prediction_Year)) %>%
  dplyr::select(hhid_x, caline_pm25, styear) # Keep only hhid_x and CALINE predicted PM2.5 value

write.csv(caline, "caline.csv", row.names = F)


#### Coordinate-based XY Predictors ####

coords <- addrs %>%
  mutate(Xcoord = st_coordinates(addrs)[,1],
         Ycoord = st_coordinates(addrs)[,2],
         XplusY = Xcoord + Ycoord,
         XminusY = Xcoord - Ycoord,
         styear = eval(Prediction_Year)) %>%
  dplyr::select(hhid_x, Xcoord, Ycoord, XplusY, XminusY, styear)

st_geometry(coords) <- NULL

write.csv(coords, "coords.csv", row.names = F)



#### Time-Integrated NDVI (TIN) Predictor ####
#Canopy photosynthetic activity across the entire growing season (interpolated NDVI).
intx_tin <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(intx_area = st_area(intx),
             tin_intx_area = tin * intx_area) %>%
      group_by(hhid_x) %>%
      mutate(full_area = sum(intx_area),
             full_tin_wtd = sum(tin_intx_area),
             tin_wtd = full_tin_wtd/full_area)
    
    tin <- data.frame(intx)
    
    tin <- tin %>% 
      distinct(hhid_x, tin_wtd)
    
    names(tin)[2] <- paste0("tin_",as.character(abuffdists))
    
    tin$styear <- Prediction_Year
    
    write.csv(tin, paste0("tin_",as.character(abuffdists),".csv"),row.names = F)
    
  } else {
    print("tin_",as.character(abuffdists)," had no intersections")
  }
}

abuffdists <- c(5000, 1000, 500, 300, 100)
predictors <- list("tin" = tin)
mapply(FUN = intx_tin, predictors, abuffdists)

#### Point-based Areal Predictors ####

abuffdists <- c(5000, 1000, 500, 300, 100)
intx_pts_inarea <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      group_by(hhid_x) %>%
      mutate(pts_sum = n())
    
    intx <- data.frame(intx)
    
    intx <- intx %>% 
      distinct(hhid_x, pts_sum)
    
    names(intx)[2] <- paste0(names(predictors)[i],"_",as.character(abuffdists))
    
    intx$styear <- eval(Prediction_Year)
    
    write.csv(intx, paste0(names(predictors)[i],"_",as.character(abuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_",as.character(abuffdists))," had no intersections")
  }
}

predictors <- list("schools" = schools)
mapply(FUN = intx_pts_inarea, predictors, abuffdists)

# Total bus stops in area
# Total bus stops can be thought of as the total number of bus lines that stop at a location
# For example, if 2 bus lines stop at a location, there will be 2 point features at that location (one for each line)
predictors <- list("busstops" = busstops)
mapply(FUN = intx_pts_inarea, predictors, abuffdists)



#### Nearest Point/Polygon Predictors - No Time Component ####

nearest_poly_pt <- function(p,i){
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
  
  predictor_nr$styear <- eval(Prediction_Year)
  
  
  if (file.exists(Prediction_Year_Folder)){
    setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
  } else {
    dir.create(file.path(mainDir, Cohort, subDir, Prediction_Year))
    setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
  }
  
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

# Distance to nearest Sun Tran transit center
predictors <- list("trnscenters" = trnscenters)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to rail yard
predictors <- list("railyard" = railyard)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to TEP Generating Station off I10, Tucson
predictors <- list("tepplant" = tepplant)
mapply(FUN = nearest_poly_pt, predictors)

# Distance to nearest school
predictors <- list("schools" = schools)
mapply(FUN = nearest_poly_pt, predictors)



#### Nearest Point/Polygon Predictors - With Time Component ####



# Distance to nearest active Sun Tran bus stop
predictors <- list("busstops" = busstops)
mapply(FUN = nearest_poly_pt, predictors)


# Distance to nearest bus maintenance depot
# Filter for only those features present in prediction year
# 1992 is the oldest satellite image verifiable year, all features in 1992 assumed to be there prior
busdepots <- busdepots %>%
  mutate(styear = ifelse(Prediction_Year<=1992, 1980, StartYr), 
         endyear = EndYear) %>%
  filter(Prediction_Year >= styear & Prediction_Year < endyear)

predictors <- list("busdepots" = busdepots)
mapply(FUN = nearest_poly_pt, predictors)


# Distance to nearest cement plant
# Filter for only those features present in prediction year
# 1992 is the oldest satellite image verifiable year, all features in 1992 assumed to be there prior
cmntplant <- cmntplant %>%
  mutate(styear = ifelse(Prediction_Year<=1992, 1980, StartYr), 
         endyear = EndYear) %>%
  filter(Prediction_Year >= styear & Prediction_Year < endyear)

predictors <- list("cmntplant" = cmntplant)
mapply(FUN = nearest_poly_pt, predictors)




# # # # Line-based Predictors # # # #

#### Line Length in Buffer Predictors - No Vehicle Loading ####

# line in area buffer distances
lbuffdists <- c(1000, 500, 300, 100, 50, 25)

intx_novehicles <- function(p,lbuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(roadlength = (st_length(intx) * 0.3048))#NOTE dividing to convert to length from ft to meters
    
    # Road (line) length
    rl <- data.frame(rowsum(x = intx$roadlength, group = intx$hhid_x))
    rl$hhid_x <- row.names(rl)
    names(rl)[1] <- paste0(names(predictors)[i],"_rl_",as.character(lbuffdists))
    rownames(rl) <- c()
    
    rl$styear <- Prediction_Year
    
    write.csv(rl, paste0(names(predictors)[i],"_rl_",as.character(lbuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_rl_",as.character(lbuffdists)," had no intersections"))
  }
}

# Length of bus routes within buffer
# Filter for only those routes present in prediction year
busrt <- busrt %>%
  filter(Prediction_Year >= styear & Prediction_Year < endyear)
predictors <- list("busrt" = busrt)
mapply(FUN = intx_novehicles, predictors, lbuffdists)

# Length of rail lines within buffer
predictors <- list("rail" = rail)
mapply(FUN = intx_novehicles, predictors, lbuffdists)



#### Line Length in Buffer Predictors - With Vehicle or Speed Loading ####


# Intersection of roads with any type of vehicle loadings
intx_vehicles <- function(p,lbuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(roadlength = (st_length(intx) * 0.3048)) %>% #NOTE dividing to convert to length from ft to meters
      mutate(trafload = (ifelse(eval(parse(text=vd_field)) == 0, 500, eval(parse(text=vd_field)))) * (st_length(intx) * 0.3048)) #NOTE roads with no counts ("0") are subbed with 500 as per ESCAPE
    
    # Road (line) length
    rl <- data.frame(rowsum(x = intx$roadlength, group = intx$hhid_x))
    rl$hhid_x <- row.names(rl)
    names(rl)[1] <- paste0(names(predictors)[i],"_rl_",as.character(lbuffdists))
    rownames(rl) <- c()
    
    rl$styear <- Prediction_Year
    
    write.csv(rl, paste0(names(predictors)[i],"_rl_",as.character(lbuffdists),".csv"),row.names = F)
    
    # Traffic load
    tl <- data.frame(rowsum(x = intx$trafload, group = intx$hhid_x))
    tl$hhid_x <- row.names(tl)
    names(tl)[1] <- paste0(names(predictors)[i],"_tl_",as.character(lbuffdists))
    rownames(tl) <- c()
    
    tl$styear <- Prediction_Year
    
    write.csv(tl, paste0(names(predictors)[i],"_tl_",as.character(lbuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_rltl_",as.character(lbuffdists)," had no intersections"))
  }
}

# line in area buffer distances
lbuffdists <- c(1000, 500, 300, 100, 50, 25)

# Road length and traffic load for major roads (as per ESCAPE, roads>5000 veh/day) within buffers
predictors <- list("mroads" = mroads)
mapply(FUN = intx_vehicles, predictors, lbuffdists)

# Road length and traffic load for all roads within buffers
predictors <- list("roads" = roads)
mapply(FUN = intx_vehicles, predictors, lbuffdists)

# Intersection of roads with heavy vehicle type (single and combo trucks) loadings only
intx_heavyvehicles <- function(p,lbuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    
    intx <- intx %>%
      mutate(roadlength = (st_length(intx) * 0.3048)) %>% #NOTE dividing to convert to length from ft to meters
      mutate(trafload = eval(parse(text=td_field)) * (st_length(intx) * 0.3048)) #NOTE there is no truck or heavy vehicle substituation count
    
    # Road (line) length
    rl <- data.frame(rowsum(x = intx$roadlength, group = intx$hhid_x))
    rl$hhid_x <- row.names(rl)
    names(rl)[1] <- paste0(names(predictors)[i],"_rl_",as.character(lbuffdists))
    rownames(rl) <- c()
    
    rl$styear <- Prediction_Year
    
    write.csv(rl, paste0(names(predictors)[i],"_rl_",as.character(lbuffdists),".csv"),row.names = F)
    
    # Traffic load
    tl <- data.frame(rowsum(x = intx$trafload, group = intx$hhid_x))
    tl$hhid_x <- row.names(tl)
    names(tl)[1] <- paste0(names(predictors)[i],"_tl_",as.character(lbuffdists))
    rownames(tl) <- c()
    
    tl$styear <- Prediction_Year
    
    write.csv(tl, paste0(names(predictors)[i],"_tl_",as.character(lbuffdists),".csv"),row.names = F)
  } else {
    print(paste0(names(predictors)[i],"_rltl_",as.character(lbuffdists)," had no intersections"))
  }
}

# Road length and traffic load for roads with heavy vehicle type (single and combo trucks) data within buffers
# NOTE - these data are only available on state routes and interstates, so there is often NO data here for participants
predictors <- list("vehtype" = vehtype)
mapply(FUN = intx_heavyvehicles, predictors, lbuffdists)

# Intersection of roads with any type of vehicle loadings and speed limits
intx_stspeed <- function(p,lbuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*lbuffdists), p)
  intx <- st_cast(intx, "MULTILINESTRING")
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(speedload = SPEED_CD * (st_length(intx) * 0.3048)) #NOTE speed limits are based on a file from 2010/03/01

    sl <- data.frame(rowsum(x = intx$speedload, group = intx$hhid_x))
    sl$hhid_x <- row.names(sl)
    names(sl)[1] <- paste0(names(predictors)[i],"_sl_",as.character(lbuffdists))
    rownames(sl) <- c()
    
    sl$styear <- Prediction_Year
    
    write.csv(sl, paste0(names(predictors)[i],"_sl_",as.character(lbuffdists),".csv"),row.names = F)
    
  } else {
    print(paste0(names(predictors)[i],"_sl_",as.character(lbuffdists)," had no intersections"))
  }
}

# Speed load (eg speed limit * road length) for all roads with speed limits as of 2010 within buffers
predictors <- list("stspeed" = stspeed)
mapply(FUN = intx_stspeed, predictors, lbuffdists)



#### Nearest Line Predictors - With Vehicle or Speed Loading NOTE NEAREST SPEED DOESN"T WORK!!!!!####

#Nearest road and TD (truck density)
d <- st_distance(addrs, vehtype)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.td <- apply(d, 1, function(x) eval(parse(text=vehtype_field)) [order(x)][1])
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

vehtypenr$styear <- Prediction_Year

write.csv(vehtypenr, "vehtype_nr.csv", row.names = F)


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

roadnr$styear <- Prediction_Year

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

mroadnr$styear <- Prediction_Year

write.csv(mroadnr, "mroad_nr.csv", row.names = F)


#Nearest road and speed limit (as per 2010)
d <- st_distance(addrs, stspeed)
min.d <- apply(d, 1, function(x) sort(x)[1])
min.d.stspeed <- apply(d, 1, function(x) stspeed$SPEED_CD [order(x)][1])
min.d_df <- data.frame(numeric(nrow(addrs)))
min.d_df$dist <- min.d * 0.3048
min.d_df$stspeed <- min.d.stspeed
min.d_df[1] <- NULL

addrsdf <- as.data.frame(addrs)
stspeednr <- cbind(addrsdf, min.d_df)
stspeednr <- subset(stspeednr, select = c(hhid_x, dist, stspeed)) #pull out hhid_x, distance to, and speed

#rename according to ESCAPE nomenclature
#names(roadnr)[1]<-"hhid"
names(stspeednr)[2]<-"NRDistM"
names(stspeednr)[3]<-"stspeednear"

#calculate predictors based on "speed" and "dist" fields
stspeednr$distinvstspeed1<-1/stspeednr$NRDistM
stspeednr$distinvstspeed2<-(1/stspeednr$NRDistM)^2

#drop the distance variable as this isn't a predictor
stspeednr$NRDistM <- NULL

stspeednr$styear <- Prediction_Year

write.csv(stspeednr, "stspeed_nr.csv", row.names = F)


#### Nearest Line Predictors - No Vehicle or Speed Loading ####

# Nearest bus route
predictors <- list("busrt" = busrt)
mapply(FUN = nearest_poly_pt, predictors)

# Nearest rail line
predictors <- list("rail" = rail)
mapply(FUN = nearest_poly_pt, predictors)


#### Census Predictor ####
#Census population and households weighted to area
intx_census <- function(p,abuffdists,i){
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
    
    hh$styear <- Prediction_Year
    
    write.csv(hh, paste0("hh_",as.character(abuffdists),".csv"),row.names = F)
    
    
    pop <- data.frame(intx)
    
    pop <- pop %>% 
      distinct(hhid_x, pop_wtd)
    
    names(pop)[2] <- paste0("pop_",as.character(abuffdists))
    
    pop$styear <- Prediction_Year
    
    write.csv(pop, paste0("pop_",as.character(abuffdists),".csv"),row.names = F)
    
    
  } else {
    print("hhpop_",as.character(abuffdists)," had no intersections")
  }
}

abuffdists <- c(5000, 1000, 500, 300, 100)

predictors <- list("census" = census)

ifelse(Prediction_Year>1976 & Prediction_Year<1985, mapply(FUN = intx_census, predictors, abuffdists),
       ifelse(Prediction_Year>1986 & Prediction_Year<1995, mapply(FUN = intx_census, predictors, abuffdists),
              ifelse(Prediction_Year>1996 & Prediction_Year<2005, mapply(FUN = intx_census, predictors, abuffdists), 
                     mapply(FUN = intx_census, predictors, abuffdists))))


#### Land Use Predictor (1992 and 2011 NLCD years only!) ####

#Land uses from NLCD weighted to area
intx_lu_1992 <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  intx <- intx %>%
    mutate(Shape_Area = as.numeric(st_area(intx)) * 0.092903)
  
  intx$GRIDCODE<-as.character(intx$grid_code)
  
  intx$luc <- ifelse(intx$GRIDCODE=='21', "lr",
                     ifelse(intx$GRIDCODE=='22', "hr",
                            ifelse(intx$GRIDCODE=='23', "cm",
                                   ifelse(intx$GRIDCODE=='85', "ug",
                                          ifelse(intx$GRIDCODE=='11' |
                                                   intx$GRIDCODE=='12' |
                                                   intx$GRIDCODE=='40' |
                                                   intx$GRIDCODE=='41' |
                                                   intx$GRIDCODE=='42' |
                                                   intx$GRIDCODE=='43' |
                                                   intx$GRIDCODE=='50' |
                                                   intx$GRIDCODE=='51' |
                                                   intx$GRIDCODE=='60' |
                                                   intx$GRIDCODE=='70' |
                                                   intx$GRIDCODE=='71' |
                                                   intx$GRIDCODE=='91' |
                                                   intx$GRIDCODE=='92', "nt",
                                                 ifelse(intx$GRIDCODE=='61' |
                                                          intx$GRIDCODE=='80' |
                                                          intx$GRIDCODE=='81' |
                                                          intx$GRIDCODE=='82' |
                                                          intx$GRIDCODE=='83' |
                                                          intx$GRIDCODE=='84', "ag", 
                                                        ifelse(intx$GRIDCODE=='30' |
                                                                intx$GRIDCODE=='31' |
                                                                 intx$GRIDCODE=='32' |
                                                                 intx$GRIDCODE=='33', "br", "xx")))))))
  
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
  names(intx)[names(intx)=="br"] <- paste0("lu_br_",as.character(abuffdists))
  
  intx$styear <- Prediction_Year
  
  write.csv(intx, paste0("lu_",as.character(abuffdists),".csv"),row.names = F)
}

intx_lu_2011 <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  intx <- intx %>%
    mutate(Shape_Area = as.numeric(st_area(intx)) * 0.092903)
  
  intx$luc <- ifelse(intx$GRIDCODE=='22' | intx$GRIDCODE=='23', "lr",
                     ifelse(intx$GRIDCODE=='24', "hr",
                            ifelse(intx$GRIDCODE=='21', "ug",
                                   ifelse(intx$GRIDCODE=='11' |
                                            intx$GRIDCODE=='12' |
                                            intx$GRIDCODE=='41' |
                                            intx$GRIDCODE=='42' |
                                            intx$GRIDCODE=='43' |
                                            intx$GRIDCODE=='51' |
                                            intx$GRIDCODE=='52' |
                                            intx$GRIDCODE=='71' |
                                            intx$GRIDCODE=='90' |
                                            intx$GRIDCODE=='95', "nt",
                                          ifelse(intx$GRIDCODE=='81' |
                                                   intx$GRIDCODE=='82', "ag", 
                                                 ifelse(intx$GRIDCODE=='31', "br", "xx"))))))
  
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
  names(intx)[names(intx)=="br"] <- paste0("lu_br_",as.character(abuffdists))
  
  intx$styear <- Prediction_Year
  
  write.csv(intx, paste0("lu_",as.character(abuffdists),".csv"),row.names = F)
}

abuffdists <- c(5000, 1000, 500, 300, 100)

predictors <- list("lu" = lu)

# Selects the appropriate land use year and function to run
ifelse(Prediction_Year<=1997, mapply(FUN = intx_lu_1992, predictors, abuffdists),
       ifelse(Prediction_Year>1997 & Prediction_Year<=2003, mapply(FUN = intx_lu_2001, predictors, abuffdists),
              ifelse(Prediction_Year>2003 & Prediction_Year<=2008, mapply(FUN = intx_lu_2006, predictors, abuffdists),
                     ifelse(Prediction_Year>2009, mapply(FUN = intx_lu_2011, predictors, abuffdists),
                            print("Check your prediction year")))))
                            











































#### **RUN FROM HERE** ####
#### Create NO2/NOx Ratio & Combine all predictors ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
results <- read.csv("TAPSdata.csv")

# Create NO2/NOx Ratio
results$no2noxratio_adj <- results$no2_adj/results$nox_adj

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("Sites.shp", stringsAsFactors = F)
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords

addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID

addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

#create a dataframe from the addrs feature by dropping the geometry fields
st_geometry(addrs) <- NULL

write.csv(addrs, "addrs.csv", row.names = F)


filenames=list.files(path="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors", full.names=TRUE)
datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})

#combine the outcome and all predictor files
lurdata <- datalist %>%
  Reduce(function(x,y) left_join(x,y,by="hhid_x"), .)

#correct outcomes by replacing NA to 0, starting with 12th column (land use info)
lurdata[, 13:ncol(lurdata)][is.na(lurdata[, 13:ncol(lurdata)])] <- 0

#correct outcomes by replacing 0 to NA, starting with 11th column (land use info)
lurdata[, 1:12][lurdata[, 1:12]==0] <- NA





#### Data Cleaning and LUR testing - Multiple Linear ####

#drop the "A" addresses which have too few sampling periods (<2) to be used in LUR
lurdata <- subset(lurdata, lurdata$hhid_x != "QC84_A") #dropped as this was only measured once without GPS coords
lurdata <- subset(lurdata, lurdata$hhid_x != "SM47_A") #dropped as this was only measured once without GPS coords
lurdata <- subset(lurdata, lurdata$hhid_x != "WF34_A") #dropped as this is almost <25m to intersection(NE corner Tucson Blvd, Arroy Chico), thus excluding it
lurdata <- subset(lurdata, lurdata$hhid_x != "HV75_A") #dropped as this has no measures completed, thus excluding it


#univariate regressions to find starting variable for NO2
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_no2adj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$no2_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for NOx
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_noxadj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$nox_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for NO2/NOx ratio
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_no2noxratadj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$no2noxratio_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM2.5
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_pm25adj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$pm25_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM10
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_pm10adj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$pm10_adj ~ x)) )
sink()#stops diverting output



#### Data Cleaning and LUR testing - Mixed Effects ####

# #drop the "A" addresses which have too few sampling periods (<2) to be used in LUR
# lurdata <- subset(lurdata, lurdata$hhid_x != "QC84_A") #dropped as this was only measured once without GPS coords
# lurdata <- subset(lurdata, lurdata$hhid_x != "SM47_A") #dropped as this was only measured once without GPS coords
# lurdata <- subset(lurdata, lurdata$hhid_x != "WF34_A") #dropped as this is almost <25m to intersection(NE corner Tucson Blvd, Arroy Chico), thus excluding it
# lurdata <- subset(lurdata, lurdata$hhid_x != "HV75_A") #dropped as this has no measures completed, thus excluding it

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
results.me <- read.csv("TAPSData_AllObs.csv")

lurdata.me <- merge(results.me, lurdata, by=c("hhid_x"))

lurdata.me <-  lurdata.me[ ,!grepl("adj$", names(lurdata.me))]


#univariate regressions to find starting variable for NO2
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_no2.txt")#saves output to text file
lapply( lurdata.me[,-1], function(x) AICc(lmer(no2 ~ x + (1 | hhid_x), data=lurdata.me)))
sink()#stops diverting output

#univariate regressions to find starting variable for NOx
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_nox.txt")#saves output to text file
lapply( lurdata.me[,-1], function(x) AICc(lmer(nox ~ x + (1 | hhid_x), data=lurdata.me)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM2.5
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_pm25.txt")#saves output to text file
lapply( lurdata.me[,-1], function(x) AICc(lmer(pm25 ~ x + (1 | hhid_x), data=lurdata.me)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM10
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_pm10.txt")#saves output to text file
lapply( lurdata.me[,-1], function(x) AICc(lmer(pm10 ~ x + (1 | hhid_x), data=lurdata.me)) )
sink()#stops diverting output





#### Summary stats ####

lurdata %>%
  summarise(n.obs=length(no2_adj),
            n.homes=n_distinct(hhid_x),
            geomean=exp(mean(log(no2_adj))),
            geosd=exp(sd(log(no2_adj))),
            min=min(no2_adj),
            max=max(no2_adj))

lurdata %>%
  filter(!is.na(pm25_adj)) %>%
  summarise(n.obs=length(pm25_adj),
            n.homes=n_distinct(hhid_x),
            geomean=exp(mean(log(pm25_adj))),
            geosd=exp(sd(log(pm25_adj))),
            min=min(pm25_adj),
            max=max(pm25_adj))

no2 <- ggplot(lurdata, aes(no2_adj)) +
  geom_dotplot() +
  labs(x=expression(paste(NO[2], " Conc. (ppb)"))) +
  theme(text = element_text(size=20))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Charts")
ggsave(filename = "no2.jpg", plot = no2, width = 8.25, height = 5.5, dpi = 300)


nox <- ggplot(lurdata, aes(nox_adj)) +
  geom_dotplot() +
  labs(x=expression(paste(NO[x], " Conc. (ppb)"))) +
  theme(text = element_text(size=20))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Charts")
ggsave(filename = "nox.jpg", plot = nox, width = 8.25, height = 5.5, dpi = 300)


pm25 <- ggplot(lurdata, aes(pm25_adj)) +
  geom_dotplot() +
  labs(x=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")"))) +
  theme(text = element_text(size=20))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Charts")
ggsave(filename = "pm25.jpg", plot = pm25, width = 8.25, height = 5.5, dpi = 300)


pm10 <- ggplot(lurdata, aes(pm10_adj)) +
  geom_dotplot() +
  labs(x=expression(paste(PM[10], " Conc. (", mu, g/m^3,")"))) +
  theme(text = element_text(size=20))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Charts")
ggsave(filename = "pm10.jpg", plot = pm10, width = 8.25, height = 5.5, dpi = 300)

  
#### Summary maps ####
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
results <- read.csv("TAPSdata.csv")

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("Sites.shp", stringsAsFactors = F)

addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)

addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords

#drop the "A" addresses which have too few sampling periods (>2) to be used in LUR
addrs <- subset(addrs, addrs$hhid_x != "QC84_A") #dropped as this was only measured once without GPS coords
addrs <- subset(addrs, addrs$hhid_x != "SM47_A") #dropped as this was only measured once without GPS coords
addrs <- subset(addrs, addrs$hhid_x != "WF34_A") #dropped as this is almost <25m to intersection(NE corner Tucson Blvd, Arroy Chico), thus excluding it



addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID

addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"))

# Transform for mapping the addrs
addrs.spdf <- as(addrs, "Spatial")
addrs.spdf <- spTransform(addrs.spdf, CRS("+proj=longlat +datum=WGS84"))

# Change addrs into dataframe
addrs.spdf$longitude <- addrs.spdf@coords[,1]
addrs.spdf$latitude <- addrs.spdf@coords[,2]
addrs.df <- data.frame(addrs.spdf)

#Drop home without any air measures
addrs.df <- filter(addrs.df, !is.na(no2_adj))


# Drop excess coordinate fields
#addrs.df <- select(addrs.df, hhid_x, long, lat)

# Create NO2/NOx ratio for mapping
addrs.df$no2noxratio_adj <- addrs.df$no2_adj/addrs.df$nox_adj

# Create a continuous palette function for pollutants
no2_pal = colorNumeric(
  palette = "Oranges",
  domain = addrs.df$no2_adj
)

nox_pal = colorNumeric(
  palette = "Reds",
  domain = addrs.df$nox_adj
)

noxratio_pal = colorNumeric(
  palette = "Greys",
  domain = addrs.df$no2noxratio_adj
)

addrs.df.pm <- filter(addrs.df, !is.na(pm25_adj))

pm25_pal = colorNumeric(
  palette = "Blues",
  domain = addrs.df.pm$pm25_adj
)

pm10_pal = colorNumeric(
  palette = "Greens",
  domain = addrs.df.pm$pm10_adj
)


#NO2 Map
map <- leaflet(data = addrs.df) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.95,
                   color=~no2_pal(addrs.df$no2_adj),
                   popup = as.character(addrs.df$no2_adj)) %>%
  addLegend(pal = no2_pal, 
            value = addrs.df$no2_adj, 
            opacity = 1,
            title="NO2  Conc. (ppb)")

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

mapshot(map, url = "no2_taps_2015.html")

  
#NOx Map
map <- leaflet(data = addrs.df) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.95,
                   color=~nox_pal(addrs.df$nox_adj),
                   popup = as.character(addrs.df$nox_adj)) %>%
  addLegend(pal = nox_pal, 
            value = addrs.df$nox_adj, 
            opacity = 1,
            title="NOx  Conc. (ppb)")

mapshot(map, url = "nox_taps_2015.html")

#NO2/NOx Ratio Map
map <- leaflet(data = addrs.df) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.95,
                   color=~noxratio_pal(addrs.df$no2noxratio_adj),
                   popup = as.character(addrs.df$no2noxratio_adj)) %>%
  addLegend(pal = noxratio_pal, 
            value = addrs.df$no2noxratio_adj, 
            opacity = 1,
            title="NO2/NOx Ratio")

mapshot(map, url = "noxratio_taps_2015.html")

#PM2.5 Map
map <- leaflet(data = addrs.df.pm) %>% 
  addTiles() %>% 
  addCircleMarkers(data = addrs.df.pm,
                   stroke = T, fillOpacity = 0.95,
                   color=~pm25_pal(addrs.df.pm$pm25_adj),
                   popup = as.character(addrs.df.pm$pm25_adj)) %>%
  addLegend(pal = pm25_pal, 
            value = addrs.df.pm$pm25_adj, 
            opacity = 1,
            title="PM2.5  Conc. (ug/m^3)")

mapshot(map, url = "pm25_taps_2015.html")

#PM10 Map
map <- leaflet(data = addrs.df.pm) %>% 
  addTiles() %>%
  addCircleMarkers(data = addrs.df.pm,
                   stroke = T, fillOpacity = 0.95,
                   color=~pm10_pal(addrs.df.pm$pm10_adj),
                   popup = as.character(addrs.df.pm$pm10_adj)) %>%
  addLegend(pal = pm10_pal, 
            value = addrs.df.pm$pm10_adj, 
            opacity = 1,
            title="PM10  Conc. (ug/m^3)")

mapshot(map, url = "pm10_taps_2015.html")


#### Develop LUR - NO2 - Multiple Linear ####

no2adj <- (lm(no2_adj~lu_hr_1000 +
                # distintvmine1 + # NOTE - removed b/c pvalue > 0.1
                distintvrail1 +
                elev +
                # lu_nt_100 + # NOTE - removed b/c pvalue > 0.1
                roads_rl_1000 
                # schools_300 # NOTE - removed b/c pvalue > 0.1
              ,data=lurdata))
summary(no2adj)


# Save model for production of raster grid
lm_no2adj <- (lm(no2_adj~lu_hr_1000 +
                distintvrail1 +
                elev +
                roads_rl_1000
              ,data=lurdata))

# Create max values of predictors
max_lu_hr_1000 <- max(lurdata$lu_hr_1000)
max_distintvrail1 <- max(lurdata$distintvrail1)
max_elev <- max(lurdata$elev)
max_roads_rl_1000 <- max(lurdata$roads_rl_1000)


#check for multicollinearity
vif(no2adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed


# Cook's D plot
# identify D values 4/(n-k-1) 
# Observations over 1 should be checked and likely excluded
cutoff <- 4/((nrow(lurdata)-length(no2adj$coefficients)-2)) 
plot(no2adj, which=4, cook.levels=cutoff, labels.id = lurdata$hhid_x)

#Validation

#Leave one out Cross Validation
# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model_loocv <- train(no2_adj~lu_hr_1000 +
                       distintvrail1 +
                       elev +
                       roads_rl_1000
                     ,data=filter(lurdata, !is.na(no2_adj)), trControl=train_control, method="lm")
# summarize results
print(model_loocv)

#RMSE
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(no2adj$residuals)


#Hold-out Validation
set.seed(1)
in_train <- createDataPartition(lurdata$no2_adj, p = 2/3, list = FALSE)
training <- lurdata[ in_train,]
testing  <- lurdata[-in_train,]

nrow(lurdata)
nrow(training)
nrow(testing)

model_hov <- train(no2_adj~lu_hr_1000 +
                     distintvrail1 +
                     elev +
                     roads_rl_1000
                   , data = training, method = "lm")
print(model_hov)




fitno2<-summary(no2adj) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r<-merge(fitno2.r,lurdata, by=c("row.names"), all=T)
names(no2_r)[2]<-"no2_r"
no2_r <- dplyr::select(no2_r, hhid_x, no2_r)

par(mfrow = c(2, 2))
plot(no2adj, labels.id = lurdata$hhid_x)


#### Develop LUR - NO2 - Mixed Effects #### 

no2 <- (lmer(no2~distintvcmntplant2 +
               distintvbusrt1 +
               distintvmajor1 +
               distintvrail1 +
               lu_nt_1000 +
               (1 | hhid_x)
              ,data=lurdata.me))

AICc(no2)
summary(no2)

#### Develop LUR - NOx - Multiple Linear  ####

noxadj <- (lm(nox_adj~
                busstops_5000 +
                distintvmine1 +
                distintvrail1 + 
                elev +
                lu_hr_1000 +
                lu_nt_100 + 
                # roads_rl_100 + #NOTE - removed due to pvalue > 0.10
                roads_rl_50 + 
                schools_300
              ,data=subset(lurdata, !is.na(nox_adj)))) 
summary(noxadj)

#check for multicollinearity
vif(noxadj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed


# Cook's D plot
# identify D values 4/(n-k-1) 
# Observations over 1 should be checked and likely excluded
cutoff <- 4/((nrow(lurdata)-length(noxadj$coefficients)-2)) 
plot(noxadj, which=4, cook.levels=cutoff, labels.id = lurdata$hhid_x)

#Validation

#Leave one out Cross Validation
# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model_loocv <- train(nox_adj~
                       busstops_5000 +
                       distintvmine1 +
                       distintvrail1 + 
                       elev +
                       lu_hr_1000 +
                       lu_nt_100 + 
                       roads_rl_50 + 
                       schools_300
                     ,data=subset(lurdata, (!is.na(nox_adj)))) # NOTE - home removed due to Cook's D over 1
# summarize results
print(model_loocv)


fitnox<-summary(noxadj) #final model

#pull out residuals
attributes(fitnox)
fitnox.r<-fitnox$residuals

nox_r<-merge(fitnox.r,lurdata, by=c("row.names"), all=T)
names(nox_r)[2]<-"nox_r"
nox_r <- dplyr::select(nox_r, hhid_x, nox_r)

par(mfrow = c(2, 2))
plot(noxadj, labels.id = lurdata$hhid_x)

# Save model for production of raster grid
lm_noxadj <- noxadj


#### Develop LUR - NOx - Mixed Effects #### 

nox <- (lmer(nox~
                busstops_5000 +
                distintvmine1 +
                # distintvrail1 + #NOTE - removed due to high influence via ZD62_A
                elev +
                # lu_hr_1000 +
                # lu_nt_100 +
                # roads_rl_100 + #NOTE - removed due to pvalue > 0.10
                roads_rl_50 +
               (1 | hhid_x) +
               factor(SamplePeriod)
              ,data=subset(lurdata.me, !is.na(nox))))

step(nox)

AICc(nox)

#### Develop LUR - PM2.5 - Multiple Linear  ####

pm25adj <- (lm(pm25_adj~
                 # elev + #NOTE - removed due to pvalue > 0.10
                 busrt_l_300 +
                 # distintvair1 + #Removed to reduce influence of CB67A
                 Xcoord +
               hd_500 +
                 schools_500
               ,data=filter(lurdata,!is.na(pm25_adj))))
summary(pm25adj)

#check for multicollinearity
vif(pm25adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed


# Cook's D plot
# identify D values 4/(n-k-1) 
# Observations over 1 should be checked and likely excluded
cutoff <- 4/((nrow(lurdata)-length(pm25adj$coefficients)-2)) 
plot(pm25adj, which=4, cook.levels=cutoff, labels.id = lurdata$hhid_x)

#Validation

#Leave one out Cross Validation
# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model_loocv <- train(pm25_adj~
                       busrt_l_300 +
                       Xcoord +
                       hd_500 +
                       schools_500
                     ,data=filter(lurdata, !is.na(pm25_adj)), trControl=train_control, method="lm")
# summarize results
print(model_loocv)



# 
# #Hold-out Validation
# set.seed(1)
# in_train <- createDataPartition(lurdata$pm25_adj, p = 2/3, list = FALSE)
# training <- lurdata[ in_train,]
# testing  <- lurdata[-in_train,]
# 
# nrow(lurdata)
# nrow(training)
# nrow(testing)
# 
# model_hov <- train(pm25_adj~busrt_l_300 +
#                      Xcoord
#                    , data = training, method = "lm")
# print(model_hov)
# 
# 


fitpm25<-summary(pm25adj) #final model

#pull out residuals
attributes(fitpm25)
fitpm25.r<-fitpm25$residuals

pm25_r<-merge(fitpm25.r,filter(lurdata, !is.na(pm25_adj)), by=c("row.names"), all=T)
names(pm25_r)[2]<-"pm25_r"
pm25_r <- dplyr::select(pm25_r, hhid_x, pm25_r)

par(mfrow = c(2, 2))
plot(pm25adj, labels.id = lurdata$hhid_x)


# Save model for production of raster grid
lm_pm25adj <- pm25adj



#### Develop LUR - PM2.5 - Mixed Effects #### 


#### Develop LUR - PM10 - Multiple Linear  ####

pm10adj <- (lm(pm10_adj~
                 lu_hr_500 +
                 # distintvair1 +
                 # distintvmine1 +
                 # distintvrail1 +
                 lu_nt_100 +
                 # lu_ug_5000 +
                 # roads_tl_500 +
                 XplusY 
               # stspeed_sl_500 +
               ,data=filter(lurdata,!is.na(pm10_adj))))
summary(pm10adj)


AIC(pm10adj)
AICc(pm10adj)

#check for multicollinearity
vif(pm10adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed


# Cook's D plot
# identify D values 4/(n-k-1) 
# Observations over 1 should be checked and likely excluded
cutoff <- 4/((nrow(lurdata)-length(pm10adj$coefficients)-2)) 
plot(pm10adj, which=4, cook.levels=cutoff, labels.id = lurdata$hhid_x)


#Validation

#Leave one out Cross Validation
# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model_loocv <- train(pm10_adj~
                       lu_hr_500 +
                       lu_nt_100 +
                       XplusY 
                     ,data=filter(lurdata, !is.na(pm10_adj)), trControl=train_control, method="lm")
# summarize results
print(model_loocv)


fitpm10<-summary(pm10adj) #final model

#pull out residuals
attributes(fitpm10)
fitpm10.r<-fitpm10$residuals

pm10_r<-merge(fitpm10.r,filter(lurdata, !is.na(pm10_adj)), by=c("row.names"), all=T)
names(pm10_r)[2]<-"pm10_r"
pm10_r <- dplyr::select(pm10_r, hhid_x, pm10_r)

par(mfrow = c(2, 2))
plot(pm10adj, labels.id = lurdata$hhid_x)

# Save model for production of raster grid
lm_pm10adj <- pm10adj


#### Develop LUR - PM10 - Mixed Effects #### 

#### Multiple Linear vs Mixed Effects LURs (AICc) ####

# Models will be compared using the Akaike Information Criterion-Corrected for few observations.

AICc(no2adj)
AICc(no2)

AICc(noxadj)
AICc(nox)

AICc(pm25adj)
AICc(pm25)

AICc(pm10adj)
AICc(pm10)


#### Check for Spatial Autocorrelation ####
# Using the 'lctools' package: https://cran.r-project.org/web/packages/lctools/lctools.pdf

#Load in results and addresses again
addrs <- read_addrs()
#merge model residuals
resids <- left_join(addrs, no2_r, by=c("hhid_x"), all.x=T)
resids <- left_join(resids, nox_r, by=c("hhid_x"), all.x=T)
resids <- left_join(resids, pm25_r, by=c("hhid_x"), all.x=T)
resids <- left_join(resids, pm10_r, by=c("hhid_x"), all.x=T)

#drop the "A" addresses which have too few sampling periods (>2) to be used in LUR
resids <- subset(resids, resids$hhid_x != "QC84_A") #dropped as this was only measured once without GPS coords
resids <- subset(resids, resids$hhid_x != "SM47_A") #dropped as this was only measured once without GPS coords
resids <- subset(resids, resids$hhid_x != "WF34_A") #dropped as this is almost <25m to intersection(NE corner Tucson Blvd, Arroy Chico), thus excluding it

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results")

st_write(resids, "TAPS_Residuals_2015_nox.shp", delete_layer = T)
st_write(filter(resids, !is.na(pm25_r)), "TAPS_Residuals_2015_pm.shp", delete_layer = T)


resids.df <- resids

write.csv(resids.df, "TAPS_Residuals.csv", row.names = F)

resids.spdf <- as(resids, "Spatial")

resids.spdf <- spTransform(resids.spdf, CRS("+proj=longlat +datum=WGS84"))

resids.spdf$long <- resids.spdf@coords[,1]
resids.spdf$lat <- resids.spdf@coords[,2]

resids.df <- data.frame(resids.spdf)

resids.spdf.nox <- resids.spdf[!is.na(resids.spdf$no2_r),]

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

pal_no2_r <- colorNumeric("RdYlBu", resids.spdf.nox$no2_r, n = 5)

map <- leaflet(data = resids.spdf.nox) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.75,
              color = ~pal_no2_r(no2_r),
              label = ~hhid_x,
              popup = paste("NO2:", format(round(resids.spdf.nox$no2_adj, 2), nsmall = 2), "<br>",
                            "NO2 Resid:", format(round(resids.spdf.nox$no2_r, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_no2_r,
            values = ~no2_r,
            title = "NO2 Model Resids. (ppb)",
            opacity = 0.75) 

mapshot(map, url = "no2_taps_resids_2015.html")

pal_nox_r <- colorNumeric("RdYlBu", resids.spdf$nox_r, n = 5)

map <- leaflet(data = resids.spdf.nox) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.90,
                   color = ~pal_nox_r(nox_r),
                   label = ~hhid_x,
                   popup = paste("NOx:", format(round(resids.spdf.nox$nox_adj, 2), nsmall = 2), "<br>",
                                 "NOx Resid:", format(round(resids.spdf.nox$nox_r, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_nox_r,
            values = ~nox_r,
            title = "NOx Model Resids. (ppb)",
            opacity = 0.75)

mapshot(map, url = "nox_taps_resids_2015.html")

resids.spdf.pm25 <- resids.spdf[!is.na(resids.spdf$pm25_r),]

pal_pm25_r <- colorNumeric("RdYlBu", resids.spdf.pm25$pm25_r, n = 5)

map <- leaflet(data = resids.spdf.pm25) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.90,
                   color = ~pal_pm25_r(pm25_r),
                   label = ~hhid_x,
                   popup = paste("PM2.5:", format(round(resids.spdf.pm25$pm25_adj, 2), nsmall = 2), "<br>",
                                 "PM2.5 Resid:", format(round(resids.spdf.pm25$pm25_r, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_pm25_r,
            values = ~pm25_r,
            title = "PM2.5 Model Resids. (ug/m3)",
            opacity = 0.75)

mapshot(map, url = "pm25_taps_resids_2015.html")

resids.spdf.pm10 <- resids.spdf[!is.na(resids.spdf$pm10_r),]

pal_pm10_r <- colorNumeric("RdYlBu", resids.spdf.pm10$pm10_r, n = 5)

map <- leaflet(data = resids.spdf.pm10) %>% 
  addTiles() %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.90,
                   color = ~pal_pm10_r(pm10_r),
                   label = ~hhid_x,
                   popup = paste("PM10:", format(round(resids.spdf.pm10$pm10_adj, 2), nsmall = 2), "<br>",
                                 "PM10 Resid:", format(round(resids.spdf.pm10$pm10_r, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_pm10_r,
            values = ~pm10_r,
            title = "PM10 Model Resids. (ug/m3)",
            opacity = 0.75)

mapshot(map, url = "pm10_taps_resids_2015.html")

# NO2 global Moran's I with 5 nearest neighbors
coords <- cbind(resids.spdf.nox@data$long,resids.spdf.nox@data$lat)
m.I <- moransI(coords,5,resids.spdf.nox@data$no2_r)
t(as.matrix(m.I[2:7]))

# NO2 local Moran's I with 5 nearest neighbors
l.m.I <- l.moransI(coords,5,resids.spdf.nox@data$no2_r, scatter.plot = T)
l.m.I


# NOx global Moran's I with 5 nearest neighbors
coords <- cbind(resids.spdf.nox@data$long,resids.spdf.nox@data$lat)
m.I <- moransI(coords,5,resids.spdf.nox@data$nox_r)
t(as.matrix(m.I[2:7]))

# NOx local Moran's I with 5 nearest neighbors
l.m.I <- l.moransI(coords,5,resids.spdf.nox@data$nox_r, scatter.plot = T)
l.m.I


# PM2.5 global Moran's I with 3 nearest neighbors
coords <- cbind(resids.spdf.pm25@data$long,resids.spdf.pm25@data$lat)
m.I <- moransI(coords,3,resids.spdf.pm25@data$pm25_r)
t(as.matrix(m.I[2:7]))

# PM2.5 local Moran's I with 3 nearest neighbors
l.m.I <- l.moransI(coords,3,resids.spdf.pm25@data$pm25_r, scatter.plot = T)
l.m.I


# PM10 global Moran's I with 3 nearest neighbors
coords <- cbind(resids.spdf.pm10@data$long,resids.spdf.pm10@data$lat)
m.I <- moransI(coords,3,resids.spdf.pm10@data$pm10_r)
t(as.matrix(m.I[2:7]))

# PM2.5 local Moran's I with 3 nearest neighbors
l.m.I <- l.moransI(coords,3,resids.spdf.pm10@data$pm10_r, scatter.plot = T)
l.m.I
#### Refine LUR Models - Method 1 - Temp. Scaling ####
# Method 1 - scale modeled air pollution exposure values by changes over time
# e.g., for each data point in [year of external valid. data]: 
# (ref value in [year of external valid. data] - ref value in [year of model devel]) + value in [year of external valid. data

# Read in NO2, PM2.5, and PM10 background values from the longest, constantly running PDEQ monitors for NAAQS compliance
# Downloaded in 20 year increments for all available data from 1980 - 2017 from https://aqs.epa.gov/api on 2018/12/28 with user: lothrop@email.arizona.edu and password 'rubycat56'
# NO2, PM2.5, and PM10 monitor sites that running longest aren't same (NO2: Alvernon/Craycroft; PM2.5: Saguaro Park; PM10: Orange Grove)
# Note that PM2.5 monitoring is taken offline temporarily at Saguaro Park 1994-1999

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/Data/PDEQHistoric")

no2_1980_2000 <- read.csv("NO2_1980_2000.txt", header = T)
no2_2000_2017 <- read.csv("NO2_2000_2017.txt", header = T)
no2_ref <- bind_rows(no2_1980_2000, no2_2000_2017)

pm25_1980_2000 <- read.csv("PM25_1980_2000.txt", header = T)
pm25_2000_2017 <- read.csv("PM25_2000_2017.txt", header = T)
pm25_ref <- bind_rows(pm25_1980_2000, pm25_2000_2017)

pm10_1980_2000 <- read.csv("PM10_1980_2000.txt", header = T)
pm10_2000_2017 <- read.csv("PM10_2000_2017.txt", header = T)
pm10_ref <- bind_rows(pm10_1980_2000, pm10_2000_2017)

# Create annual averages for each pollutant type
no2_ref <- no2_ref %>%
  dplyr::group_by(Year.GMT) %>%
  dplyr::summarise(no2_ref_avg = mean(Sample.Measurement)) %>%
  dplyr::mutate(year = Year.GMT) %>%
  dplyr::filter(!is.na(year) & year<2017) %>%
  dplyr::select(year, no2_ref_avg)

pm25_ref <- pm25_ref %>%
  dplyr::group_by(Year.GMT) %>%
  dplyr::summarise(pm25_ref_avg = mean(Sample.Measurement)) %>%
  dplyr::mutate(year = Year.GMT) %>%
  dplyr::filter(!is.na(year) & year<2017) %>%
  dplyr::select(year, pm25_ref_avg)
  
pm10_ref <- pm10_ref %>%
  dplyr::group_by(Year.GMT) %>%
  dplyr::summarise(pm10_ref_avg = mean(Sample.Measurement)) %>%
  dplyr::mutate(year = Year.GMT) %>%
  dplyr::filter(!is.na(year) & year<2017) %>%
  dplyr::select(year, pm10_ref_avg)

ref_levels <- full_join(no2_ref, pm25_ref, by=c("year"))
ref_levels <- full_join(ref_levels, pm10_ref, by=c("year"))

# Check correlations between pollutant types
summary(lm(no2_ref_avg ~ pm25_ref_avg, data=ref_levels))
summary(lm(no2_ref_avg ~ pm10_ref_avg, data=ref_levels))
summary(lm(pm25_ref_avg ~ pm10_ref_avg, data=ref_levels))

# Limited correlation, so will impute average of missings between 2 closest known years

# For non-measured PM2.5 and PM10 for 1980s, use earliest available value, 1985 for PM10

pm25_93_00_avg <- (as.numeric(ref_levels[which(ref_levels$year==1993),3])+as.numeric(ref_levels[which(ref_levels$year==2000),3]))/2

ref_levels$pm25_ref_avg <- ifelse(ref_levels$year<1988, 
                                  as.numeric(ref_levels[which(ref_levels$year==1988),3]),
                                  ifelse(ref_levels$year>1988 & is.na(ref_levels$pm25_ref_avg),
                                         pm25_93_00_avg, 
                                         ref_levels$pm25_ref_avg))

ref_levels$pm10_ref_avg <- ifelse(is.na(ref_levels$pm10_ref_avg), 
                                  as.numeric(ref_levels[which(ref_levels$year==1985),4]),
                                  ref_levels$pm10_ref_avg)

  
# Method 1 - scale modeled air pollution exposure values by changes over time done by difference and ratio
# e.g., for each data point in [year of external valid. data]: 
# (ref value in [year of external valid. data] - ref value in [year of model devel]) + value in [year of external valid. data

# Create temporal scalar to be applied to testing dataset outcome
year_mod_devel <- 2015

year_ext_val_data <- 1987

# no2_ref_year_dif <- as.numeric(ref_levels[which(ref_levels$year==year_ext_val_data),2]) - as.numeric(ref_levels[which(ref_levels$year==year_mod_devel),2])
# pm25_ref_year_dif <- as.numeric(ref_levels[which(ref_levels$year==year_ext_val_data),3]) - as.numeric(ref_levels[which(ref_levels$year==year_mod_devel),3])
# pm10_ref_year_dif <- as.numeric(ref_levels[which(ref_levels$year==year_ext_val_data),4]) - as.numeric(ref_levels[which(ref_levels$year==year_mod_devel),4])

no2_ref_year_ratio <- as.numeric(ref_levels[which(ref_levels$year==year_ext_val_data),2]) / as.numeric(ref_levels[which(ref_levels$year==year_mod_devel),2])
pm25_ref_year_ratio <- as.numeric(ref_levels[which(ref_levels$year==year_ext_val_data),3]) / as.numeric(ref_levels[which(ref_levels$year==year_mod_devel),3])
pm10_ref_year_ratio  <- as.numeric(ref_levels[which(ref_levels$year==year_ext_val_data),4]) / as.numeric(ref_levels[which(ref_levels$year==year_mod_devel),4])


# Import PCWS data for 1987
pcws_mthd1 <- rdata_2obs

# Calibrate the Palmes NO2 measures to Ogawa measures using Caesaroni et al. 2012 equation:
pcws_mthd1$no2_adj <- (0.68 * pcws_mthd1$no2_adj) + 13.53

# Temporal trend correction with absolute difference and ratio change
# pcws_mthd1$no2_adj_scl_dif <- pcws_mthd1$no2_adj + no2_ref_year_dif

pcws_mthd1$no2_adj_scl_ratio <- pcws_mthd1$no2_adj * no2_ref_year_ratio

# plot(pcws_mthd1$no2_adj_scl_dif, pcws_mthd1$no2_adj_scl_ratio)

hist(pcws_mthd1$no2_adj)
# hist(pcws_mthd1$no2_adj_scl_dif)
hist(pcws_mthd1$no2_adj_scl_ratio)









#NO2
#NOTE - lu_hr_1000 DOES NOT EXIST in NLCD 1992 data, so we add it manually

rdata_2obs$lu_hr_1000 <- 0

rdata_2obs$lu_hr_1000 <- rdata_2obs$cm_1000

rdata_2obs$roads_rl_1000 <- rdata_2obs$rl_1000

rdata_2obs$no2adj_taps_pred <- predict(no2adj, newdata=rdata_2obs)

# Calculate R^2
print(postResample(rdata_2obs$no2adj_taps_pred,rdata_2obs$no2_adj))

# Residuals
rdata_2obs$no2adj_taps_resids <- rdata_2obs$no2_adj - rdata_2obs$no2adj_taps_pred


# Method 2 - use year-specific predictor data

# Method 3 - combination of Method 1 + 2

# Method 4 - account for changes in the predictor-air pollution level relationships by calibrating predictor coefficients to the prediction-year outcome data 
# e.g., take TAPS model, and refit same model predictors using PCWS data


#### Validate TAPS LURs on PCWS 1987 Measures ####

#Validation

#NO2
#NOTE - lu_hr_1000 DOES NOT EXIST in NLCD 1992 data, so we add it manually
rdata$lu_hr_1000 <- 0

rdata$lu_hr_1000 <- rdata$cm_1000

rdata$roads_rl_1000 <- rdata$rl_1000

rdata$no2adj_taps_pred <- predict(no2adj, newdata=rdata)

# Calculate R^2
print(postResample(rdata$no2adj_taps_pred,rdata$no2_adj))

#NO2
#NOTE - lu_hr_1000 DOES NOT EXIST in NLCD 1992 data, so we add it manually
rdata_2obs$lu_hr_1000 <- 0

rdata_2obs$lu_hr_1000 <- rdata_2obs$cm_1000

rdata_2obs$roads_rl_1000 <- rdata_2obs$rl_1000

rdata_2obs$no2adj_taps_pred <- predict(no2adj, newdata=rdata_2obs)

# Calculate R^2
print(postResample(rdata_2obs$no2adj_taps_pred,rdata_2obs$no2_adj))

# Residuals
rdata_2obs$no2adj_taps_resids <- rdata_2obs$no2_adj - rdata_2obs$no2adj_taps_pred

summary(rdata_2obs$no2adj_taps_resids)

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")

write.csv(rdata_2obs,"TAPS_NO2_ExtPCWS87_Eval.csv")

#PM25
rdata$lu_hr_1000 <- 0

rdata$busrt_l_300 <- rdata$bl_300

rdata$Xcoord <- 0

rdata_pm25only <- filter(rdata, !is.na(pm25_adj))

rdata_pm25only$pm25adj_taps_pred <- predict(pm25adj, newdata=rdata_pm25only)

# Calculate R^2
print(postResample(rdata_pm25only$pm25adj_taps_pred,rdata_pm25only$pm25_adj))



#PM10
rdata$lu_hr_500 <- 0 #NOT PRESENT IN 1992 NLCD

rdata$lu_nt_100 <- rdata$nt_100

rdata$busrt_l_300 <- rdata$bl_300

rdata$XplusY <- 0

rdata_pm10only <- filter(rdata, !is.na(pm10_adj))

rdata_pm10only$pm10adj_taps_pred <- predict(pm10adj, newdata=rdata_pm10only)

# Calculate R^2
print(postResample(rdata_pm10only$pm10adj_taps_pred,rdata_pm10only$pm10_adj))




#### Tmap approach - NOT FINISHED - DO NOT RUN ####

library(devtools)
install_github("mtennekes/tmaptools")
install_github("mtennekes/tmap")
install.packages("OpenStreetMap")
install.packages("ggmap")

library("OpenStreetMap")
library("tmap", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tmaptools", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ggmap")

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
results <- read.csv("TAPSdata.csv")

# Create NO2/NOx Ratio
#results$no2noxratio_adj <- results$no2_adj/results$nox_adj

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("Sites.shp", stringsAsFactors = F)

addrs <- addrs  %>% st_set_crs(NA) %>% st_set_crs(2868)
st_transform(addrs, crs = 2868)

addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords

#drop the "A" addresses which have too few sampling periods (>2) to be used in LUR
addrs <- subset(addrs, addrs$hhid_x != "QC84_A") #dropped as this was only measured once without GPS coords
addrs <- subset(addrs, addrs$hhid_x != "SM47_A") #dropped as this was only measured once without GPS coords
addrs <- subset(addrs, addrs$hhid_x != "WF34_A") #dropped as this is almost <25m to intersection(NE corner Tucson Blvd, Arroy Chico), thus excluding it

addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID

addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"))

#Shapefiles
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
pima <- st_read("Pima_1.shp", stringsAsFactors = F)

#Update all CRS to those of addresses
pima <- pima  %>% st_set_crs(NA) %>% st_set_crs(2868)

st_transform(pima, crs = 2868)

tmap_mode("view")

addrs <- filter(addrs, !is.na(no2_adj))

no2 <- tm_shape(addrs) +
  tm_dots("no2_adj", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="NO2 Conc. (ppb)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

tmap_save(no2, "no2.html")

nox <- tm_shape(addrs) +
  tm_dots("nox_adj", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="NOx Conc. (ppb)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

tmap_save(nox, "nox.html")

pm25 <- tm_shape(addrs) +
  tm_dots("pm25_adj", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="PM2.5 Conc. (ug/m^3)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

tmap_save(pm25, "pm25.html")


pm10 <- tm_shape(addrs) +
  tm_dots("pm10_adj", 
          size = .1,
          style="kmeans",
          palette="Reds",
          title="PM10 Conc. (ug/m^3)") +
  tm_view(alpha = 1, basemaps = 'https://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}')

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Maps")

tmap_save(pm10, "pm10.html")

