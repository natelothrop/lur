# Aim of script is to create temporally-matched (where data available and appropriate) 
# land use regression predictors for a cohort of interest using ESCAPE approaches

# Be aware polygon-based analyses will take <1 hour to run

# Coding is done for Mac-based paths

# Please update the following based on which dataset you're doing GIS analyses for:
# 1. - Set your cohort! (eg, Tucson Air Pollution Study = 'TAPS' -OR- Pima County Workers Study = 'PCWS' -OR- Tucson Children's Resp Study = 'TCRS')
# 2. - Set prediction year! (eg, for 2015, put 2015)


# Clear environment of temporary files
rm(list=ls())

# Set your cohort/dataset of interest by commenting out the ones you DON'T want!
# Cohort <- 'GRID' # a 1 x 1 mi grid of Tucson metro area covering area with data
# Cohort <- 'TAPS'
# Cohort <- 'PCWS'
Cohort <- 'TCRS'

# Set your prediction year of interest: any year from 1980-1992 and 2015. Data is not complete outside these years!!!
Prediction_Year <- 1980

#### Loading Packages ####

packages <- c('devtools', 'caret', 'car', 'raster', 'AICcmodavg',
              'htmltools','rgdal', 'sp', 'sf', 'methods', 'tidyverse', 
              'lwgeom', 'arm', 'lme4', 'lmerTest','lctools', 
              'parallel')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


#### Load PCWS/TAPS/TCRS Addresses ####

PCWS_read_addrs <- function(addrs){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/PCWS/Data/LUR/Results")
  results <- read.csv("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/PCWS/Data/LUR/Results/P5_Master.csv")
  results$hhidx <- 'A'
  results$hhid_x <- paste(results$hhid, results$hhidx, sep="_") #make unique address ID
  
  
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/PCWS/Data/LUR/Shapefiles")
  addrs <- st_read("P5_Addresses.shp", stringsAsFactors = F)
  addrs$hhid_x <- paste(addrs$hhid, addrs$hhidx, sep="_") #make unique address ID
  
  addrs <- subset(addrs, select = c(hhid, hhidx, hhid_x))
  addrs$hhid_x <- paste(addrs$hhid, addrs$hhidx, sep="_") #make unique address ID
  
  addrs <- inner_join(addrs,results, by = c("hhid_x"))
  
  # This keeps only those unique household ids that start in the prediction year if during PCWS, otherwise, all unique household IDs are kept
  ifelse((Prediction_Year>=1987 & Prediction_Year<=1992),
         addrs <- distinct(filter(addrs, (styear+1900) == Prediction_Year), hhid_x, .keep_all = T),
         ifelse(Prediction_Year == 2015, addrs <- distinct(addrs, hhid_x, .keep_all = T),
                print("Check your cohort names and prediction year!")))

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

TCRS_read_addrs <- function(addrs){
  setwd("/Volumes/Lexar/TCRS/Data/LUR/Shapefiles")
  
  # Read enroll addresses shapefile
  enroll_addrs <- st_read("Enroll.shp", stringsAsFactors = F) %>%
    mutate(hhid_x = paste0(hhid,"_enr")) %>%
    filter(!is.na(X))

  # Read ID1 addresses shapefile
  id1_addrs <- st_read("ID1_Age6.shp", stringsAsFactors = F) %>%
    mutate(hhid = FORMCRSID,
           hhid_x = paste0(hhid,"_id1")) %>%
    filter(!is.na(X))
  
  # Create hhid to match addresses and specific start years
  tcrs_years <- read.csv("CRS_Dates_Nate_031717.csv") %>%
    mutate(hhid = paste0(crsid, "01"),
           enroll_styear = as.numeric(str_sub(birthdate, -3, -1)),
           id1_styear = as.numeric(str_sub(date6, -3, -1)))
  
  # Merge addresses to year data
  enroll_addrs <- inner_join(enroll_addrs, tcrs_years, by = c("hhid")) %>%
    mutate(styear = enroll_styear) %>%
    dplyr::select(hhid, hhid_x, styear, Match_addr)
  
  id1_addrs <- inner_join(id1_addrs, tcrs_years, by = c("hhid")) %>%
    mutate(styear = id1_styear) %>%
    dplyr::select(hhid, hhid_x, styear, Match_addr)
  
  # Bind all addresses together
  addrs <- rbind(enroll_addrs,id1_addrs)
  
  # Filter addresses for just chosen prediction year
  addrs <- distinct(filter(addrs, (styear+1900) == Prediction_Year), hhid_x, .keep_all = T)
  
  addrs <- addrs  %>% 
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(addrs, crs = 2868)

  
}

GRID_read_addrs <- function(addrs){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
  # results <- read.csv("TAPSdata.csv")
  
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
  addrs <- st_read("grid_5284ft_tucson.shp", stringsAsFactors = F)
  addrs$hhid_x <- paste(addrs$OBJECTID, "A", sep="_") #make unique address ID

  addrs <- addrs  %>% 
    st_set_crs(NA) %>% 
    st_set_crs(2868) %>%
    st_transform(addrs, crs = 2868)
}


# Read in the address based on the cohort above
ifelse(Cohort == "PCWS", addrs <- PCWS_read_addrs(),
       ifelse(Cohort == "TAPS", addrs <- TAPS_read_addrs(),
              ifelse(Cohort == "TCRS", addrs <- TCRS_read_addrs(),
                     ifelse(Cohort == "GRID", addrs <- GRID_read_addrs(),
                            print("Check your cohorts and that they are either 'PCWS' or 'TAPS' or 'TCRS' or 'GRID'")))))


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
                     ifelse(Prediction_Year>2008 & Prediction_Year<=2013, lu <- shape_input("nlcd2011_pimaclippolygon.shp"),
                            lu <- shape_input("nlcd2016_pima.shp")))))

# Selects the appropriate census file year to read
ifelse(Prediction_Year>1976 & Prediction_Year<1985, census <- shape_input("Tracts1980.shp"),
       ifelse(Prediction_Year>1986 & Prediction_Year<1995, census <- shape_input("Tracts1990.shp"),
              ifelse(Prediction_Year>1996 & Prediction_Year<2005, census <- shape_input("Tracts2000.shp"), 
                     census <- shape_input("Tracts2010.shp"))))

# Selects the appropriate Time-integrated NDVI (TIN) file year to read
# Note - as of Feb. 2019, TIN only available from 1989-2013 at 1km, so any later years default to 2013 values
# Earlier than 1989 and you only have 100km resolution
ifelse(Prediction_Year<=2013 & Prediction_Year>=1989, 
       tin <- shape_input(as.character(paste0("ndvi_tin_",Prediction_Year,".shp"))),
       ifelse(Prediction_Year<=1988, tin <- shape_input("ndvi_tin_1989.shp"),
              tin <- shape_input("ndvi_tin_2013.shp")))

# Selects the appropriate Maximum NDVI (MAXN) file year to read
# Note - as of Feb. 2019, MAXN only available from 1989-2013 at 1km, so any later years default to 2013 values
# Earlier than 1989 and you only have 100km resolution
ifelse(Prediction_Year<=2013 & Prediction_Year>=1989, 
       maxn <- shape_input(as.character(paste0("ndvi_maxn_",Prediction_Year,".shp"))),
       ifelse(Prediction_Year<=1988, maxn <- shape_input("ndvi_maxn_1989.shp"),
              maxn <- shape_input("ndvi_maxn_2013.shp")))

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
trnscenters <- busstops %>%
  filter(StopName=="Laos Transit Center" |
           StopName=="Downtown Ronstadt Center" |
           StopName=="Tohono Transit Center")

# Create Davis Monthan AFB and Tucson Airport predictors
dmafb <- filter(air, NAME == "DAVIS-MONTHAN AIR FORCE BASE")
tia <- filter(air, NAME == "TUCSON INTERNATIONAL AIRPORT")

#Remove vehicle type fields that don't start with F or VD
vehtype <- vehtype[ , grepl( "^F" , names( vehtype ) ) | grepl( "VD" , names( vehtype ) )  ]

# Create truck vehicle loading counts in vehicle type based on ADOT truck %s, PAG traffic counts
# This includes changing truck counts into percents; updating older road counts into true vehicle counts (not in thousands)
vehtype <- vehtype %>%
  mutate(TD80=((F80__T + F80__Truck)/100) * F80_AADT,
         TD81=((F81__T + F81__Truck)/100) * F81_AADT,
         TD82=((F82__T + F82__Truck)/100) * F82_AADT,
         TD83=((F83__T + F83__Truck)/100) * F83_AADT,
         TD84=((F84__T + F84__Truck)/100) * F84_AADT,
         TD85=((F85__T + F85__Truck)/100) * F85_AADT,
         TD86=((F86__T + F86__Truck)/100) * F86_AADT,
         TD87=((F87__T + F87__Truck)/100) * F87_AADT,
         TD88=((F88__T + F88__Truck)/100) * F88_AADT,
         TD89=(F89__T/100) * F89_AADT,
         TD90=(F90__T/100) * F90_AADT,
         TD91=(F91__T/100) * F91_AADT,
         TD92=(F92__T/100) * F92_AADT,
         TD93=(F93__T/100) * F93_AADT,
         TD94=(F94__T/100) * F94_AADT,
         TD15=((F10__S + F10__C)/100) * F10_AADT) %>%
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

ifelse(Cohort == "TCRS", 
       mainDir <- "/Volumes/Lexar", 
       mainDir <- "/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP")

subDir <- "Data/LUR/Predictors"
Prediction_Year_Folder <- as.character(Prediction_Year)

if (file.exists(Prediction_Year_Folder)){
  setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
} else {
  dir.create(file.path(mainDir, Cohort, subDir, Prediction_Year))
  setwd(file.path(mainDir, Cohort, subDir, Prediction_Year))
}


#### CALINE3-Modeled Average Annual PM2.5 Predictor ####

# Based on cohort and prediction year, read in correct ExposureForAllIndividuals.csv
ifelse((Cohort == "PCWS" & Prediction_Year>=1987 & Prediction_Year<=1992), 
       caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/PCWS/Data/LUR/CALINE3/1987_1992/ExposureForAllIndividuals.csv", header = T),
       ifelse((Cohort == "TAPS" & Prediction_Year>=1987 & Prediction_Year<=1992),
              caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/1987_1992/ExposureForAllIndividuals.csv", header = T),
              ifelse((Cohort == "PCWS" & Prediction_Year>=2010),
                     caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/PCWS/Data/LUR/CALINE3/2010/ExposureForAllIndividuals.csv", header = T),
                     ifelse((Cohort == "TAPS" & Prediction_Year>=2010),
                            caline <- read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/2010/ExposureForAllIndividuals.csv", header = T),
                            ifelse((Cohort == "TCRS"),
                                   caline <- bind_rows(read.csv(file="/Volumes/Lexar/TCRS/Data/LUR/CALINE3/Enroll/ExposureForAllIndividuals_F.csv", header = T, encoding = 'UTF-8'),
                                                       ifelse((Cohort == "GRID" & Prediction_Year==1980),
                                                              caline <- bind_rows(read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/GRID/Data/LUR/CALINE3/1980_1of2/ExposureForAllIndividuals_F.csv", header = T),
                                                                                  read.csv(file="/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/GRID/Data/LUR/CALINE3/1980_2of2/ExposureForAllIndividuals_F.csv", header = T)),
                                                              ifelse((Cohort == "TCRS"),
                                                                     read.csv(file="/Volumes/Lexar/TCRS/Data/LUR/CALINE3/ID1/ExposureForAllIndividuals_F.csv", header = T, encoding = 'UTF-8')),
                                   print("Dataset or CALINE3 year not found"))))))))

# Update CALINE output file
caline <- caline %>%
  filter(!is.na(CRSID)) %>%
  group_by(CRSID, Year) %>% # Average total PM2.5 by year
  summarise(caline_pm25 = mean(Total)) %>% 
  filter(Year == ifelse(eval(Prediction_Year)>=2010, 2010, eval(Prediction_Year))) # Filter for only those that match prediction year

# Merge to addrs, keeping only matching CALINE values
caline <- caline %>%
  mutate(hhid_x = ifelse(Cohort == 'PCWS', paste0(CRSID,"_A"), as.character(CRSID)))# create hhid_x merge field

if (Cohort=='TCRS'){
  caline <- caline %>%
    ungroup() %>%
    mutate(addrs_year = Year - 1900,
           styear = eval(Prediction_Year)) %>%
    inner_join(addrs, caline, by=c("CRSID"="hhid", "addrs_year"="styear")) %>%
    mutate(hhid_x = hhid_x.y) %>%
    dplyr::select(hhid_x, caline_pm25, styear) # Keep only hhid_x and CALINE predicted PM2.5 value
  
  write.csv(caline, "caline.csv", row.names = F)
  
} else {
  caline <- caline %>%
    ungroup() %>%
    inner_join(addrs, caline, by=c("hhid_x")) %>%
    mutate(styear = eval(Prediction_Year)) %>%
    dplyr::select(hhid_x, caline_pm25, styear) # Keep only hhid_x and CALINE predicted PM2.5 value
  
  write.csv(caline, "caline.csv", row.names = F)
  
}


#### Coordinate-based XY Predictors  ####

coords <- addrs %>%
  mutate(Xcoord = st_coordinates(addrs)[,1],
         Ycoord = st_coordinates(addrs)[,2],
         XplusY = Xcoord + Ycoord,
         XminusY = Xcoord - Ycoord,
         styear = eval(Prediction_Year)) %>%
  dplyr::select(hhid_x, Xcoord, Ycoord, XplusY, XminusY, styear)

st_geometry(coords) <- NULL

write.csv(coords, "coords.csv", row.names = F)



#### Time-Integrated NDVI (TIN) Predictor  ####
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

#### Maximum NDVI (MAXN) Predictor  ####
# Maximum NDVI value during the growing season.
intx_maxn <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(intx_area = st_area(intx),
             maxn_intx_area = maxn * intx_area) %>%
      group_by(hhid_x) %>%
      mutate(full_area = sum(intx_area),
             full_maxn_wtd = sum(maxn_intx_area),
             maxn_wtd = full_maxn_wtd/full_area)
    
    maxn <- data.frame(intx)
    
    maxn <- maxn %>% 
      distinct(hhid_x, maxn_wtd)
    
    names(maxn)[2] <- paste0("maxn_",as.character(abuffdists))
    
    maxn$styear <- Prediction_Year
    
    write.csv(maxn, paste0("maxn_",as.character(abuffdists),".csv"),row.names = F)
    
  } else {
    print("maxn_",as.character(abuffdists)," had no intersections")
  }
}

abuffdists <- c(5000, 1000, 500, 300, 100)
predictors <- list("maxn" = maxn)
mapply(FUN = intx_maxn, predictors, abuffdists)

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


#### Census Predictor ####
#Census population and households weighted to area
intx_census <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
  if (nrow(intx)>0) {
    intx <- intx %>%
      mutate(intx_area = st_area(intx), # this is doing just with people, not people/area
             hh_intx_area = HHOLD * intx_area,
             pop_intx_area = POP * intx_area) %>%
      mutate(intx_area = st_area(intx),
             hh_intx_area_dens = (HHOLD/Shape_Area) * (intx_area/Shape_Area), # this is using pop density
             pop_intx_area_dens = (POP/Shape_Area) * (intx_area/Shape_Area)) %>%
      group_by(hhid_x) %>%
      mutate(full_area = sum(intx_area),
             full_hh_wtd = sum(hh_intx_area),
             full_pop_wtd = sum(pop_intx_area),
             hh_wtd = full_hh_wtd/full_area,
             pop_wtd = full_pop_wtd/full_area)%>%
      mutate(hh_wtd_dens = sum(hh_intx_area_dens),
             pop_wtd_dens = sum(pop_intx_area_dens))
    
    hh <- data.frame(intx)
    
    hh <- hh %>% 
      distinct(hhid_x, hh_wtd, hh_wtd_dens)
    
    names(hh)[2] <- paste0("hh_",as.character(abuffdists))
    names(hh)[3] <- paste0("hh_dens_",as.character(abuffdists))
    
    hh$styear <- Prediction_Year
    
    write.csv(hh, paste0("hh_",as.character(abuffdists),".csv"),row.names = F)
    
    
    pop <- data.frame(intx)
    
    pop <- pop %>% 
      distinct(hhid_x, pop_wtd, pop_wtd_dens)
    
    names(pop)[2] <- paste0("pop_",as.character(abuffdists))
    names(pop)[3] <- paste0("pop_dens_",as.character(abuffdists))
    
    pop$styear <- Prediction_Year
    
    write.csv(pop, paste0("pop_",as.character(abuffdists),".csv"),row.names = F)
    
    
  } else {
    print("hhpop_",as.character(abuffdists)," had no intersections")
  }
}

abuffdists <- c(5000, 1000, 500, 300, 100)

predictors <- list("census" = census)

ifelse(Prediction_Year>1976 & Prediction_Year<1985, mcmapply(FUN = intx_census, predictors, abuffdists, mc.cores = 4),
       ifelse(Prediction_Year>1986 & Prediction_Year<1995, mcmapply(FUN = intx_census, predictors, abuffdists, mc.cores = 4),
              ifelse(Prediction_Year>1996 & Prediction_Year<2005, mcmapply(FUN = intx_census, predictors, abuffdists, mc.cores = 4), 
                     mcmapply(FUN = intx_census, predictors, abuffdists, mc.cores = 4))))


#### Land Use Predictor (1992, 2011, or 2016 NLCD years only!) ####

#Land uses from NLCD weighted to area
intx_lu_1992 <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  intx <- intx %>%
    mutate(Shape_Area = as.numeric(st_area(intx)) * 0.092903)
  
  intx$GRIDCODE<-as.character(intx$grid_code)
  
  intx$luc <- ifelse(intx$GRIDCODE=='21', "lr",
                     ifelse(intx$GRIDCODE=='22' |
                              intx$GRIDCODE=='23', "hr",
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
                                                                 intx$GRIDCODE=='33', "br", "xx"))))))
  
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

intx_lu_2016 <- function(p,abuffdists,i){
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), lu)
  intx <- intx %>%
    mutate(Shape_Area = as.numeric(st_area(intx)) * 0.092903)
  
  intx$luc <- ifelse(intx$gridcode=='22' | intx$gridcode=='23', "lr",
                     ifelse(intx$gridcode=='24', "hr",
                            ifelse(intx$gridcode=='21', "ug",
                                   ifelse(intx$gridcode=='11' |
                                            intx$gridcode=='12' |
                                            intx$gridcode=='41' |
                                            intx$gridcode=='42' |
                                            intx$gridcode=='43' |
                                            intx$gridcode=='51' |
                                            intx$gridcode=='52' |
                                            intx$gridcode=='71' |
                                            intx$gridcode=='90' |
                                            intx$gridcode=='95', "nt",
                                          ifelse(intx$gridcode=='81' |
                                                   intx$gridcode=='82', "ag", 
                                                 ifelse(intx$gridcode=='31', "br", "xx"))))))
  
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
ifelse(Prediction_Year<=1997, mcmapply(FUN = intx_lu_1992, predictors, abuffdists, mc.cores = 4),
       ifelse(Prediction_Year>1997 & Prediction_Year<=2003, mcmapply(FUN = intx_lu_2001, predictors, abuffdists, mc.cores = 4),
              ifelse(Prediction_Year>2003 & Prediction_Year<=2008, mcmapply(FUN = intx_lu_2006, predictors, abuffdists, mc.cores = 4),
                     ifelse(Prediction_Year>2008 & Prediction_Year<=2013, mcmapply(FUN = intx_lu_2011, predictors, abuffdists, mc.cores = 4),
                            ifelse(Prediction_Year>2013, mcmapply(FUN = intx_lu_2016, predictors, abuffdists, mc.cores = 4),
                                   print("Check your prediction year"))))))


