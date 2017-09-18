#This script is to create spatial analysis variables for a point set using ESCAPE methods found in their study manuals

# Clear environment of temporary files
rm(list=ls())

#### Loading Packages ####

packages <- c('devtools', 'tidyverse', 'caret', 'car', 'raster', 'leaflet', 'leaflet.minicharts', 'htmltools','rgdal', 'sp', 'sf', 'methods')

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






#### Load, transform if need, predictor rasters and shapfiles ####

#Rasters
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Rasters")
elev_rast <- raster("dem.tif")

#Shapefiles
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
air <- st_read("Airrunway_1.shp", stringsAsFactors = F)
busrt <- st_read("busroute_1.shp", stringsAsFactors = F)
census <- st_read("Census2010_1.shp", stringsAsFactors = F)
mines <- st_read("mines_1.shp", stringsAsFactors = F)
rail <- st_read("railroad_1.shp", stringsAsFactors = F)
railyard <- st_read("railroad_yardcentroid_1.shp", stringsAsFactors = F)
roads <- st_read("Roads_v20170728.shp", stringsAsFactors = F)
lu <- st_read("nlcd2011_pimaclippolygon.shp", stringsAsFactors = F)

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

crs_raster <- "+proj=tmerc +lat_0=31 +lon_0=-111.9166666666667 +k=0.9999 +x_0=213360 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=ft +no_defs"


st_transform(addrs, crs = 2868)
st_transform(air, crs = 2868)
st_transform(busrt, crs = 2868)
st_transform(census, crs = 2868)
st_transform(mines, crs = 2868)
st_transform(rail, crs = 2868)
st_transform(railyard, crs = 2868)
st_transform(roads, crs = 2868)
st_transform(lu, crs = 2868)

elev_rast <- projectRaster(elev_rast, crs=crs_raster)


#define major roads as >5000 vehicles/day as per ESCAPE
mroads <- subset(roads, roads$VD15>5000)


#### Elevation raster at point ####

addrs_spdf <- readOGR("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles", layer="Sites")
addrs_spdf$hhid_x <- paste(addrs_spdf$HHID, addrs_spdf$HHIDX, sep="_") #make unique address ID

proj4string(addrs_spdf) <- CRS("+init=epsg:2868")                   # set original projection
addrs_spdf <- spTransform(addrs_spdf, CRS("+init=epsg:2868"))

elev_df <- extract(x = elev_rast, y = addrs_spdf, method = 'bilinear', df = F)
addrsdf <- as.data.frame(addrs_spdf)
elev <- cbind(addrsdf, elev_df)
elev$elev <- sqrt(elev$elev_df)
elev <- subset(elev, select = c(hhid_x, elev))
#names(elev)[1] <- "hhid"

elev <- filter(elev, !is.na(elev))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

write.csv(elev, "elev.csv",row.names = F)




#### Polygon areal predictors (census, land use) ####

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



abuffdists <- c(100, 300, 500, 1000, 5000)

#Census population and households weighted to area
intx_census <- function(p,abuffdists,i){
  setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
  intx <- st_intersection(st_buffer(addrs, dist = 3.28084*abuffdists), p)
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




#### Nearest line sources with vehicle loading (roads, major roads) ####

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")

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


#### Nearest line sources without vehicle loading (bus routes, rail lines) ####


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
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/CALINE3/2010")
caline <- read.csv("ExposureForAllIndividuals.csv")

caline$hhid_x <- caline$CRSID

caline <- caline %>%
  group_by(hhid_x) %>%
  summarize(cal3_pm25 = mean(Total))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Predictors")
write.csv(caline, "caline.csv", row.names = F)


#### Create coordinate point predictors ####

addrs$Xcoord <- st_coordinates(addrs)[,1]
addrs$Ycoord <- st_coordinates(addrs)[,2]

addrs$XplusY <- addrs$Xcoord + addrs$Ycoord
addrs$XminusY <- addrs$Xcoord - addrs$Ycoord


#### Combine all predictors ####
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
lurdata[, 12:ncol(lurdata)][is.na(lurdata[, 12:ncol(lurdata)])] <- 0

#correct outcomes by replacing 0 to NA, starting with 11th column (land use info)
lurdata[, 1:11][lurdata[, 1:11]==0] <- NA





#### Data Cleaning and LUR Testing ####

#drop the "A" addresses which have too few sampling periods (>2) to be used in LUR
lurdata <- subset(lurdata, lurdata$hhid_x != "QC84_A") #dropped as this was only measured once without GPS coords
lurdata <- subset(lurdata, lurdata$hhid_x != "SM47_A") #dropped as this was only measured once without GPS coords
lurdata <- subset(lurdata, lurdata$hhid_x != "WF34_A") #dropped as this is almost <25m to intersection(NE corner Tucson Blvd, Arroy Chico), thus excluding it


#univariate regressions to find starting variable for NO2
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_no2adj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$no2_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for NOx
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_noxadj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$nox_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM2.5
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_pm25adj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$pm25_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM10
sink("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Results/taps_lur_pm10adj.txt")#saves output to text file
lapply( lurdata[,-1], function(x) summary(lm(lurdata$pm10_adj ~ x)) )
sink()#stops diverting output

#### LUR Analysis Based on ESCAPE Protocol - NO2 ####

no2adj <- (lm(no2_adj~lu_hr_1000 +
                #distintvmine1 +
                distintvrail1 +
                elev +
                #lu_nt_100 +
                roads_rl_1000
              ,data=lurdata))
summary(no2adj)

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
                     ,data=lurdata, trControl=train_control, method="lm")
# summarize results
print(model_loocv)




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

keep<-c(2:3)
no2_r<-no2_r[,keep]

par(mfrow = c(2, 2))
plot(no2adj, labels.id = lurdata$hhid_x)


#### LUR Analysis Based on ESCAPE Protocol - NOx ####

noxadj <- (lm(nox_adj~
                lu_hr_1000 +
                distintvmine1 +
                #distintvrail1 + #NOTE - removed due to reduced significance after ZD62 removal
                elev +
                #lu_hr_5000 +
                #lu_nt_100 +
                #mroads_tl_50 +
                roads_rl_100 +
                roads_rl_1000
              ,data=subset(lurdata, lurdata$hhid_x != "ZD62_A"))) # NOTE - home removed due to Cook's D over 1
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
                       lu_hr_1000 +
                       distintvmine1 +
                       distintvrail1 +
                       elev +
                       roads_rl_100 +
                       roads_rl_1000
                     ,data=lurdata, trControl=train_control, method="lm")
# summarize results
print(model_loocv)




#Hold-out Validation
set.seed(1)
in_train <- createDataPartition(lurdata$nox_adj, p = 2/3, list = FALSE)
training <- lurdata[ in_train,]
testing  <- lurdata[-in_train,]

nrow(lurdata)
nrow(training)
nrow(testing)

model_hov <- train(nox_adj~lu_hr_1000 +
                     distintvrail1 +
                     elev +
                     roads_rl_1000
                   , data = training, method = "lm")
print(model_hov)




fitnox<-summary(noxadj) #final model

#pull out residuals
attributes(fitnox)
fitnox.r<-fitnox$residuals

nox_r<-merge(fitnox.r,lurdata, by=c("row.names"), all=T)
names(nox_r)[2]<-"nox_r"
nox_r <- dplyr::select(nox_r, hhid_x, nox_r)

keep<-c(2:3)
nox_r<-nox_r[,keep]

par(mfrow = c(2, 2))
plot(noxadj, labels.id = lurdata$hhid_x)



#### LUR Analysis Based on ESCAPE Protocol - PM2.5 ####

pm25adj <- (lm(pm25_adj~
                 #elev +
                 busrt_l_300 +
                 #distintvair1 +
                 Xcoord
               
               ,data=lurdata))
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
model_loocv <- train(pm25_adj~busrt_l_300 +
                       Xcoord
                     ,data=filter(lurdata, !is.na(pm25_adj)), trControl=train_control, method="lm")
# summarize results
print(model_loocv)




#Hold-out Validation
set.seed(1)
in_train <- createDataPartition(lurdata$pm25_adj, p = 2/3, list = FALSE)
training <- lurdata[ in_train,]
testing  <- lurdata[-in_train,]

nrow(lurdata)
nrow(training)
nrow(testing)

model_hov <- train(pm25_adj~busrt_l_300 +
                     Xcoord
                   , data = training, method = "lm")
print(model_hov)




fitpm25<-summary(pm25adj) #final model

#pull out residuals
attributes(fitpm25)
fitpm25.r<-fitpm25$residuals

pm25_r<-merge(fitpm25.r,lurdata, by=c("row.names"), all=T)
names(pm25_r)[2]<-"pm25_r"
pm25_r <- dplyr::select(pm25_r, hhid_x, pm25_r)

keep<-c(2:3)
pm25_r<-pm25_r[,keep]

par(mfrow = c(2, 2))
plot(pm25adj, labels.id = lurdata$hhid_x)

#### LUR Analysis Based on ESCAPE Protocol - PM10 ####

pm10adj <- (lm(pm10_adj~
                 lu_hr_500 +
                 #distintvair1 +
                 #distintvmine1 +
                 #distintvrail1 +
                 lu_nt_100 +
                 #lu_ug_5000 +
                 #roads_tl_500 +
                 XplusY
               
               ,data=lurdata))
summary(pm10adj)

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




#Hold-out Validation
set.seed(1)
in_train <- createDataPartition(lurdata$pm10_adj, p = 2/3, list = FALSE)
training <- lurdata[ in_train,]
testing  <- lurdata[-in_train,]

nrow(lurdata)
nrow(training)
nrow(testing)

model_hov <- train(pm10_adj~
                     lu_hr_500 +
                     lu_nt_100 +
                     XplusY
                   , data = training, method = "lm")
print(model_hov)




fitpm10<-summary(pm10adj) #final model

#pull out residuals
attributes(fitpm10)
fitpm10.r<-fitpm10$residuals

pm10_r<-merge(fitpm10.r,lurdata, by=c("row.names"), all=T)
names(pm10_r)[2]<-"pm10_r"
pm10_r <- dplyr::select(pm10_r, hhid_x, pm10_r)

keep<-c(2:3)
pm10_r<-pm10_r[,keep]

par(mfrow = c(2, 2))
plot(pm10adj, labels.id = lurdata$hhid_x)


#### Create Residual Output ####
#Load in results and addresses again
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
results <- read.csv("TAPSdata.csv")

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/LUR/Shapefiles")
addrs <- st_read("Sites.shp", stringsAsFactors = F)
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID
addrs <- subset(addrs, addrs$hhid_x != "QF44_A") #dropped as this was only measured once without GPS coords

addrs <- subset(addrs, select = c(HHID, HHIDX, hhid_x))
addrs$hhid_x <- paste(addrs$HHID, addrs$HHIDX, sep="_") #make unique address ID

addrs <- left_join(addrs,results, by = c("HHID", "HHIDX"), all.x=T)

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

st_write(resids, "TAPS_Residuals.shp")

resids.df <- resids

write.csv(resids.df, "TAPS_Residuals.csv", row.names = F)

resids.spdf <- as(resids, "Spatial")

resids.spdf <- spTransform(resids.spdf, CRS("+proj=longlat +datum=WGS84"))

resids.spdf$long <- resids.spdf@coords[,1]
resids.spdf$lat <- resids.spdf@coords[,2]

resids.df <- data.frame(resids.spdf)

# qpal_no2 <- colorQuantile("Reds", resids.df$no2_adj, n = 5)
# 
# leaflet(data = resids.df) %>% addTiles() %>%
#   addCircleMarkers(lat = ~lat, lng = ~long,
#              popup = ~hhid_x,
#              color = ~qpal_no2(no2_adj),
#              stroke = T, fillOpacity = 0.75
#              )

map <- leaflet(data = resids.df) %>% addTiles()

pal_no2 <- colorNumeric("Reds", resids.df$no2_adj, n = 5)

map %>%
  addCircleMarkers(stroke = T, fillOpacity = 0.75,
              color = ~pal_no2(no2_adj),
              label = ~hhid_x,
              popup = paste("NO2:", format(round(resids.spdf$no2_adj, 2), nsmall = 2), "<br>",
                            "NO2 Resid:", format(round(resids.spdf$no2_r, 2), nsmall = 2), "<br>")) %>%
  addLegend(pal = pal_no2,
            values = ~no2_adj,
            title = "NO2 Concentration (ppb)",
            opacity = 0.75) 

# addMinicharts(
#   resids.df$long, resids.df$lat,
#   type = "pie",
#   chartdata = resids.df[, c("no2_adj", "nox_adj")], 
#   colorPalette = colors, 
#   width = 2 * (resids.df$no2_adj + resids.df$nox_adj), transitionTime = 0,
#   showLabels = T) %>%

  # # Layers control
  # addLayersControl(
  #   baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  #   overlayGroups = c("Quakes", "Outline"),
  #   options = layersControlOptions(collapsed = FALSE)
  # )



