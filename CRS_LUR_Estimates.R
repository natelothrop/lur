
#this code is meant to be run after successfully running the U:\P5\P5DataCleanup.do STATA do file, GIS predictors creation models in ArcGIS, and after P5_LUR_Development R script
library('reshape2')
library('rms')
library('plyr')
library('dplyr')
library('ggplot2')
library('Hmisc')


#read in CRS IDs and year of in depth
crs <- read.csv("N:\\CRS Address Hx\\Hx\\CRSAddressesbyID_121316.csv")
crs$hhid <- crs$firstid

#read in file of addresses in LUR area (ie Tucson metro in Pima Co. where roadway counts available)
enrollinarea <- read.csv("U:\\P5\\Data\\Enroll_inarea.csv")
id1inarea <- read.csv("U:\\P5\\Data\\ID1_inarea.csv")


#read and bind model predictors by data set and year

####TESTING NON FXN ONLY####
#Enroll 1980
##indepth <- 'Enroll'

##year <- 1980
#############################

P5_ESC_ENROLL_NO2 <- function(indepth,year){
  
  wd <- paste("U:\\P5\\Data\\",indepth,year,sep = "")
  
  setwd(wd)
  
  #BUS ROUTES DISTANCE TO
  filenames <- Sys.glob("BusrtNear*.txt")
  busrtnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  busrtnr<-busrtnr[keep]
  
  #rename remaining fields
  names(busrtnr)[1]<-"hhid"
  names(busrtnr)[2]<-"NRDistM"
  
  busrtnr$distintvbusrt1<-1/busrtnr$NRDistM
  busrtnr$distintvbusrt2<-(1/busrtnr$NRDistM)^2
  
  busrtnr$NRDistM<-NULL
  
  #BUS ROUTES LENGTH IN BUFFER
  filenames <- Sys.glob("BusrtLength*.txt")
  busrtl <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  busrtl<-busrtl[keep]
  
  #rename remaining fields
  names(busrtl)[1]<-"hhid"
  names(busrtl)[2]<-"BuffM"
  names(busrtl)[3]<-"ROADLENGTH"
  
  #RAILWAY DISTANCE TO
  filenames <- Sys.glob("RailNear*.txt")
  railnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railnr<-railnr[keep]
  
  #rename remaining fields
  names(railnr)[1]<-"hhid"
  names(railnr)[2]<-"NRDistM"
  
  railnr$distintvrail1<-1/railnr$NRDistM
  railnr$distintvrail2<-(1/railnr$NRDistM)^2
  
  railnr$NRDistM<-NULL
  
  #RAILWAY YARD DISTANCE TO
  filenames <- Sys.glob("RailYardNear*.txt")
  railyardnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railyardnr<-railyardnr[keep]
  
  #rename remaining fields
  names(railyardnr)[1]<-"hhid"
  names(railyardnr)[2]<-"NRDistM"
  
  railyardnr$distintvrailyard1<-1/railyardnr$NRDistM
  railyardnr$distintvrailyard2<-(1/railyardnr$NRDistM)^2
  
  railyardnr$NRDistM<-NULL
  
  #RAILWAYS LENGTH IN BUFFER
  filenames <- Sys.glob("RailLength*.txt")
  raill <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  raill<-raill[keep]
  
  #rename remaining fields
  names(raill)[1]<-"hhid"
  names(raill)[2]<-"BuffM"
  names(raill)[3]<-"ROADLENGTH"
  
  summary(is.na(raill))
  
  
  #AIRPORT DISTANCE TO
  filenames <- Sys.glob("AirNear*.txt")
  airnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  airnr<-airnr[keep]
  
  #rename remaining fields
  names(airnr)[1]<-"hhid"
  names(airnr)[2]<-"NRDistM"
  
  airnr$distintvair1<-1/airnr$NRDistM
  airnr$distintvair2<-(1/airnr$NRDistM)^2
  
  airnr$NRDistM<-NULL
  
  
  #MINES DISTANCE TO
  filenames <- Sys.glob("MineNear*.txt")
  minenr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  minenr<-minenr[keep]
  
  #rename remaining fields
  names(minenr)[1]<-"hhid"
  names(minenr)[2]<-"NRDistM"
  
  minenr$distintvmine1<-1/minenr$NRDistM
  minenr$distintvmine2<-(1/minenr$NRDistM)^2
  
  minenr$NRDistM<-NULL
  
  
  #ALL ROADS DISTANCE TO
  #read in predictors from GIS model script
  filenames <- Sys.glob("RoadNear*.txt")
  roadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  roadnr<-roadnr[keep]
  
  #rename remaining fields
  names(roadnr)[1]<-"hhid"
  names(roadnr)[2]<-"trafnear"
  names(roadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  roadnr$trafnear<-roadnr$trafnear * 1000
  
  #replaces any values that had no traffic counts with ESCAPE imputation value of 500 veh/day
  for (i in 1:length(roadnr[,3])){
    if (roadnr$trafnear[i]<0)
      roadnr$trafnear[i]<-500
    else
      roadnr$trafnear[i]<-roadnr$trafnear[i]
  } 
  
  roadnr$distinvnear1<-1/roadnr$NRDistM
  roadnr$distinvnear2<-(1/roadnr$NRDistM)^2
  roadnr$intinvnear1<-roadnr$trafnear * (1/roadnr$NRDistM)
  roadnr$intinvnear2<-roadnr$trafnear * ((1/roadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  roadnr$NRDistM <- NULL
  
  #MAJOR ROADS DISTANCE TO
  filenames <- Sys.glob("MRoadNear*.txt")
  mroadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  mroadnr<-mroadnr[keep]
  
  #rename remaining fields
  names(mroadnr)[1]<-"hhid"
  names(mroadnr)[2]<-"trafmajor"
  names(mroadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  mroadnr$trafmajor<-mroadnr$trafmajor * 1000
  
  mroadnr$distintvmajor1<-1/mroadnr$NRDistM
  mroadnr$distintvmajor2<-(1/mroadnr$NRDistM)^2
  mroadnr$intmajorinv1<-mroadnr$trafmajor * (1/mroadnr$NRDistM)
  mroadnr$intmajorinv2<-mroadnr$trafmajor * ((1/mroadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  mroadnr$NRDistM <- NULL
  
  
  
  #ALL ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("RoadLL*.txt")
  roadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  roadll<-roadll[keep]
  
  #rename remaining fields
  names(roadll)[1]<-"hhid"
  names(roadll)[2]<-"BuffM"
  names(roadll)[3]<-"ROADLENGTH"
  names(roadll)[4]<-"TRAFLOAD"
  
  
  #MAJOR ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("MRoadLL*.txt")
  mroadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  mroadll<-mroadll[keep]
  
  #rename remaining fields
  names(mroadll)[1]<-"hhid"
  names(mroadll)[2]<-"BuffM"
  names(mroadll)[3]<-"ROADLENGTH"
  names(mroadll)[4]<-"TRAFLOAD"
  
  
  #LAND USE AREA IN BUFFER
  #read in predictors from GIS model script
  filenames <- Sys.glob("LU*.txt")
  lu <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  lu<-lu[keep]
  
  #rename remaining fields
  names(lu)[1]<-"grid_code"
  names(lu)[2]<-"hhid"
  names(lu)[3]<-"BuffM"
  names(lu)[4]<-"Area"
  
  
  lu$grid_code<-as.numeric(as.character(lu$grid_code))
  # lu<-subset(lu,lu$grid_code==21 | 
  #              lu$grid_code==22 |
  #              lu$grid_code==23 |
  #              lu$grid_code==85 |
  #              (lu$grid_code>30 & lu$grid_code<79) 
  #              )
  #lu$lu<-"NULL"
  
  for (i in 1:length(lu[,1])){
    if (lu$grid_code[i]==21)
      lu$lu[i]<-"lr"
    if (lu$grid_code[i]==22)
      lu$lu[i]<-"hr"
    if (lu$grid_code[i]==23)
      lu$lu[i]<-"cm"
    if (lu$grid_code[i]==85)
      lu$lu[i]<-"ug"
    if (lu$grid_code[i]<=12 | (lu$grid_code[i]>=30 & lu$grid_code[i]<=60 & lu$grid_code[i]!=32) | lu$grid_code==70 | lu$grid_code==71 | lu$grid_code[i]>=90)
      lu$lu[i]<-"nt"
    if (lu$grid_code[i]==61 | (lu$grid_code[i]>=80 & lu$grid_code[i]<=84))
      lu$lu[i]<-"ag"
  }
  
  #sum land use code type by hhid, land use code, buffer distance
  lu.sum<-aggregate(x=lu$Area, by=list(lu$hhid,lu$lu,lu$BuffM), FUN=sum, na.rm=T)
  names(lu.sum)[1]<-"hhid"
  names(lu.sum)[2]<-"lu_code"
  names(lu.sum)[3]<-"BuffM"
  names(lu.sum)[4]<-"Area"
  
  #POP AND HOUSING DENSITY AREA IN BUFFER
  filenames <- Sys.glob("Census*.txt")
  census <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  census<-census[keep]
  
  #rename remaining fields
  names(census)[1]<-"hhid"
  names(census)[2]<-"BuffM"
  names(census)[3]<-"pop"
  names(census)[4]<-"hh"
  
  
  #ELEVATION
  filenames <- Sys.glob("Elev*.txt")
  elev <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  elev<-elev[keep]
  
  #rename remaining fields
  names(elev)[1]<-"hhid"
  names(elev)[2]<-"elev"
  
  #square root elevation
  elev$elev <- sqrt(elev$elev)
  
  #reshape data to wide form for regression
  require(reshape2)
  library(reshape2)
  
  lu<-dcast(lu.sum,hhid~lu_code+BuffM,value.var="Area")
  
  census.pop<-dcast(census,hhid~BuffM,value.var="pop")
  names(census.pop)[2]<-"pop_100"
  names(census.pop)[3]<-"pop_300"
  names(census.pop)[4]<-"pop_500"
  names(census.pop)[5]<-"pop_1000"
  names(census.pop)[6]<-"pop_5000"
  
  census.hh<-dcast(census,hhid~BuffM,value.var="hh")
  names(census.hh)[2]<-"hh_100"
  names(census.hh)[3]<-"hh_300"
  names(census.hh)[4]<-"hh_500"
  names(census.hh)[5]<-"hh_1000"
  names(census.hh)[6]<-"hh_5000"
  
  busrt.len<-dcast(busrtl,hhid~BuffM,value.var="ROADLENGTH")
  names(busrt.len)[2]<-"bl_25"
  names(busrt.len)[3]<-"bl_50"
  names(busrt.len)[4]<-"bl_100"
  names(busrt.len)[5]<-"bl_300"
  names(busrt.len)[6]<-"bl_500"
  names(busrt.len)[7]<-"bl_1000"
  
  rail.len<-dcast(raill,hhid~BuffM,value.var="ROADLENGTH")
  names(rail.len)[2]<-"ll_50"
  names(rail.len)[3]<-"ll_100"
  names(rail.len)[4]<-"ll_300"
  names(rail.len)[5]<-"ll_500"
  names(rail.len)[6]<-"ll_1000"
  
  road.len<-dcast(roadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.len)[2]<-"rl_25"
  names(road.len)[3]<-"rl_50"
  names(road.len)[4]<-"rl_100"
  names(road.len)[5]<-"rl_300"
  names(road.len)[6]<-"rl_500"
  names(road.len)[7]<-"rl_1000"
  
  road.tl<-dcast(roadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tl)[2]<-"tl_25"
  names(road.tl)[3]<-"tl_50"
  names(road.tl)[4]<-"tl_100"
  names(road.tl)[5]<-"tl_300"
  names(road.tl)[6]<-"tl_500"
  names(road.tl)[7]<-"tl_1000"
  
  road.lenm<-dcast(mroadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.lenm)[2]<-"rlm_25"
  names(road.lenm)[3]<-"rlm_50"
  names(road.lenm)[4]<-"rlm_100"
  names(road.lenm)[5]<-"rlm_300"
  names(road.lenm)[6]<-"rlm_500"
  names(road.lenm)[7]<-"rlm_1000"
  
  road.tlm<-dcast(mroadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tlm)[2]<-"tlm_25"
  names(road.tlm)[3]<-"tlm_50"
  names(road.tlm)[4]<-"tlm_100"
  names(road.tlm)[5]<-"tlm_300"
  names(road.tlm)[6]<-"tlm_500"
  names(road.tlm)[7]<-"tlm_1000"
  
  ####################################################
  #Merge all predictors together with CRS date info
  
  #use reduce function
  pdata<-Reduce(function(x,y) merge(x,y, all=TRUE),
                list(crs,
                     lu,
                     census.hh,
                     census.pop,
                     elev,
                     roadnr,
                     mroadnr,
                     road.tl,
                     road.tlm,
                     road.len,
                     road.lenm,
                     minenr,
                     airnr,
                     railnr,
                     railyardnr,
                     rail.len,
                     busrtnr,
                     busrt.len
                ))
  
  #drop predictors without elevation data
  pdata <- filter(pdata, !is.na(pdata$elev))
  
  #correct predictors by replacing NA to 0, starting with 11th column (land use info)
  pdata[, 11:ncol(pdata)][is.na(pdata[, 11:ncol(pdata)])] <- 0
  
  #replace homes right against mining with max predictor values from original 1987 model
  pdata$distintvmine1 <- ifelse( pdata$distintvmine1>2.952*10^-3,
                                 2.952*10^-3, 
                                 pdata$distintvmine1)
  
  pdata$distintvair1 <- ifelse( pdata$distintvair1>0.0013267244,
                                0.0013267244, 
                                pdata$distintvair1)
  
  pdata$distintvrail1 <- ifelse( pdata$distintvrail1>0.0076052224,
                                 0.0076052224, 
                                 pdata$distintvrail1)
  
  # # # # # # REGRESSIONS # # # # # # 
  
  #calculate NO2 exposure using best fitting ESCAPE model: logged outcome, >1 measurement
  pdata$no2 <- 6.621858 +
    (-0.1639089*pdata$elev) +
    ((4.596843 *10^-8) * pdata$cm_5000) +
    ((1.051332 * 10^-5) * pdata$rl_1000) +
    ((1.983009 * 10^-4) * pdata$rlm_300) +
    (482.2285 * pdata$distintvmine1)
  
  #updates from logged outcome
  pdata$no2 <- exp(pdata$no2)
  
  #calculate NO2 exposure using best fitting ESCAPE mixed effects model: logged outcome
  pdata$no2me <- 6.176692 +
    ((8.480870*10^-6)*pdata$pop_5000)+
    (-0.1619694 * pdata$elev) +
    (247.1642 * pdata$distintvair1) +
    (67.58739 * pdata$distintvrail1)
  
  #updates from logged outcome
  pdata$no2me <- exp(pdata$no2me)
  
  # # # # # # REGRESSIONS # # # # # # 
  
  
  #get rid of records not matching enrollment for designated analysis set year
  pdata$year <- year
  
  pdata <- filter(pdata,byr==year)
  
  #pulls out HHID and NO2 concentrations
  pdata <- select(pdata, hhid, ug_100, ug_300, ug_500, ug_1000, ug_5000, no2, no2me)
  
  #write output table
  write.table(pdata, file=paste("no2_",indepth,"_",year,".csv",sep=""),
              sep=",",
              row.names = FALSE)
  
  #renames dynamically to indepth and year as dataframe
  #assign(paste("no2",indepth,year,sep="_"), pdata)
}


P5_ESC_ENROLL_NO2('Enroll', 1980)
P5_ESC_ENROLL_NO2('Enroll', 1981)
P5_ESC_ENROLL_NO2('Enroll', 1982)
P5_ESC_ENROLL_NO2('Enroll', 1983)
P5_ESC_ENROLL_NO2('Enroll', 1984)

setwd("U:\\P5\\Data\\")

sources.files  <- list.files(path="U:\\P5\\Data",
                             recursive=T,
                             pattern='^no2_Enroll_*'
                             ,full.names=F)

## ou read all files with the id and bind them
dat <- do.call("rbind", lapply(sources.files, read.delim, header = T, sep=","))

dat <- merge(crs,dat,by=c("hhid"))

dat <- select(dat, FORMCRSID, ug_100, ug_300, ug_500, ug_1000, ug_5000, no2, no2me)

#flag values
dat$flag <- 0
dat$flagme <- 0

#flag as outlier if over 43, max value measured in P5 in 1987
dat$flag <- ifelse(dat$no2>43, -11, 0)
dat$flagme <- ifelse(dat$no2me>43, -11, 0)

dat$flag <- factor(dat$flag,
                   levels = c(0,-11,-99),
                   labels = c("None", "Prediction-Outlier","No Address"))
dat$flagme <- factor(dat$flagme,
                     levels = c(0,-11,-99),
                     labels = c("None", "Prediction-Outlier","No Address"))

indepth<-'Enroll'

#apply indepth label to no2 and flag values
names(dat)[names(dat)=="no2"] <- (paste(indepth, "_no2", sep=""))
names(dat)[names(dat)=="no2me"] <- (paste(indepth, "_no2me", sep=""))
names(dat)[names(dat)=="flag"] <- (paste(indepth, "_no2_flag", sep=""))
names(dat)[names(dat)=="flagme"] <- (paste(indepth, "_no2_flagme", sep=""))

#and apply indepth label to urban green measures
names(dat)[names(dat)=="ug_100"] <- (paste(indepth, "_ug_100", sep=""))
names(dat)[names(dat)=="ug_300"] <- (paste(indepth, "_ug_300", sep=""))
names(dat)[names(dat)=="ug_500"] <- (paste(indepth, "_ug_500", sep=""))
names(dat)[names(dat)=="ug_1000"] <- (paste(indepth, "_ug_1000", sep=""))
names(dat)[names(dat)=="ug_5000"] <- (paste(indepth, "_ug_5000", sep=""))

#merge in the addresses in LUR area, keeping only those that match (with prediction and in LUR area)
dat <- merge(dat,enrollinarea,by=c("FORMCRSID"))
dat$Enroll_inarea <- NULL

#create dataframe of results for merging with later address sets
no2_p5esc_Enroll <- dat

### write the file for the output
write.table(dat, file="no2_p5esc_Enroll.csv",
            sep=",",
            row.names = FALSE)

#summary stats
ggplot(dat, aes(y = Enroll_no2, x = Enroll_no2_flag)) +
  geom_boxplot() +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Flag")

ggplot(dat, aes(y = Enroll_no2me, x = Enroll_no2_flagme)) +
  geom_boxplot() +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Flag")

#########################################
#ID 1 outputs


P5_ESC_ID1_NO2 <- function(indepth,year){
  
  #indepth<-'ID1'
  #year<-1984
  
  wd <- paste("U:\\P5\\Data\\",indepth,year,sep = "")
  
  setwd(wd)
  
  #BUS ROUTES DISTANCE TO
  filenames <- Sys.glob("BusrtNear*.txt")
  busrtnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  busrtnr<-busrtnr[keep]
  
  #rename remaining fields
  names(busrtnr)[1]<-"hhid"
  names(busrtnr)[2]<-"NRDistM"
  
  busrtnr$distintvbusrt1<-1/busrtnr$NRDistM
  busrtnr$distintvbusrt2<-(1/busrtnr$NRDistM)^2
  
  busrtnr$NRDistM<-NULL
  
  #BUS ROUTES LENGTH IN BUFFER
  filenames <- Sys.glob("BusrtLength*.txt")
  busrtl <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  busrtl<-busrtl[keep]
  
  #rename remaining fields
  names(busrtl)[1]<-"hhid"
  names(busrtl)[2]<-"BuffM"
  names(busrtl)[3]<-"ROADLENGTH"
  
  #RAILWAY DISTANCE TO
  filenames <- Sys.glob("RailNear*.txt")
  railnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railnr<-railnr[keep]
  
  #rename remaining fields
  names(railnr)[1]<-"hhid"
  names(railnr)[2]<-"NRDistM"
  
  railnr$distintvrail1<-1/railnr$NRDistM
  railnr$distintvrail2<-(1/railnr$NRDistM)^2
  
  railnr$NRDistM<-NULL
  
  #RAILWAY YARD DISTANCE TO
  filenames <- Sys.glob("RailYardNear*.txt")
  railyardnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railyardnr<-railyardnr[keep]
  
  #rename remaining fields
  names(railyardnr)[1]<-"hhid"
  names(railyardnr)[2]<-"NRDistM"
  
  railyardnr$distintvrailyard1<-1/railyardnr$NRDistM
  railyardnr$distintvrailyard2<-(1/railyardnr$NRDistM)^2
  
  railyardnr$NRDistM<-NULL
  
  #RAILWAYS LENGTH IN BUFFER
  filenames <- Sys.glob("RailLength*.txt")
  raill <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  raill<-raill[keep]
  
  #rename remaining fields
  names(raill)[1]<-"hhid"
  names(raill)[2]<-"BuffM"
  names(raill)[3]<-"ROADLENGTH"
  
  summary(is.na(raill))
  
  
  #AIRPORT DISTANCE TO
  filenames <- Sys.glob("AirNear*.txt")
  airnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  airnr<-airnr[keep]
  
  #rename remaining fields
  names(airnr)[1]<-"hhid"
  names(airnr)[2]<-"NRDistM"
  
  airnr$distintvair1<-1/airnr$NRDistM
  airnr$distintvair2<-(1/airnr$NRDistM)^2
  
  airnr$NRDistM<-NULL
  
  
  #MINES DISTANCE TO
  filenames <- Sys.glob("MineNear*.txt")
  minenr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  minenr<-minenr[keep]
  
  #rename remaining fields
  names(minenr)[1]<-"hhid"
  names(minenr)[2]<-"NRDistM"
  
  minenr$distintvmine1<-1/minenr$NRDistM
  minenr$distintvmine2<-(1/minenr$NRDistM)^2
  
  minenr$NRDistM<-NULL
  
  
  #ALL ROADS DISTANCE TO
  #read in predictors from GIS model script
  filenames <- Sys.glob("RoadNear*.txt")
  roadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  roadnr<-roadnr[keep]
  
  #rename remaining fields
  names(roadnr)[1]<-"hhid"
  names(roadnr)[2]<-"trafnear"
  names(roadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  roadnr$trafnear<-roadnr$trafnear * 1000
  
  #replaces any values that had no traffic counts with ESCAPE imputation value of 500 veh/day
  for (i in 1:length(roadnr[,3])){
    if (roadnr$trafnear[i]<0)
      roadnr$trafnear[i]<-500
    else
      roadnr$trafnear[i]<-roadnr$trafnear[i]
  } 
  
  roadnr$distinvnear1<-1/roadnr$NRDistM
  roadnr$distinvnear2<-(1/roadnr$NRDistM)^2
  roadnr$intinvnear1<-roadnr$trafnear * (1/roadnr$NRDistM)
  roadnr$intinvnear2<-roadnr$trafnear * ((1/roadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  roadnr$NRDistM <- NULL
  
  #MAJOR ROADS DISTANCE TO
  filenames <- Sys.glob("MRoadNear*.txt")
  mroadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  mroadnr<-mroadnr[keep]
  
  #rename remaining fields
  names(mroadnr)[1]<-"hhid"
  names(mroadnr)[2]<-"trafmajor"
  names(mroadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  mroadnr$trafmajor<-mroadnr$trafmajor * 1000
  
  mroadnr$distintvmajor1<-1/mroadnr$NRDistM
  mroadnr$distintvmajor2<-(1/mroadnr$NRDistM)^2
  mroadnr$intmajorinv1<-mroadnr$trafmajor * (1/mroadnr$NRDistM)
  mroadnr$intmajorinv2<-mroadnr$trafmajor * ((1/mroadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  mroadnr$NRDistM <- NULL
  
  
  
  #ALL ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("RoadLL*.txt")
  roadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  roadll<-roadll[keep]
  
  #rename remaining fields
  names(roadll)[1]<-"hhid"
  names(roadll)[2]<-"BuffM"
  names(roadll)[3]<-"ROADLENGTH"
  names(roadll)[4]<-"TRAFLOAD"
  
  
  #MAJOR ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("MRoadLL*.txt")
  mroadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  mroadll<-mroadll[keep]
  
  #rename remaining fields
  names(mroadll)[1]<-"hhid"
  names(mroadll)[2]<-"BuffM"
  names(mroadll)[3]<-"ROADLENGTH"
  names(mroadll)[4]<-"TRAFLOAD"
  
  
  #LAND USE AREA IN BUFFER
  #read in predictors from GIS model script
  filenames <- Sys.glob("LU*.txt")
  lu <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  lu<-lu[keep]
  
  #rename remaining fields
  names(lu)[1]<-"grid_code"
  names(lu)[2]<-"hhid"
  names(lu)[3]<-"BuffM"
  names(lu)[4]<-"Area"
  
  
  lu$grid_code<-as.numeric(as.character(lu$grid_code))
  # lu<-subset(lu,lu$grid_code==21 | 
  #              lu$grid_code==22 |
  #              lu$grid_code==23 |
  #              lu$grid_code==85 |
  #              (lu$grid_code>30 & lu$grid_code<79) 
  #              )
  #lu$lu<-"NULL"
  
  for (i in 1:length(lu[,1])){
    if (lu$grid_code[i]==21)
      lu$lu[i]<-"lr"
    if (lu$grid_code[i]==22)
      lu$lu[i]<-"hr"
    if (lu$grid_code[i]==23)
      lu$lu[i]<-"cm"
    if (lu$grid_code[i]==85)
      lu$lu[i]<-"ug"
    if (lu$grid_code[i]<=12 | (lu$grid_code[i]>=30 & lu$grid_code[i]<=60 & lu$grid_code[i]!=32) | lu$grid_code==70 | lu$grid_code==71 | lu$grid_code[i]>=90)
      lu$lu[i]<-"nt"
    if (lu$grid_code[i]==61 | (lu$grid_code[i]>=80 & lu$grid_code[i]<=84))
      lu$lu[i]<-"ag"
  }
  
  #sum land use code type by hhid, land use code, buffer distance
  lu.sum<-aggregate(x=lu$Area, by=list(lu$hhid,lu$lu,lu$BuffM), FUN=sum, na.rm=T)
  names(lu.sum)[1]<-"hhid"
  names(lu.sum)[2]<-"lu_code"
  names(lu.sum)[3]<-"BuffM"
  names(lu.sum)[4]<-"Area"
  
  #POP AND HOUSING DENSITY AREA IN BUFFER
  filenames <- Sys.glob("Census*.txt")
  census <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  census<-census[keep]
  
  #rename remaining fields
  names(census)[1]<-"hhid"
  names(census)[2]<-"BuffM"
  names(census)[3]<-"pop"
  names(census)[4]<-"hh"
  
  
  #ELEVATION
  filenames <- Sys.glob("Elev*.txt")
  elev <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  elev<-elev[keep]
  
  #rename remaining fields
  names(elev)[1]<-"hhid"
  names(elev)[2]<-"elev"
  
  #square root elevation
  elev$elev <- sqrt(elev$elev)
  
  #reshape data to wide form for regression
  require(reshape2)
  library(reshape2)
  
  lu<-dcast(lu.sum,hhid~lu_code+BuffM,value.var="Area")
  
  census.pop<-dcast(census,hhid~BuffM,value.var="pop")
  names(census.pop)[2]<-"pop_100"
  names(census.pop)[3]<-"pop_300"
  names(census.pop)[4]<-"pop_500"
  names(census.pop)[5]<-"pop_1000"
  names(census.pop)[6]<-"pop_5000"
  
  census.hh<-dcast(census,hhid~BuffM,value.var="hh")
  names(census.hh)[2]<-"hh_100"
  names(census.hh)[3]<-"hh_300"
  names(census.hh)[4]<-"hh_500"
  names(census.hh)[5]<-"hh_1000"
  names(census.hh)[6]<-"hh_5000"
  
  busrt.len<-dcast(busrtl,hhid~BuffM,value.var="ROADLENGTH")
  names(busrt.len)[2]<-"bl_25"
  names(busrt.len)[3]<-"bl_50"
  names(busrt.len)[4]<-"bl_100"
  names(busrt.len)[5]<-"bl_300"
  names(busrt.len)[6]<-"bl_500"
  names(busrt.len)[7]<-"bl_1000"
  
  rail.len<-dcast(raill,hhid~BuffM,value.var="ROADLENGTH")
  names(rail.len)[2]<-"ll_50"
  names(rail.len)[3]<-"ll_100"
  names(rail.len)[4]<-"ll_300"
  names(rail.len)[5]<-"ll_500"
  names(rail.len)[6]<-"ll_1000"
  
  road.len<-dcast(roadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.len)[2]<-"rl_25"
  names(road.len)[3]<-"rl_50"
  names(road.len)[4]<-"rl_100"
  names(road.len)[5]<-"rl_300"
  names(road.len)[6]<-"rl_500"
  names(road.len)[7]<-"rl_1000"
  
  road.tl<-dcast(roadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tl)[2]<-"tl_25"
  names(road.tl)[3]<-"tl_50"
  names(road.tl)[4]<-"tl_100"
  names(road.tl)[5]<-"tl_300"
  names(road.tl)[6]<-"tl_500"
  names(road.tl)[7]<-"tl_1000"
  
  road.lenm<-dcast(mroadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.lenm)[2]<-"rlm_25"
  names(road.lenm)[3]<-"rlm_50"
  names(road.lenm)[4]<-"rlm_100"
  names(road.lenm)[5]<-"rlm_300"
  names(road.lenm)[6]<-"rlm_500"
  names(road.lenm)[7]<-"rlm_1000"
  
  road.tlm<-dcast(mroadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tlm)[2]<-"tlm_25"
  names(road.tlm)[3]<-"tlm_50"
  names(road.tlm)[4]<-"tlm_100"
  names(road.tlm)[5]<-"tlm_300"
  names(road.tlm)[6]<-"tlm_500"
  names(road.tlm)[7]<-"tlm_1000"
  
  ####################################################
  #Merge all predictors together with CRS date info
  
  #use reduce function
  pdata<-Reduce(function(x,y) merge(x,y, all=TRUE),
                list(crs,
                     lu,
                     census.hh,
                     census.pop,
                     elev,
                     roadnr,
                     mroadnr,
                     road.tl,
                     road.tlm,
                     road.len,
                     road.lenm,
                     minenr,
                     airnr,
                     railnr,
                     railyardnr,
                     rail.len,
                     busrtnr,
                     busrt.len
                ))
  
  #drop predictors without elevation data
  pdata <- filter(pdata, !is.na(pdata$elev))
  
  #correct predictors by replacing NA to 0, starting with 11th column (land use info)
  pdata[, 11:ncol(pdata)][is.na(pdata[, 11:ncol(pdata)])] <- 0
  
  #replace homes right against mining with max predictor values from original 1987 model
  pdata$distintvmine1 <- ifelse( pdata$distintvmine1>2.952*10^-3,
                                 2.952*10^-3, 
                                 pdata$distintvmine1)
  
  pdata$distintvair1 <- ifelse( pdata$distintvair1>0.0013267244,
                                0.0013267244, 
                                pdata$distintvair1)
  
  pdata$distintvrail1 <- ifelse( pdata$distintvrail1>0.0076052224,
                                 0.0076052224, 
                                 pdata$distintvrail1)
  
  # # # # # # REGRESSIONS # # # # # # 
  
  #calculate NO2 exposure using best fitting ESCAPE model: logged outcome, >1 measurement
  pdata$no2 <- 6.621858 +
    (-0.1639089*pdata$elev) +
    ((4.596843 *10^-8) * pdata$cm_5000) +
    ((1.051332 * 10^-5) * pdata$rl_1000) +
    ((1.983009 * 10^-4) * pdata$rlm_300) +
    (482.2285 * pdata$distintvmine1)
  
  #updates from logged outcome
  pdata$no2 <- exp(pdata$no2)
  
  #calculate NO2 exposure using best fitting ESCAPE mixed effects model: logged outcome
  pdata$no2me <- 6.176692 +
    ((8.480870*10^-6)*pdata$pop_5000)+
    (-0.1619694 * pdata$elev) +
    (247.1642 * pdata$distintvair1) +
    (67.58739 * pdata$distintvrail1)
  
  #updates from logged outcome
  pdata$no2me <- exp(pdata$no2me)
  
  # # # # # # REGRESSIONS # # # # # # 
  
  
  
  #get rid of records not matching enrollment for designated analysis set year
  pdata$year <- year
  
  pdata <- filter(pdata,id1yr==year)
  
  #pulls out HHID and NO2 concentration
  pdata <- select(pdata, hhid, ug_100, ug_300, ug_500, ug_1000, ug_5000, no2, no2me)
  
  #write output table
  write.table(pdata, file=paste("no2_",indepth,"_",year,".csv",sep=""),
              sep=",",
              row.names = FALSE)
  
  #renames dynamically to indepth and year as dataframe
  #assign(paste("no2",indepth,year,sep="_"), pdata)
}


P5_ESC_ID1_NO2('ID1', 1984)
P5_ESC_ID1_NO2('ID1', 1985)
P5_ESC_ID1_NO2('ID1', 1986)
P5_ESC_ID1_NO2('ID1', 1987)
P5_ESC_ID1_NO2('ID1', 1988)
P5_ESC_ID1_NO2('ID1', 1989)
P5_ESC_ID1_NO2('ID1', 1990)
P5_ESC_ID1_NO2('ID1', 1991)
P5_ESC_ID1_NO2('ID1', 1992)

setwd("U:\\P5\\Data\\")

indepth <- 'ID1'

sources.files  <- list.files(path="U:\\P5\\Data",
                             recursive=T,
                             pattern='^no2_ID1_*'
                             ,full.names=F)

## ou read all files with the id and bind them
dat <- do.call("rbind", lapply(sources.files, read.delim, header = T, sep=","))

dat <- merge(crs,dat,by=c("hhid"))

dat <- select(dat, FORMCRSID, ug_100, ug_300, ug_500, ug_1000, ug_5000, no2, no2me)

#flag values
dat$flag <- 0
dat$flagme <- 0

#flag as outlier if over 43, max value measured in P5 in 1987
dat$flag <- ifelse(dat$no2>43, -11, 0)
dat$flagme <- ifelse(dat$no2me>43, -11, 0)

dat$flag <- factor(dat$flag,
                   levels = c(0,-11,-99),
                   labels = c("None", "Prediction-Outlier","No Address"))
dat$flagme <- factor(dat$flagme,
                     levels = c(0,-11,-99),
                     labels = c("None", "Prediction-Outlier","No Address"))


#apply indepth label to no2 and flag values
names(dat)[names(dat)=="no2"] <- (paste(indepth, "_no2", sep=""))
names(dat)[names(dat)=="no2me"] <- (paste(indepth, "_no2me", sep=""))
names(dat)[names(dat)=="flag"] <- (paste(indepth, "_no2_flag", sep=""))
names(dat)[names(dat)=="flagme"] <- (paste(indepth, "_no2_flagme", sep=""))

#and apply indepth label to urban green measures
names(dat)[names(dat)=="ug_100"] <- (paste(indepth, "_ug_100", sep=""))
names(dat)[names(dat)=="ug_300"] <- (paste(indepth, "_ug_300", sep=""))
names(dat)[names(dat)=="ug_500"] <- (paste(indepth, "_ug_500", sep=""))
names(dat)[names(dat)=="ug_1000"] <- (paste(indepth, "_ug_1000", sep=""))
names(dat)[names(dat)=="ug_5000"] <- (paste(indepth, "_ug_5000", sep=""))



#merge in the addresses in LUR area, keeping only those that match (with prediction and in LUR area)
dat <- merge(dat,id1inarea,by=c("FORMCRSID"))
dat$ID1_inarea <- NULL

#create dataframe of results for merging with later address sets
no2_p5esc_ID1 <- dat

### write the file for the output
write.table(dat, file="no2_p5esc_ID1.csv",
            sep=",",
            row.names = FALSE)



#Summary stats
ggplot(dat, aes(x = ID1_no2)) +
  geom_histogram()

ggplot(dat, aes(y = ID1_no2, x = ID1_no2_flag)) +
  geom_boxplot() +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Flag")

ggplot(dat, aes(y = ID1_no2me, x = ID1_no2_flagme)) +
  geom_boxplot() +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Flag")





####################################
#merge all output files together

#use reduce function
no2_p5esc<-Reduce(function(x,y) merge(x,y, all=TRUE),
                  list(crs,
                       no2_p5esc_Enroll,
                       no2_p5esc_ID1
                  ))

no2_p5esc <- select(no2_p5esc,
                    FORMCRSID,
                    contains('_'))

#translate NaN to -999 for concentration for not applicable
no2_p5esc[is.na(no2_p5esc)] <- -99
# no2_p5esc$Enroll_no2 <- ifelse(is.na(no2_p5esc$Enroll_no2),-99,no2_p5esc$Enroll_no2)
# no2_p5esc$ID1_no2 <- ifelse(is.na(no2_p5esc$ID1_no2),-99,no2_p5esc$ID1_no2)
# no2_p5esc$Enroll_no2me <- ifelse(is.na(no2_p5esc$Enroll_no2me),-99,no2_p5esc$Enroll_no2me)
# no2_p5esc$ID1_no2me <- ifelse(is.na(no2_p5esc$ID1_no2me),-99,no2_p5esc$ID1_no2me)
# no2_p5esc$ID1_no2me <- ifelse(is.na(no2_p5esc$ID1_no2me),-99,no2_p5esc$ID1_no2me)



### write the file to csv
write.table(no2_p5esc, file="no2_p5esc.csv",
            sep=",",
            row.names = FALSE)


# setwd("U:\\P5\\Data\\")
# 
# multmerge = function(mypath){
#   filenames=list.files(path=mypath, full.names=TRUE)
#   datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
#   Reduce(function(x,y) {merge(x,y)}, datalist)
# 
# sources.files  <- list.files(path="U:\\P5\\Data",
#                              recursive=T,
#                              pattern='no2_p5esc_*'
#                              ,full.names=F)
# 
# datalist = lapply(sources.files, read.csv)
# 
# Reduce(function(x,y) {merge(x,y)}, datalist)
# 
# no2_p5esc <- do.call("merge", lapply(sources.files, read.delim, header = T, sep=","))
# 
# no2_p5esc <- merge(crs,dat,by=c("hhid"))


# # # # # # # # # # # # # # # #

#read and bind model predictors by data set and year

#Enroll 1980
##indepth <- 'Enroll'

##year <- 1980

P5_ESC_ENROLL_PM25 <- function(indepth,year){
  
  wd <- paste("U:\\P5\\Data\\",indepth,year,sep = "")
  
  setwd(wd)
  
  #BUS ROUTES DISTANCE TO
  filenames <- Sys.glob("BusrtNear*.txt")
  busrtnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  busrtnr<-busrtnr[keep]
  
  #rename remaining fields
  names(busrtnr)[1]<-"hhid"
  names(busrtnr)[2]<-"NRDistM"
  
  busrtnr$distintvbusrt1<-1/busrtnr$NRDistM
  busrtnr$distintvbusrt2<-(1/busrtnr$NRDistM)^2
  
  busrtnr$NRDistM<-NULL
  
  #BUS ROUTES LENGTH IN BUFFER
  filenames <- Sys.glob("BusrtLength*.txt")
  busrtl <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  busrtl<-busrtl[keep]
  
  #rename remaining fields
  names(busrtl)[1]<-"hhid"
  names(busrtl)[2]<-"BuffM"
  names(busrtl)[3]<-"ROADLENGTH"
  
  #RAILWAY DISTANCE TO
  filenames <- Sys.glob("RailNear*.txt")
  railnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railnr<-railnr[keep]
  
  #rename remaining fields
  names(railnr)[1]<-"hhid"
  names(railnr)[2]<-"NRDistM"
  
  railnr$distintvrail1<-1/railnr$NRDistM
  railnr$distintvrail2<-(1/railnr$NRDistM)^2
  
  railnr$NRDistM<-NULL
  
  #RAILWAY YARD DISTANCE TO
  filenames <- Sys.glob("RailYardNear*.txt")
  railyardnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railyardnr<-railyardnr[keep]
  
  #rename remaining fields
  names(railyardnr)[1]<-"hhid"
  names(railyardnr)[2]<-"NRDistM"
  
  railyardnr$distintvrailyard1<-1/railyardnr$NRDistM
  railyardnr$distintvrailyard2<-(1/railyardnr$NRDistM)^2
  
  railyardnr$NRDistM<-NULL
  
  #RAILWAYS LENGTH IN BUFFER
  filenames <- Sys.glob("RailLength*.txt")
  raill <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  raill<-raill[keep]
  
  #rename remaining fields
  names(raill)[1]<-"hhid"
  names(raill)[2]<-"BuffM"
  names(raill)[3]<-"ROADLENGTH"
  
  summary(is.na(raill))
  
  
  #AIRPORT DISTANCE TO
  filenames <- Sys.glob("AirNear*.txt")
  airnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  airnr<-airnr[keep]
  
  #rename remaining fields
  names(airnr)[1]<-"hhid"
  names(airnr)[2]<-"NRDistM"
  
  airnr$distintvair1<-1/airnr$NRDistM
  airnr$distintvair2<-(1/airnr$NRDistM)^2
  
  airnr$NRDistM<-NULL
  
  
  #MINES DISTANCE TO
  filenames <- Sys.glob("MineNear*.txt")
  minenr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  minenr<-minenr[keep]
  
  #rename remaining fields
  names(minenr)[1]<-"hhid"
  names(minenr)[2]<-"NRDistM"
  
  minenr$distintvmine1<-1/minenr$NRDistM
  minenr$distintvmine2<-(1/minenr$NRDistM)^2
  
  minenr$NRDistM<-NULL
  
  
  #ALL ROADS DISTANCE TO
  #read in predictors from GIS model script
  filenames <- Sys.glob("RoadNear*.txt")
  roadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  roadnr<-roadnr[keep]
  
  #rename remaining fields
  names(roadnr)[1]<-"hhid"
  names(roadnr)[2]<-"trafnear"
  names(roadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  roadnr$trafnear<-roadnr$trafnear * 1000
  
  #replaces any values that had no traffic counts with ESCAPE imputation value of 500 veh/day
  for (i in 1:length(roadnr[,3])){
    if (roadnr$trafnear[i]<0)
      roadnr$trafnear[i]<-500
    else
      roadnr$trafnear[i]<-roadnr$trafnear[i]
  } 
  
  roadnr$distinvnear1<-1/roadnr$NRDistM
  roadnr$distinvnear2<-(1/roadnr$NRDistM)^2
  roadnr$intinvnear1<-roadnr$trafnear * (1/roadnr$NRDistM)
  roadnr$intinvnear2<-roadnr$trafnear * ((1/roadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  roadnr$NRDistM <- NULL
  
  #MAJOR ROADS DISTANCE TO
  filenames <- Sys.glob("MRoadNear*.txt")
  mroadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  mroadnr<-mroadnr[keep]
  
  #rename remaining fields
  names(mroadnr)[1]<-"hhid"
  names(mroadnr)[2]<-"trafmajor"
  names(mroadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  mroadnr$trafmajor<-mroadnr$trafmajor * 1000
  
  mroadnr$distintvmajor1<-1/mroadnr$NRDistM
  mroadnr$distintvmajor2<-(1/mroadnr$NRDistM)^2
  mroadnr$intmajorinv1<-mroadnr$trafmajor * (1/mroadnr$NRDistM)
  mroadnr$intmajorinv2<-mroadnr$trafmajor * ((1/mroadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  mroadnr$NRDistM <- NULL
  
  
  
  #ALL ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("RoadLL*.txt")
  roadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  roadll<-roadll[keep]
  
  #rename remaining fields
  names(roadll)[1]<-"hhid"
  names(roadll)[2]<-"BuffM"
  names(roadll)[3]<-"ROADLENGTH"
  names(roadll)[4]<-"TRAFLOAD"
  
  
  #MAJOR ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("MRoadLL*.txt")
  mroadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  mroadll<-mroadll[keep]
  
  #rename remaining fields
  names(mroadll)[1]<-"hhid"
  names(mroadll)[2]<-"BuffM"
  names(mroadll)[3]<-"ROADLENGTH"
  names(mroadll)[4]<-"TRAFLOAD"
  
  
  #LAND USE AREA IN BUFFER
  #read in predictors from GIS model script
  filenames <- Sys.glob("LU*.txt")
  lu <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  lu<-lu[keep]
  
  #rename remaining fields
  names(lu)[1]<-"grid_code"
  names(lu)[2]<-"hhid"
  names(lu)[3]<-"BuffM"
  names(lu)[4]<-"Area"
  
  
  lu$grid_code<-as.numeric(as.character(lu$grid_code))
  # lu<-subset(lu,lu$grid_code==21 | 
  #              lu$grid_code==22 |
  #              lu$grid_code==23 |
  #              lu$grid_code==85 |
  #              (lu$grid_code>30 & lu$grid_code<79) 
  #              )
  #lu$lu<-"NULL"
  
  for (i in 1:length(lu[,1])){
    if (lu$grid_code[i]==21)
      lu$lu[i]<-"lr"
    if (lu$grid_code[i]==22)
      lu$lu[i]<-"hr"
    if (lu$grid_code[i]==23)
      lu$lu[i]<-"cm"
    if (lu$grid_code[i]==85)
      lu$lu[i]<-"ug"
    if (lu$grid_code[i]<=12 | (lu$grid_code[i]>=30 & lu$grid_code[i]<=60 & lu$grid_code[i]!=32) | lu$grid_code==70 | lu$grid_code==71 | lu$grid_code[i]>=90)
      lu$lu[i]<-"nt"
    if (lu$grid_code[i]==61 | (lu$grid_code[i]>=80 & lu$grid_code[i]<=84))
      lu$lu[i]<-"ag"
  }
  
  #sum land use code type by hhid, land use code, buffer distance
  lu.sum<-aggregate(x=lu$Area, by=list(lu$hhid,lu$lu,lu$BuffM), FUN=sum, na.rm=T)
  names(lu.sum)[1]<-"hhid"
  names(lu.sum)[2]<-"lu_code"
  names(lu.sum)[3]<-"BuffM"
  names(lu.sum)[4]<-"Area"
  
  #POP AND HOUSING DENSITY AREA IN BUFFER
  filenames <- Sys.glob("Census*.txt")
  census <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  census<-census[keep]
  
  #rename remaining fields
  names(census)[1]<-"hhid"
  names(census)[2]<-"BuffM"
  names(census)[3]<-"pop"
  names(census)[4]<-"hh"
  
  
  #ELEVATION
  filenames <- Sys.glob("Elev*.txt")
  elev <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  elev<-elev[keep]
  
  #rename remaining fields
  names(elev)[1]<-"hhid"
  names(elev)[2]<-"elev"
  
  #square root elevation
  elev$elev <- sqrt(elev$elev)
  
  #reshape data to wide form for regression
  require(reshape2)
  library(reshape2)
  
  lu<-dcast(lu.sum,hhid~lu_code+BuffM,value.var="Area")
  
  census.pop<-dcast(census,hhid~BuffM,value.var="pop")
  names(census.pop)[2]<-"pop_100"
  names(census.pop)[3]<-"pop_300"
  names(census.pop)[4]<-"pop_500"
  names(census.pop)[5]<-"pop_1000"
  names(census.pop)[6]<-"pop_5000"
  
  census.hh<-dcast(census,hhid~BuffM,value.var="hh")
  names(census.hh)[2]<-"hh_100"
  names(census.hh)[3]<-"hh_300"
  names(census.hh)[4]<-"hh_500"
  names(census.hh)[5]<-"hh_1000"
  names(census.hh)[6]<-"hh_5000"
  
  busrt.len<-dcast(busrtl,hhid~BuffM,value.var="ROADLENGTH")
  names(busrt.len)[2]<-"bl_25"
  names(busrt.len)[3]<-"bl_50"
  names(busrt.len)[4]<-"bl_100"
  names(busrt.len)[5]<-"bl_300"
  names(busrt.len)[6]<-"bl_500"
  names(busrt.len)[7]<-"bl_1000"
  
  rail.len<-dcast(raill,hhid~BuffM,value.var="ROADLENGTH")
  names(rail.len)[2]<-"ll_50"
  names(rail.len)[3]<-"ll_100"
  names(rail.len)[4]<-"ll_300"
  names(rail.len)[5]<-"ll_500"
  names(rail.len)[6]<-"ll_1000"
  
  road.len<-dcast(roadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.len)[2]<-"rl_25"
  names(road.len)[3]<-"rl_50"
  names(road.len)[4]<-"rl_100"
  names(road.len)[5]<-"rl_300"
  names(road.len)[6]<-"rl_500"
  names(road.len)[7]<-"rl_1000"
  
  road.tl<-dcast(roadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tl)[2]<-"tl_25"
  names(road.tl)[3]<-"tl_50"
  names(road.tl)[4]<-"tl_100"
  names(road.tl)[5]<-"tl_300"
  names(road.tl)[6]<-"tl_500"
  names(road.tl)[7]<-"tl_1000"
  
  road.lenm<-dcast(mroadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.lenm)[2]<-"rlm_25"
  names(road.lenm)[3]<-"rlm_50"
  names(road.lenm)[4]<-"rlm_100"
  names(road.lenm)[5]<-"rlm_300"
  names(road.lenm)[6]<-"rlm_500"
  names(road.lenm)[7]<-"rlm_1000"
  
  road.tlm<-dcast(mroadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tlm)[2]<-"tlm_25"
  names(road.tlm)[3]<-"tlm_50"
  names(road.tlm)[4]<-"tlm_100"
  names(road.tlm)[5]<-"tlm_300"
  names(road.tlm)[6]<-"tlm_500"
  names(road.tlm)[7]<-"tlm_1000"
  
  ####################################################
  #Merge all predictors together with CRS date info
  
  #use reduce function
  pdata<-Reduce(function(x,y) merge(x,y, all=TRUE),
                list(crs,
                     lu,
                     census.hh,
                     census.pop,
                     elev,
                     roadnr,
                     mroadnr,
                     road.tl,
                     road.tlm,
                     road.len,
                     road.lenm,
                     minenr,
                     airnr,
                     railnr,
                     railyardnr,
                     rail.len,
                     busrtnr,
                     busrt.len
                ))
  
  #drop predictors without elevation data
  pdata <- filter(pdata, !is.na(pdata$elev))
  
  #correct predictors by replacing NA to 0, starting with 11th column (land use info)
  pdata[, 11:ncol(pdata)][is.na(pdata[, 11:ncol(pdata)])] <- 0
  # # # # # # REGRESSIONS # # # # # # 
  
  #replace homes right against mining with max predictor values from original 1987 model
  pdata$distintvair2 <- ifelse( pdata$distintvair2>1.448279*10^-7,
                                1.448279*10^-7, 
                                pdata$distintvair2)
  
  #replace homes right against mining with max predictor values from original 1987 model
  pdata$distintvmajor1 <- ifelse( pdata$distintvmajor1>2.315988893,
                                  2.315988893,
                                  pdata$distintvmajor1)
  
  #calculate PM25 exposure using best fitting ESCAPE model: logged outcome, >1 measurement
  pdata$pm25 <- 5.256598 +
    ((9.226826*10^7)*pdata$distintvair2)
  
  #updates from logged outcome ifnee
  #pdata$pm25 <- pdata$pm25
  
  #calculate PM25 exposure using best fitting mixed effects  model
  pdata$pm25me <- 2.062737 +
    ((2.54510*10^6) * pdata$distintvair2) +
    (2.636611 * pdata$distintvmajor1)
  
  #updates from logged outcome
  pdata$pm25me <- exp(pdata$pm25me)
  
  # # # # # # REGRESSIONS # # # # # # 
  
  
  #get rid of records not matching enrollment for designated analysis set year
  pdata$year <- year
  
  pdata <- filter(pdata,byr==year)
  
  #pulls out HHID and PM25 concentration
  pdata <- select(pdata, hhid, pm25, pm25me)
  
  #write output table
  write.table(pdata, file=paste("pm25_",indepth,"_",year,".csv",sep=""),
              sep=",",
              row.names = FALSE)
  
  #renames dynamically to indepth and year as dataframe
  #assign(paste("pm25",indepth,year,sep="_"), pdata)
}


P5_ESC_ENROLL_PM25('Enroll', 1980)
P5_ESC_ENROLL_PM25('Enroll', 1981)
P5_ESC_ENROLL_PM25('Enroll', 1982)
P5_ESC_ENROLL_PM25('Enroll', 1983)
P5_ESC_ENROLL_PM25('Enroll', 1984)

setwd("U:\\P5\\Data\\")

sources.files  <- list.files(path="U:\\P5\\Data",
                             recursive=T,
                             pattern='^pm25_Enroll_*'
                             ,full.names=F)

## ou read all files with the id and bind them
dat <- do.call("rbind", lapply(sources.files, read.delim, header = T, sep=","))

dat <- merge(crs,dat,by=c("hhid"))

dat <- select(dat, FORMCRSID, pm25, pm25me)

#flag values
dat$flag <- 0
dat$flagme <- 0

#flag as outlier if over 54, max value measured in P5
dat$flag <- ifelse(dat$pm25>54, -11, 0)
dat$flagme <- ifelse(dat$pm25me>54, -11, 0)

dat$flag <- factor(dat$flag,
                   levels = c(0,-11,-99),
                   labels = c("None", "Prediction-Outlier","No Address"))

dat$flagme <- factor(dat$flagme,
                     levels = c(0,-11,-99),
                     labels = c("None", "Prediction-Outlier","No Address"))


indepth<-'Enroll'

#apply indepth label to pm25 and flag values
names(dat)[2] <- (paste(indepth, "_pm25", sep=""))
names(dat)[3] <- (paste(indepth, "_pm25me", sep=""))
names(dat)[4] <- (paste(indepth, "_pm25_flag", sep=""))
names(dat)[5] <- (paste(indepth, "_pm25_flagme", sep=""))

#merge in the addresses in LUR area, keeping only those that match (with prediction and in LUR area)
dat <- merge(dat,enrollinarea,by=c("FORMCRSID"))
dat$Enroll_inarea <- NULL

#create dataframe of results for merging with later address sets
pm25_p5esc_Enroll <- dat

### write the file for the output
write.table(dat, file="pm25_p5esc_Enroll.csv",
            sep=",",
            row.names = FALSE)

#summary stats
ggplot(dat, aes(y = Enroll_pm25, x = Enroll_pm25_flag)) +
  geom_boxplot() +
  labs(y=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")"))
       ,x="Flag")

ggplot(dat, aes(y = Enroll_pm25me, x = Enroll_pm25_flagme)) +
  geom_boxplot() +
  labs(y=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")"))
       ,x="Flag")

#########################################
#ID 1 outputs

P5_ESC_ID1_PM25 <- function(indepth,year){
  
  #indepth<-'ID1'
  #year<-1984
  
  wd <- paste("U:\\P5\\Data\\",indepth,year,sep = "")
  
  setwd(wd)
  
  #BUS ROUTES DISTANCE TO
  filenames <- Sys.glob("BusrtNear*.txt")
  busrtnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  busrtnr<-busrtnr[keep]
  
  #rename remaining fields
  names(busrtnr)[1]<-"hhid"
  names(busrtnr)[2]<-"NRDistM"
  
  busrtnr$distintvbusrt1<-1/busrtnr$NRDistM
  busrtnr$distintvbusrt2<-(1/busrtnr$NRDistM)^2
  
  busrtnr$NRDistM<-NULL
  
  #BUS ROUTES LENGTH IN BUFFER
  filenames <- Sys.glob("BusrtLength*.txt")
  busrtl <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  busrtl<-busrtl[keep]
  
  #rename remaining fields
  names(busrtl)[1]<-"hhid"
  names(busrtl)[2]<-"BuffM"
  names(busrtl)[3]<-"ROADLENGTH"
  
  #RAILWAY DISTANCE TO
  filenames <- Sys.glob("RailNear*.txt")
  railnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railnr<-railnr[keep]
  
  #rename remaining fields
  names(railnr)[1]<-"hhid"
  names(railnr)[2]<-"NRDistM"
  
  railnr$distintvrail1<-1/railnr$NRDistM
  railnr$distintvrail2<-(1/railnr$NRDistM)^2
  
  railnr$NRDistM<-NULL
  
  #RAILWAY YARD DISTANCE TO
  filenames <- Sys.glob("RailYardNear*.txt")
  railyardnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  railyardnr<-railyardnr[keep]
  
  #rename remaining fields
  names(railyardnr)[1]<-"hhid"
  names(railyardnr)[2]<-"NRDistM"
  
  railyardnr$distintvrailyard1<-1/railyardnr$NRDistM
  railyardnr$distintvrailyard2<-(1/railyardnr$NRDistM)^2
  
  railyardnr$NRDistM<-NULL
  
  #RAILWAYS LENGTH IN BUFFER
  filenames <- Sys.glob("RailLength*.txt")
  raill <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  raill<-raill[keep]
  
  #rename remaining fields
  names(raill)[1]<-"hhid"
  names(raill)[2]<-"BuffM"
  names(raill)[3]<-"ROADLENGTH"
  
  summary(is.na(raill))
  
  
  #AIRPORT DISTANCE TO
  filenames <- Sys.glob("AirNear*.txt")
  airnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  airnr<-airnr[keep]
  
  #rename remaining fields
  names(airnr)[1]<-"hhid"
  names(airnr)[2]<-"NRDistM"
  
  airnr$distintvair1<-1/airnr$NRDistM
  airnr$distintvair2<-(1/airnr$NRDistM)^2
  
  airnr$NRDistM<-NULL
  
  
  #MINES DISTANCE TO
  filenames <- Sys.glob("MineNear*.txt")
  minenr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  minenr<-minenr[keep]
  
  #rename remaining fields
  names(minenr)[1]<-"hhid"
  names(minenr)[2]<-"NRDistM"
  
  minenr$distintvmine1<-1/minenr$NRDistM
  minenr$distintvmine2<-(1/minenr$NRDistM)^2
  
  minenr$NRDistM<-NULL
  
  
  #ALL ROADS DISTANCE TO
  #read in predictors from GIS model script
  filenames <- Sys.glob("RoadNear*.txt")
  roadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  roadnr<-roadnr[keep]
  
  #rename remaining fields
  names(roadnr)[1]<-"hhid"
  names(roadnr)[2]<-"trafnear"
  names(roadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  roadnr$trafnear<-roadnr$trafnear * 1000
  
  #replaces any values that had no traffic counts with ESCAPE imputation value of 500 veh/day
  for (i in 1:length(roadnr[,3])){
    if (roadnr$trafnear[i]<0)
      roadnr$trafnear[i]<-500
    else
      roadnr$trafnear[i]<-roadnr$trafnear[i]
  } 
  
  roadnr$distinvnear1<-1/roadnr$NRDistM
  roadnr$distinvnear2<-(1/roadnr$NRDistM)^2
  roadnr$intinvnear1<-roadnr$trafnear * (1/roadnr$NRDistM)
  roadnr$intinvnear2<-roadnr$trafnear * ((1/roadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  roadnr$NRDistM <- NULL
  
  #MAJOR ROADS DISTANCE TO
  filenames <- Sys.glob("MRoadNear*.txt")
  mroadnr <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:5)
  mroadnr<-mroadnr[keep]
  
  #rename remaining fields
  names(mroadnr)[1]<-"hhid"
  names(mroadnr)[2]<-"trafmajor"
  names(mroadnr)[3]<-"NRDistM"
  
  #transforms traffic counts into true values
  mroadnr$trafmajor<-mroadnr$trafmajor * 1000
  
  mroadnr$distintvmajor1<-1/mroadnr$NRDistM
  mroadnr$distintvmajor2<-(1/mroadnr$NRDistM)^2
  mroadnr$intmajorinv1<-mroadnr$trafmajor * (1/mroadnr$NRDistM)
  mroadnr$intmajorinv2<-mroadnr$trafmajor * ((1/mroadnr$NRDistM)^2)
  
  #drop the distance variable (not a regression predictor)
  mroadnr$NRDistM <- NULL
  
  
  
  #ALL ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("RoadLL*.txt")
  roadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  roadll<-roadll[keep]
  
  #rename remaining fields
  names(roadll)[1]<-"hhid"
  names(roadll)[2]<-"BuffM"
  names(roadll)[3]<-"ROADLENGTH"
  names(roadll)[4]<-"TRAFLOAD"
  
  
  #MAJOR ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
  filenames <- Sys.glob("MRoadLL*.txt")
  mroadll <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  mroadll<-mroadll[keep]
  
  #rename remaining fields
  names(mroadll)[1]<-"hhid"
  names(mroadll)[2]<-"BuffM"
  names(mroadll)[3]<-"ROADLENGTH"
  names(mroadll)[4]<-"TRAFLOAD"
  
  
  #LAND USE AREA IN BUFFER
  #read in predictors from GIS model script
  filenames <- Sys.glob("LU*.txt")
  lu <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  lu<-lu[keep]
  
  #rename remaining fields
  names(lu)[1]<-"grid_code"
  names(lu)[2]<-"hhid"
  names(lu)[3]<-"BuffM"
  names(lu)[4]<-"Area"
  
  
  lu$grid_code<-as.numeric(as.character(lu$grid_code))
  # lu<-subset(lu,lu$grid_code==21 | 
  #              lu$grid_code==22 |
  #              lu$grid_code==23 |
  #              lu$grid_code==85 |
  #              (lu$grid_code>30 & lu$grid_code<79) 
  #              )
  #lu$lu<-"NULL"
  
  for (i in 1:length(lu[,1])){
    if (lu$grid_code[i]==21)
      lu$lu[i]<-"lr"
    if (lu$grid_code[i]==22)
      lu$lu[i]<-"hr"
    if (lu$grid_code[i]==23)
      lu$lu[i]<-"cm"
    if (lu$grid_code[i]==85)
      lu$lu[i]<-"ug"
    if (lu$grid_code[i]<=12 | (lu$grid_code[i]>=30 & lu$grid_code[i]<=60 & lu$grid_code[i]!=32) | lu$grid_code==70 | lu$grid_code==71 | lu$grid_code[i]>=90)
      lu$lu[i]<-"nt"
    if (lu$grid_code[i]==61 | (lu$grid_code[i]>=80 & lu$grid_code[i]<=84))
      lu$lu[i]<-"ag"
  }
  
  #sum land use code type by hhid, land use code, buffer distance
  lu.sum<-aggregate(x=lu$Area, by=list(lu$hhid,lu$lu,lu$BuffM), FUN=sum, na.rm=T)
  names(lu.sum)[1]<-"hhid"
  names(lu.sum)[2]<-"lu_code"
  names(lu.sum)[3]<-"BuffM"
  names(lu.sum)[4]<-"Area"
  
  #POP AND HOUSING DENSITY AREA IN BUFFER
  filenames <- Sys.glob("Census*.txt")
  census <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:6)
  census<-census[keep]
  
  #rename remaining fields
  names(census)[1]<-"hhid"
  names(census)[2]<-"BuffM"
  names(census)[3]<-"pop"
  names(census)[4]<-"hh"
  
  
  #ELEVATION
  filenames <- Sys.glob("Elev*.txt")
  elev <- do.call("rbind", lapply(filenames, read.delim, header = F, sep=" "))
  
  # keep HHID and dist to fields
  keep<-c(3:4)
  elev<-elev[keep]
  
  #rename remaining fields
  names(elev)[1]<-"hhid"
  names(elev)[2]<-"elev"
  
  #square root elevation
  elev$elev <- sqrt(elev$elev)
  
  #reshape data to wide form for regression
  require(reshape2)
  library(reshape2)
  
  lu<-dcast(lu.sum,hhid~lu_code+BuffM,value.var="Area")
  
  census.pop<-dcast(census,hhid~BuffM,value.var="pop")
  names(census.pop)[2]<-"pop_100"
  names(census.pop)[3]<-"pop_300"
  names(census.pop)[4]<-"pop_500"
  names(census.pop)[5]<-"pop_1000"
  names(census.pop)[6]<-"pop_5000"
  
  census.hh<-dcast(census,hhid~BuffM,value.var="hh")
  names(census.hh)[2]<-"hh_100"
  names(census.hh)[3]<-"hh_300"
  names(census.hh)[4]<-"hh_500"
  names(census.hh)[5]<-"hh_1000"
  names(census.hh)[6]<-"hh_5000"
  
  busrt.len<-dcast(busrtl,hhid~BuffM,value.var="ROADLENGTH")
  names(busrt.len)[2]<-"bl_25"
  names(busrt.len)[3]<-"bl_50"
  names(busrt.len)[4]<-"bl_100"
  names(busrt.len)[5]<-"bl_300"
  names(busrt.len)[6]<-"bl_500"
  names(busrt.len)[7]<-"bl_1000"
  
  rail.len<-dcast(raill,hhid~BuffM,value.var="ROADLENGTH")
  names(rail.len)[2]<-"ll_50"
  names(rail.len)[3]<-"ll_100"
  names(rail.len)[4]<-"ll_300"
  names(rail.len)[5]<-"ll_500"
  names(rail.len)[6]<-"ll_1000"
  
  road.len<-dcast(roadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.len)[2]<-"rl_25"
  names(road.len)[3]<-"rl_50"
  names(road.len)[4]<-"rl_100"
  names(road.len)[5]<-"rl_300"
  names(road.len)[6]<-"rl_500"
  names(road.len)[7]<-"rl_1000"
  
  road.tl<-dcast(roadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tl)[2]<-"tl_25"
  names(road.tl)[3]<-"tl_50"
  names(road.tl)[4]<-"tl_100"
  names(road.tl)[5]<-"tl_300"
  names(road.tl)[6]<-"tl_500"
  names(road.tl)[7]<-"tl_1000"
  
  road.lenm<-dcast(mroadll,hhid~BuffM,value.var="ROADLENGTH")
  names(road.lenm)[2]<-"rlm_25"
  names(road.lenm)[3]<-"rlm_50"
  names(road.lenm)[4]<-"rlm_100"
  names(road.lenm)[5]<-"rlm_300"
  names(road.lenm)[6]<-"rlm_500"
  names(road.lenm)[7]<-"rlm_1000"
  
  road.tlm<-dcast(mroadll,hhid~BuffM,value.var="TRAFLOAD")
  names(road.tlm)[2]<-"tlm_25"
  names(road.tlm)[3]<-"tlm_50"
  names(road.tlm)[4]<-"tlm_100"
  names(road.tlm)[5]<-"tlm_300"
  names(road.tlm)[6]<-"tlm_500"
  names(road.tlm)[7]<-"tlm_1000"
  
  ####################################################
  #Merge all predictors together with CRS date info
  
  #use reduce function
  pdata<-Reduce(function(x,y) merge(x,y, all=TRUE),
                list(crs,
                     lu,
                     census.hh,
                     census.pop,
                     elev,
                     roadnr,
                     mroadnr,
                     road.tl,
                     road.tlm,
                     road.len,
                     road.lenm,
                     minenr,
                     airnr,
                     railnr,
                     railyardnr,
                     rail.len,
                     busrtnr,
                     busrt.len
                ))
  
  #drop predictors without elevation data
  pdata <- filter(pdata, !is.na(pdata$elev))
  
  #correct predictors by replacing NA to 0, starting with 11th column (land use info)
  pdata[, 11:ncol(pdata)][is.na(pdata[, 11:ncol(pdata)])] <- 0
  
  # # # # # # REGRESSIONS # # # # # # 
  
  #replace homes right against mining with max predictor values from original 1987 model
  pdata$distintvair2 <- ifelse( pdata$distintvair2>1.448279*10^-7,
                                1.448279*10^-7, 
                                pdata$distintvair2)
  
  #replace homes right against mining with max predictor values from original 1987 model
  pdata$distintvmajor1 <- ifelse( pdata$distintvmajor1>2.315988893,
                                  2.315988893, 
                                  pdata$distintvmajor1)
  
  #calculate PM25 exposure using best fitting ESCAPE model: logged outcome, >1 measurement
  pdata$pm25 <- 5.256598 +
    ((9.226826*10^7)*pdata$distintvair2)
  
  #updates from logged outcome ifnee
  #pdata$pm25 <- pdata$pm25
  
  #calculate PM25 exposure using best fitting mixed effects  model
  pdata$pm25me <- 2.062737 +
    ((2.54510*10^6)*pdata$distintvair2) +
    (2.636611 * pdata$distintvmajor1)
  
  #updates from logged outcome
  pdata$pm25me <- exp(pdata$pm25me)
  
  # # # # # # REGRESSIONS # # # # # # 
  
  #get rid of records not matching enrollment for designated analysis set year
  pdata$year <- year
  
  pdata <- filter(pdata,id1yr==year)
  
  #pulls out HHID and PM25 concentration
  pdata <- select(pdata, hhid, pm25, pm25me)
  
  #write output table
  write.table(pdata, file=paste("pm25_",indepth,"_",year,".csv",sep=""),
              sep=",",
              row.names = FALSE)
  
  #renames dynamically to indepth and year as dataframe
  #assign(paste("pm25",indepth,year,sep="_"), pdata)
}


P5_ESC_ID1_PM25('ID1', 1984)
P5_ESC_ID1_PM25('ID1', 1985)
P5_ESC_ID1_PM25('ID1', 1986)
P5_ESC_ID1_PM25('ID1', 1987)
P5_ESC_ID1_PM25('ID1', 1988)
P5_ESC_ID1_PM25('ID1', 1989)
P5_ESC_ID1_PM25('ID1', 1990)
P5_ESC_ID1_PM25('ID1', 1991)
P5_ESC_ID1_PM25('ID1', 1992)

setwd("U:\\P5\\Data\\")

indepth <- 'ID1'

sources.files  <- list.files(path="U:\\P5\\Data",
                             recursive=T,
                             pattern='^pm25_ID1_*'
                             ,full.names=F)

## ou read all files with the id and bind them
dat <- do.call("rbind", lapply(sources.files, read.delim, header = T, sep=","))

dat <- merge(crs,dat,by=c("hhid"))

dat <- select(dat, FORMCRSID, pm25, pm25me)


#flag values
dat$flag <- 0
dat$flagme <- 0

#flag as outlier if over 54, max value measured in P5
dat$flag <- ifelse(dat$pm25>54, -11, 0)
dat$flagme <- ifelse(dat$pm25me>54, -11, 0)

dat$flag <- factor(dat$flag,
                   levels = c(0,-11,-99),
                   labels = c("None", "Prediction-Outlier","No Address"))

dat$flagme <- factor(dat$flagme,
                     levels = c(0,-11,-99),
                     labels = c("None", "Prediction-Outlier","No Address"))


#apply indepth label to pm25 and flag values
names(dat)[2] <- (paste(indepth, "_pm25", sep=""))
names(dat)[3] <- (paste(indepth, "_pm25me", sep=""))
names(dat)[4] <- (paste(indepth, "_pm25_flag", sep=""))
names(dat)[5] <- (paste(indepth, "_pm25_flagme", sep=""))

#merge in the addresses in LUR area, keeping only those that match (with prediction and in LUR area)
dat <- merge(dat,id1inarea,by=c("FORMCRSID"))
dat$ID1_inarea <- NULL

#create dataframe of results for merging with later address sets
pm25_p5esc_ID1 <- dat

### write the file for the output
write.table(dat, file="pm25_p5esc_ID1.csv",
            sep=",",
            row.names = FALSE)



#Summary stats
ggplot(dat, aes(x = ID1_pm25)) +
  geom_histogram()

ggplot(dat, aes(y = ID1_pm25, x = ID1_pm25_flag)) +
  geom_boxplot() +
  labs(y=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")"))
       ,x="Flag")

ggplot(dat, aes(y = ID1_pm25me, x = ID1_pm25_flagme)) +
  geom_boxplot() +
  labs(y=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")"))
       ,x="Flag")

####################################
#merge all output files together

#use reduce function
pm25_p5esc<-Reduce(function(x,y) merge(x,y, all=TRUE),
                   list(crs,
                        pm25_p5esc_Enroll,
                        pm25_p5esc_ID1
                   ))

pm25_p5esc <- select(pm25_p5esc,
                     FORMCRSID,
                     Enroll_pm25,
                     Enroll_pm25me,
                     Enroll_pm25_flag,
                     Enroll_pm25_flagme,
                     ID1_pm25,
                     ID1_pm25me,
                     ID1_pm25_flag,
                     ID1_pm25_flagme)

#translate NaN to -999 for concentration for not applicable
# pm25_p5esc$Enroll_pm25 <- ifelse(is.na(pm25_p5esc$Enroll_pm25),-99,pm25_p5esc$Enroll_pm25)
# pm25_p5esc$ID1_pm25 <- ifelse(is.na(pm25_p5esc$ID1_pm25),-99,pm25_p5esc$ID1_pm25)
pm_p5esc[is.na(pm_p5esc)] <- -99



### write the file to csv
write.table(pm25_p5esc, file="pm25_p5esc.csv",
            sep=",",
            row.names = FALSE)

######################################

no2_pm25_p5esc<-Reduce(function(x,y) merge(x,y, all=TRUE),
                       list(crs,
                            no2_p5esc,
                            pm25_p5esc
                       ))

no2_pm25_p5esc <- select(no2_pm25_p5esc,
                         FORMCRSID,
                         Enroll_no2,
                         Enroll_no2me,
                         Enroll_no2_flag,
                         Enroll_no2_flagme,
                         ID1_no2,
                         ID1_no2me,
                         ID1_no2_flag,
                         ID1_no2_flagme,
                         Enroll_pm25,
                         Enroll_pm25me,
                         Enroll_pm25_flag,
                         Enroll_pm25_flagme,
                         ID1_pm25,
                         ID1_pm25me,
                         ID1_pm25_flag,
                         ID1_pm25_flagme)

### write the file to csv
write.table(no2_pm25_p5esc, file="no2_pm25_p5esc.csv",
            sep=",",
            row.names = FALSE)
