
#### Load packages as needed ####

packages <- c('Hmisc', 'corrplot', 'tidyverse', 'ggplot2')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


#### Import Pima DEQ Background Data ####
#if not done so, download daily mean for Children's Park station for period of interest from here: http://envista.pima.gov/
##delete summary rows at bottom and 1st and 3rd rows at top in excel so only describe headers remain

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data")

#read in PDEQ Children's Park data
pdeq <- read.csv("ChildrensPark.csv")

pdeq$no2ppb<-as.numeric(as.character(pdeq$NO2))
pdeq$noxppb<-as.numeric(as.character(pdeq$NOX))
pdeq$pm25ugm3<-as.numeric(as.character(pdeq$PM25HRNCore))

#pull out and format date fields to match P5 expo formatting
pdeq$stdate<-as.Date(pdeq$Date, "%m/%d/%Y")
pdeq$stmon<-as.numeric(format(pdeq$stdate, "%m"))
pdeq$stday<-as.numeric(format(pdeq$stdate, "%d"))
pdeq$styear<-as.numeric(format(pdeq$stdate, "%y"))

#### PDEQ Summary Stats ####
ozone_no2 <- lm(pdeq$Ozone ~ pdeq$no2ppb)
summary(ozone_no2)

ozone_nox <- lm(pdeq$Ozone ~ pdeq$noxppb)
summary(ozone_nox)

ozone_pm25 <- lm(pdeq$Ozone ~ pdeq$pm25ugm3)
summary(ozone_pm25)

ggplot(pdeq, aes(y=Ozone,x=as.factor(stmon))) +
  geom_boxplot() 

ggplot(pdeq, aes(y=no2ppb,x=as.factor(stmon))) +
  geom_boxplot() 
  



#### PDEQ Monthly Averages ####
#average by month
pdeq.pm25mavg<-aggregate(x=pdeq$pm25ugm3, by=list(pdeq$stmon,pdeq$styear), FUN=mean, na.rm=T)
pdeq.no2mavg<-aggregate(x=pdeq$no2ppb, by=list(pdeq$stmon,pdeq$styear), FUN=mean, na.rm=T)
pdeq.noxmavg<-aggregate(x=pdeq$noxppb, by=list(pdeq$stmon,pdeq$styear), FUN=mean, na.rm=T)


names(pdeq.pm25mavg)[1]<-"stmon"
names(pdeq.pm25mavg)[2]<-"styear"
names(pdeq.pm25mavg)[3]<-"pm25avg"

names(pdeq.no2mavg)[1]<-"stmon"
names(pdeq.no2mavg)[2]<-"styear"
names(pdeq.no2mavg)[3]<-"no2avg"

names(pdeq.noxmavg)[1]<-"stmon"
names(pdeq.noxmavg)[2]<-"styear"
names(pdeq.noxmavg)[3]<-"noxavg"

#### Import TAPS Data ####

#read in NOx and PM data


#NOTE: first export from TAPS access db the NOx and PM conc files using saved exports and overwrite files

nox <- read.csv("NOxConc.txt")
pm <- read.csv("PMConc.txt")

str(nox)
str(pm)

#Create hyphenated unique HHID and HHIDX name
nox$HHID_X <- paste(nox$HHID, nox$HHIDX, sep="_")
pm$HHID_X <- paste(pm$HHID, pm$HHIDX, sep="_")

#pull out those with fewer than 4 total Nox and NO2
#create frequency of participants
count <- plyr::count(nox,"HHID_X")

#drop those without 3 measurement periods
nox <- merge(count, nox, c("HHID_X"))
nox <- subset(nox, nox$freq>2)

#pull out those with fewer than 4 total Nox and NO2
#create frequency of participants
count <- plyr::count(pm,"HHID_X")

#drop those without 3 measurement periods
pm <- merge(count, pm, c("HHID_X"))
pm <- subset(pm, pm$freq>2)



#pull out and format date fields to match P5 expo formatting
nox$stdate<-as.Date(nox$StDate, "%m/%d/%Y")
nox$stmon<-as.numeric(format(nox$stdate, "%m"))
nox$stday<-as.numeric(format(nox$stdate, "%d"))
nox$styear<-as.numeric(format(nox$stdate, "%y"))

pm$stdate<-as.Date(pm$StDate, "%m/%d/%Y")
pm$stmon<-as.numeric(format(pm$stdate, "%m"))
pm$stday<-as.numeric(format(pm$stdate, "%d"))
pm$styear<-as.numeric(format(pm$stdate, "%y"))

#### Merge Children's Park TAPS and PDEQ Data ####

no2.cp<-subset(nox,nox$HHID=='XW84'& nox$Filter=='no2')
nox.cp<-subset(nox,nox$HHID=='XW84'& nox$Filter=='nox')
pm25.cp<-subset(pm,pm$HHID=='XW84'& pm$SizeFrac==2.5)
pm10.cp<-subset(pm,pm$HHID=='XW84'& pm$SizeFrac==10)

# # # Average by month - display only # # #
#average by month
no2.cp<-aggregate(x=no2.cp$Conc, by=list(no2.cp$stmon,no2.cp$styear,no2.cp$Filter), FUN=mean, na.rm=T)
nox.cp<-aggregate(x=nox.cp$Conc, by=list(nox.cp$stmon,nox.cp$styear,nox.cp$Filter), FUN=mean, na.rm=T)
pm25.cp<-aggregate(x=pm25.cp$PMConc, by=list(pm25.cp$stmon,pm25.cp$styear, pm25.cp$SizeFrac), FUN=mean, na.rm=T)
pm10.cp<-aggregate(x=pm10.cp$PMConc, by=list(pm10.cp$stmon,pm10.cp$styear, pm10.cp$SizeFrac), FUN=mean, na.rm=T)


names(no2.cp)[1]<-"stmon"
names(no2.cp)[2]<-"styear"
names(no2.cp)[3]<-"contam"
names(no2.cp)[4]<-"cp_no2"

names(nox.cp)[1]<-"stmon"
names(nox.cp)[2]<-"styear"
names(nox.cp)[3]<-"contam"
names(nox.cp)[4]<-"cp_nox"

names(pm25.cp)[1]<-"stmon"
names(pm25.cp)[2]<-"styear"
names(pm25.cp)[3]<-"sizefrac"
names(pm25.cp)[4]<-"cp_pm25"

names(pm10.cp)[1]<-"stmon"
names(pm10.cp)[2]<-"styear"
names(pm10.cp)[3]<-"sizefrac"
names(pm10.cp)[4]<-"cp_pm10"



data.no.cp<-merge(no2.cp,nox.cp, by=c("stmon","styear"), all=TRUE)
data.pm.cp<-merge(pm25.cp,pm10.cp, by=c("stmon","styear"), all=TRUE)
data.cp<-merge(data.no.cp,data.pm.cp, by=c("stmon","styear"), all=TRUE)

keep<-data.cp[,c(1:2,4,6,8,10)]
data.cp<-keep

data.pdeq.no<-merge(pdeq.no2mavg,pdeq.noxmavg, by=c("stmon","styear"), all=TRUE)
data.pdeq<-merge(data.pdeq.no,pdeq.pm25mavg, by=c("stmon","styear"), all=TRUE)

data.mntr<-merge(data.cp, data.pdeq, by=c("stmon","styear"), all=TRUE)

data.mntr <- na.omit(data.mntr)


#### Compare Pima DEQ vs TAPS at Childrens Park Data ####

datapm25.lm<-lm(data.mntr$cp_pm25~data.mntr$pm25avg)
summary(datapm25.lm)

datano2.lm<-lm(data.mntr$cp_no2~data.mntr$no2avg)
summary(datano2.lm)

datanox.lm<-lm(data.mntr$cp_nox~data.mntr$noxavg)
summary(datanox.lm)

data.corr<-cor(data.mntr[,c(3:6)],use="pairwise.complete.obs",method="pearson")
corrplot(data.corr, method="shade",addCoef.col="white",type="upper")
pairs(data.mntr[,c(3:9)])


#NO2
p1 <- ggplot(data.mntr, aes(cp_no2, no2avg, label=stmon)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  labs(x=expression(paste("TAPS Avg. ",NO[2]," Conc.(ppb)"))) +
  labs(y=expression(paste("PDEQ Avg. ",NO[2]," Conc.(ppb)"))) +
  labs(title=expression(paste("Monthly Avg. ",NO[2]," Conc. Comparison"))) +
  geom_text(aes(label=stmon),hjust=0, vjust=0)

p2 <- ggplot(data.mntr, aes(cp_nox, noxavg, label=stmon)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  labs(x=expression(paste("TAPS Avg. ",NO[x]," Conc.(ppb)"))) +
  labs(y=expression(paste("PDEQ Avg. ",NO[x]," Conc.(ppb)"))) +
  labs(title=expression(paste("Monthly Avg. ",NO[x]," Conc. Comparison"))) +
  geom_text(aes(label=stmon),hjust=0, vjust=0)

p3 <- ggplot(data.mntr, aes(cp_pm25, pm25avg, label=stmon)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  labs(x=expression(paste("TAPS Avg. ",PM[2.5]," Conc. (", mu, g/m^3,")"))) +
  labs(y=expression(paste("PDEQ Avg. ",PM[2.5]," Conc. (", mu, g/m^3,")"))) +
  labs(title=expression(paste("Monthly Avg. ",PM[2.5]," Conc. Comparison"))) +
  geom_text(aes(label=stmon),hjust=0, vjust=0)

gp1<- ggplot_gtable(ggplot_build(p1))
gp2<- ggplot_gtable(ggplot_build(p2))
gp3<- ggplot_gtable(ggplot_build(p3))

grid.arrange(gp1, gp2, gp3, ncol=2, nrow=2)
# #NOx
# plot(data.mntr$cp_nox, data.mntr$noxavg, main=expression(paste(Monthly NO[x], " Avg. Conc.")), 
#      xlab="PDEQ NOx Conc. (ppb)", ylab="TAPS NOx Conc. (ppb)", pch=19)
# abline(lm(data.mntr$noxavg~data.mntr$cp_nox), col="red") # regression line (y~x)
# 
# #PM2.5
# plot(data.mntr$cp_pm25, data.mntr$pm25avg, main="PM2.5 Conc. Comparison", 
#      xlab="PDEQ PM2.5 Conc. (ug/m^3)", ylab="TAPS PM2.5 Conc. (ug/m^3)", pch=19)
# abline(lm(data.mntr$pm25avg~data.mntr$cp_pm25), col="red") # regression line (y~x)

no2.box<-ap.data$no2_adj
boxplot(no2.box,main=expression(paste(NO[2], " Concentration")),
        ylab=expression(paste(NO[2], " Conc. (ppb)")))

nox.box<-ap.data$nox_adj
boxplot(nox.box,main=expression(paste(NO[x], " Concentration")),
        ylab=expression(paste(NO[x], " Conc. (ppb)")))

pm25.box<-ap.data$pm25_adj
boxplot(pm25.box,main=expression(paste(PM[2.5], " Concentration")),
        ylab=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")")))

pm10.box<-ap.data$pm10_adj
boxplot(pm10.box,main=expression(paste(PM[10], " Concentration")),
        ylab=expression(paste(PM[10], " Conc. (", mu, g/m^3,")")))

#### Subset CP and Homes of TAPS Concentrations ####
#temporal adjustment of samples using the TAPS continous site at Children's Park

# nox <- read.csv("NOxConc.txt")
# pm <- read.csv("PMConc.txt")

#extract Children's Park measurements only
no2.cp<-subset(nox,nox$HHID=='XW84'& nox$Filter=='no2' & nox$FieldBlank==0 & nox$Flag=="No Notes")
nox.cp<-subset(nox,nox$HHID=='XW84'& nox$Filter=='nox' & nox$FieldBlank==0 & nox$Flag=="No Notes")
pm25.cp<-subset(pm,pm$HHID=='XW84'& pm$SizeFrac==2.5 & pm$FieldBlank==0)
pm10.cp<-subset(pm,pm$HHID=='XW84'& pm$SizeFrac==10 & pm$FieldBlank==0)

#extract all other sample measurements only
no2.samp<-subset(nox,nox$HHID!='XW84'& nox$Filter=='no2' & nox$FieldBlank==0)
nox.samp<-subset(nox,nox$HHID!='XW84'& nox$Filter=='nox' & nox$FieldBlank==0)
pm25.samp<-subset(pm,pm$HHID!='XW84'& pm$SizeFrac==2.5 & pm$FieldBlank==0)
pm10.samp<-subset(pm,pm$HHID!='XW84'& pm$SizeFrac==10 & pm$FieldBlank==0)

#### Check NO2 vs NOx Concentrations ####
nox <- merge(nox.samp, no2.samp, by=c("HHID_X","SamplePeriod"))

ifelse(nox$Conc.x<nox$Conc.y,
       paste("Check NO2/NOx ratios! NO2>NOx for",nox$HHID_X, "in Sampling Period:",nox$SamplePeriod),
       "NO2/NOx ratios OK!")
#### Summary stats of all observations ####
no2.samp %>%
  filter(!is.na(Conc)) %>%
  summarise(n.obs=length(Conc),
            n.homes=n_distinct(HHID_X),
            mean=mean(Conc),
            geomean=exp(mean(log(Conc))),
            geosd=exp(sd(log(Conc))),
            min=min(Conc),
            max=max(Conc))

nox.samp %>%
  filter(!is.na(Conc)) %>%
  summarise(n.obs=length(Conc),
            n.homes=n_distinct(HHID_X),
            geomean=exp(mean(log(Conc))),
            geosd=exp(sd(log(Conc))),
            min=min(Conc),
            max=max(Conc))

pm25.samp %>%
  filter(!is.na(PMConc) & PMConc>0) %>%
  summarise(n.obs=length(PMConc),
            n.homes=n_distinct(HHID_X),
            geomean=exp(mean(log(PMConc))),
            geosd=exp(sd(log(PMConc))),
            min=min(PMConc),
            max=max(PMConc))

pm10.samp %>%
  filter(!is.na(PMConc) & PMConc>0) %>%
  summarise(n.obs=length(PMConc),
            n.homes=n_distinct(HHID_X),
            geomean=exp(mean(log(PMConc))),
            geosd=exp(sd(log(PMConc))),
            min=min(PMConc),
            max=max(PMConc))




#### Create full output file for mixed effects - no annual average or temporal changes ####


no2.samp.all <- no2.samp %>%
  select(HHID_X, SamplePeriod, Conc) %>%
  filter(!is.na(Conc))  %>%
  dplyr::group_by(HHID_X, SamplePeriod) %>%
  dplyr::summarise(no2 = mean(Conc))

nox.samp.all <- nox.samp %>%
  select(HHID_X, SamplePeriod, Conc) %>%
  filter(!is.na(Conc))  %>%
  dplyr::group_by(HHID_X, SamplePeriod) %>%
  dplyr::summarise(nox = mean(Conc))

pm25.samp.all <- pm25.samp %>%
  select(HHID_X, SamplePeriod, PMConc) %>%
  filter(!is.na(PMConc))  %>%
  dplyr::group_by(HHID_X, SamplePeriod) %>%
  dplyr::summarise(pm25 = mean(PMConc))

pm10.samp.all <- pm10.samp %>%
  select(HHID_X, SamplePeriod, PMConc) %>%
  filter(!is.na(PMConc))  %>%
  dplyr::group_by(HHID_X, SamplePeriod) %>%
  dplyr::summarise(pm10 = mean(PMConc))

samp.all.data<-Reduce(function(x,y) merge(x,y, all=TRUE, by = c("HHID_X", "SamplePeriod")),
                list(no2.samp.all,nox.samp.all,
                     pm25.samp.all,pm10.samp.all))

names(samp.all.data)[1] <- "hhid_x"
  
  
# Check if no2 is larger than nox for each sample period and home, and correct, as there is no issue found in lab processing
ifelse(samp.all.data$nox<samp.all.data$no2,
       paste("Check NO2/NOx ratios! NO2>NOx for ",samp.all.data$HHID_X, " in Sample Period ", samp.all.data$SamplePeriod, ". This will be corrected so that NOx = NO2."),
       "NO2/NOx ratios OK!")

samp.all.data$nox <- ifelse(samp.all.data$nox<samp.all.data$no2, samp.all.data$no2, samp.all.data$nox)

ifelse(samp.all.data$nox<samp.all.data$no2,
       paste("Check NO2/NOx ratios! NO2>NOx for ",samp.all.data$HHID_X, " in Sample Period ", samp.all.data$SamplePeriod, ". This will be corrected so that NOx = NO2."),
       "NO2/NOx ratios OK!")

#write table of homes with all individual air pollution data samples
setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/TAPS/Data/Results")
write.table(samp.all.data, "TAPSData_AllObs.csv", sep=",", row.names = FALSE)

#### Create Annual Averages ####

#do annual avg for study period
no2avg <- mean(no2.cp$Conc, na.rm=TRUE)
noxavg <- mean(nox.cp$Conc, na.rm=TRUE)
pm25avg <- mean(pm25.cp$PMConc, na.rm=TRUE)
pm10avg <- mean(pm10.cp$PMConc, na.rm=TRUE)

#find the avg dif correction factor for temporal correction by sampling period
no2.cp$avgdif<-no2.cp$Conc - no2avg
nox.cp$avgdif<-nox.cp$Conc - noxavg
pm25.cp$avgdif<-pm25.cp$PMConc - pm25avg
pm10.cp$avgdif<-pm10.cp$PMConc - pm10avg

#drop all other vars except sampling period and avg difference
keep<-no2.cp[,c("SamplePeriod","avgdif")]
no2.cp<-keep

keep<-nox.cp[,c("SamplePeriod","avgdif")]
nox.cp<-keep

keep<-pm25.cp[,c("SamplePeriod","avgdif")]
pm25.cp<-keep

keep<-pm10.cp[,c("SamplePeriod","avgdif")]
pm10.cp<-keep

#drop CP measures not in sampling periods


#subtract avg difference by sampling period for each site
no2.calc<-merge(no2.samp,no2.cp,by=c("SamplePeriod"), all=TRUE)
nox.calc<-merge(nox.samp,nox.cp,by=c("SamplePeriod"), all=TRUE)
pm25.calc<-merge(pm25.samp,pm25.cp,by=c("SamplePeriod"), all=TRUE)
pm10.calc<-merge(pm10.samp,pm10.cp,by=c("SamplePeriod"), all=TRUE)

no2.calc$no2_adj<-no2.calc$Conc - no2.calc$avgdif
no2.calc$no2_unadj<-no2.calc$Conc #note that unadjusted difference is the original concentration

nox.calc$nox_adj<-nox.calc$Conc - nox.calc$avgdif
nox.calc$nox_unadj<-nox.calc$Conc #note that unadjusted difference is the original concentration

pm25.calc$pm25_adj<-pm25.calc$PMConc - pm25.calc$avgdif
pm25.calc$pm25_unadj<-pm25.calc$PMConc #note that unadjusted difference is the original concentration

pm10.calc$pm10_adj<-pm10.calc$PMConc - pm10.calc$avgdif
pm10.calc$pm10_unadj<-pm10.calc$PMConc #note that unadjusted difference is the original concentration


#drop vars to cleanly merge by sampling period
keep<-no2.calc[,c("SamplePeriod","HHID","HHIDX","no2_adj","no2_unadj")]
no2.calc<-keep

keep<-nox.calc[,c("SamplePeriod","HHID","HHIDX","nox_adj","nox_unadj")]
nox.calc<-keep

keep<-pm25.calc[,c("SamplePeriod","HHID","HHIDX","pm25_adj","pm25_unadj")]
pm25.calc<-keep

keep<-pm10.calc[,c("SamplePeriod","HHID","HHIDX","pm10_adj","pm10_unadj")]
pm10.calc<-keep

#calculate annual averages
no2.adj<-aggregate(x=no2.calc$no2_adj, by=list(no2.calc$HHID, no2.calc$HHIDX), FUN=mean, na.rm=T)
names(no2.adj)[1]<-"HHID"
names(no2.adj)[2]<-"HHIDX"
names(no2.adj)[3]<-"no2_adj"


no2.unadj<-aggregate(x=no2.calc$no2_unadj, by=list(no2.calc$HHID), FUN=mean, na.rm=T)
names(no2.unadj)[1]<-"HHID"
names(no2.unadj)[2]<-"no2_unadj"

nox.adj<-aggregate(x=nox.calc$nox_adj, by=list(nox.calc$HHID), FUN=mean, na.rm=T)
names(nox.adj)[1]<-"HHID"
names(nox.adj)[2]<-"nox_adj"

nox.unadj<-aggregate(x=nox.calc$nox_unadj, by=list(nox.calc$HHID), FUN=mean, na.rm=T)
names(nox.unadj)[1]<-"HHID"
names(nox.unadj)[2]<-"nox_unadj"

pm25.adj<-aggregate(x=pm25.calc$pm25_adj, by=list(pm25.calc$HHID), FUN=mean, na.rm=T)
names(pm25.adj)[1]<-"HHID"
names(pm25.adj)[2]<-"pm25_adj"

pm25.unadj<-aggregate(x=pm25.calc$pm25_unadj, by=list(pm25.calc$HHID), FUN=mean, na.rm=T)
names(pm25.unadj)[1]<-"HHID"
names(pm25.unadj)[2]<-"pm25_unadj"

pm10.adj<-aggregate(x=pm10.calc$pm10_adj, by=list(pm10.calc$HHID), FUN=mean, na.rm=T)
names(pm10.adj)[1]<-"HHID"
names(pm10.adj)[2]<-"pm10_adj"

pm10.unadj<-aggregate(x=pm10.calc$pm10_unadj, by=list(pm10.calc$HHID), FUN=mean, na.rm=T)
names(pm10.unadj)[1]<-"HHID"
names(pm10.unadj)[2]<-"pm10_unadj"

#merge annual avgs by HHID

ap.data<-Reduce(function(x,y) merge(x,y, all=TRUE),
              list(no2.adj,no2.unadj,
                   nox.adj,nox.unadj,
                   pm25.adj,pm25.unadj,
                   pm10.adj,pm10.unadj
              ))

#replace values temporally corrected to ND with 0
#ap.data[ap.data<0]<-0
  
#### Summary stats of annual average observations ####
ap.data %>%
  filter(!is.na(no2_adj)) %>%
  summarise(NO2.n.obs=length(no2_adj),
            NO2.n.homes=n_distinct(HHID,HHIDX),
            NO2.mean=mean(no2_adj),
            NO2.median=median(no2_adj),
            NO2.geomean=exp(mean(log(no2_adj))),
            NO2.geosd=exp(sd(log(no2_adj))),
            NO2.min=min(no2_adj),
            NO2.max=max(no2_adj))

ap.data %>%
  filter(!is.na(nox_adj)) %>%
  summarise(NOx.n.obs=length(nox_adj),
            NOx.n.homes=n_distinct(HHID,HHIDX),
            NOx.mean=mean(nox_adj),
            NOx.median=median(nox_adj),
            #NOx.geomean=exp(mean(log(nox_adj))),
            #NOx.geosd=exp(sd(log(nox_adj))),
            NOx.min=min(nox_adj),
            NOx.max=max(nox_adj))

ap.data %>%
  filter(!is.na(pm25_adj)) %>%
  summarise(pm25.n.obs=length(pm25_adj),
            pm25.n.homes=n_distinct(HHID,HHIDX),
            pm25.mean=mean(pm25_adj),
            pm25.median=median(pm25_adj),
            #pm25.geomean=exp(mean(log(pm25_adj))),
            #pm25.geosd=exp(sd(log(pm25_adj))),
            pm25.min=min(pm25_adj),
            pm25.max=max(pm25_adj))

ap.data %>%
  filter(!is.na(pm10_adj)) %>%
  summarise(pm10.n.obs=length(pm10_adj),
            pm10.n.homes=n_distinct(HHID,HHIDX),
            pm10.mean=mean(pm10_adj),
            pm10.median=median(pm10_adj),
            #pm10.geomean=exp(mean(log(pm10_adj))),
            #pm10.geosd=exp(sd(log(pm10_adj))),
            pm10.min=min(pm10_adj),
            pm10.max=max(pm10_adj))

#### Check NO2 vs NOx Concentrations ####

ifelse(ap.data$nox_adj<ap.data$no2_adj,
       paste("Check NO2/NOx ratios! NO2>NOx for ",ap.data$HHID),
             "NO2/NOx ratios OK!")

#### Compare Adjusted and Unadjusted Concentrations ####

#correlate adjusted and unadjusted values
dev.off(dev.list()["RStudioGD"])
require(corrplot)
data.corr<-cor(ap.data[,c(2:9)],use="pairwise.complete.obs",method="pearson")
corrplot(data.corr, method="shade",addCoef.col="black")
pairs(ap.data)

no2.lm<-lm(ap.data$no2_adj~ap.data$no2_unadj)
summary(no2.lm)

nox.lm<-lm(ap.data$nox_adj~ap.data$nox_unadj)
summary(nox.lm)

pm25.lm<-lm(ap.data$pm25_adj~ap.data$pm25_unadj)
summary(pm25.lm)

pm10.lm<-lm(ap.data$pm10_adj~ap.data$pm10_unadj)
summary(pm10.lm)


#write table of homes with air pollution data
write.table(ap.data, "Results/TAPSData.csv"
            ,sep=","
            ,row.names = FALSE)

#box plots of pollutants
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2, 2))


no2.box<-ap.data$no2_adj
boxplot(no2.box,main=expression(paste(NO[2], " Concentration")),
        ylab=expression(paste(NO[2], " Conc. (ppb)")))

nox.box<-ap.data$nox_adj
boxplot(nox.box,main=expression(paste(NO[x], " Concentration")),
        ylab=expression(paste(NO[x], " Conc. (ppb)")))

pm25.box<-ap.data$pm25_adj
boxplot(pm25.box,main=expression(paste(PM[2.5], " Concentration")),
        ylab=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")")))

pm10.box<-ap.data$pm10_adj
boxplot(pm10.box,main=expression(paste(PM[10], " Concentration")),
        ylab=expression(paste(PM[10], " Conc. (", mu, g/m^3,")")))

write.table(ap.data, "TAPSFinalConc.csv", sep=",", row.names = F)


#### Create Participant Graphics ####

#for participant report back only, replace negative concentrations with 0
ap.data$no2_ptcpnt <- ifelse(ap.data$no2_adj>0, ap.data$no2_adj, 0)
ap.data$nox_ptcpnt <- ifelse(ap.data$nox_adj>0, ap.data$nox_adj, 0)
ap.data$pm25_ptcpnt <- ifelse(ap.data$pm25_adj>0, ap.data$pm25_adj, 0)
ap.data$pm10_ptcpnt <- ifelse(ap.data$pm10_adj>0, ap.data$pm10_adj, 0)



no2 <- ggplot(subset(ap.data, !is.na(no2_ptcpnt)),aes(x=no2_ptcpnt, y=reorder(HHID, no2_ptcpnt), label = signif(no2_ptcpnt, 3))) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.75) + 
  labs(x=expression(paste(NO[2]," Concentration (ppb)")),
       y="Household ID",
       title=expression(paste("Annual Average ",NO[2]," Concentration"))) +
  geom_vline(aes(xintercept = 53, linetype = "US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average)"),
             colour = "red", size = 0.75, show.legend = T) +
  scale_linetype_manual(name = "Guidelines:", labels = c("US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average)"),
                        values = c("US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average)" = 1)) +
  theme(text = element_text(size = 14))

ggsave(filename = "NO2_Participant.jpg", plot = no2, width = 8, height = 8, dpi = 300)

nox <- ggplot(subset(ap.data, !is.na(nox_ptcpnt)),aes(x=nox_ptcpnt, y=reorder(HHID, nox_ptcpnt), label = signif(nox_ptcpnt, 3))) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.25) + 
  labs(x=expression(paste(NO[x]," Concentration (ppb)")),
       y="Household ID",
       title=expression(paste("Annual Average ",NO[x]," Concentration"))) +
  theme(text = element_text(size = 14))

ggsave(filename = "NOx_Participant.jpg", plot = nox, width = 8, height = 8, dpi = 300)



pm25 <- ggplot(subset(ap.data, !is.na(pm25_ptcpnt)),aes(x=pm25_ptcpnt, y=reorder(HHID, pm25_ptcpnt), label = signif(pm25_ptcpnt, 3))) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.15) + 
  labs(x=expression(paste(PM[2.5], " Concentration (", mu, g/m^3,")")),
       y="Household ID",
       title=expression(paste("Annual Average ", PM[2.5], " Concentration"))) +
  geom_vline(aes(xintercept = 12, linetype = "US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average Over 3 Years)"),
             colour = "red", size = 0.75, show.legend = T) +
  scale_linetype_manual(name = "Guidelines:", labels = c("US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average Over 3 Years)"),
                        values = c("US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average Over 3 Years)" = 1)) +
  theme(text = element_text(size = 14))

ggsave(filename = "PM25_Participant.jpg", plot = pm25, width = 8, height = 8, dpi = 300)

pm10 <- ggplot(subset(ap.data, !is.na(pm10_ptcpnt)),aes(x=pm10_ptcpnt, y=reorder(HHID, pm10_ptcpnt), label = signif(pm10_ptcpnt, 3))) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 2) + 
  labs(x=expression(paste(PM[10], " Concentration (", mu, g/m^3,")")),
       y="Household ID",
       title=expression(paste("Annual Average ", PM[10], " Concentration"))) +
  geom_vline(aes(xintercept = 150, linetype = "US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average Over 3 Years)"),
             colour = "red", size = 0.75, show.legend = T) +
  scale_linetype_manual(name = "Guidelines:", labels = c("US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average Over 3 Years)"),
                        values = c("US EPA\nNational Ambient\nAir Quality Standard\n(Annual Average Over 3 Years)" = 1)) +
  theme(text = element_text(size = 14))

ggsave(filename = "PM10_Participant.jpg", plot = pm10, width = 8, height = 8, dpi = 300)
