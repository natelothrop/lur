
#this code is meant to be run after successfully running the U:\P5\P5DataCleanup.do STATA do file AND after running

#set output window device to the plot pane
options(device="RStudioGD")

#### Read in packages ####

library(reshape2)
library(GGally)
library(rms)
library(caret)
library(Rcpp)
library(leaps)
library(Hmisc)
library(MASS)
library(lme4)
library(tidyverse)

#### Summary stats/graphs ####
#read in all p5 data
p5all<- read.csv("~/Dropbox/P5_TAPS_TEMP/P5_Master.csv")

#subset out the monitor data
monitor<-subset(p5all,hhid>5001)
data<-subset(p5all,hhid<5001)
data87<-subset(data,data$styear==87)

str(data)

#descriptive graphs
data$freq <- factor(data$freq)
data$styear <- factor(data$styear)
data$stssn <- factor(data$stssn)
data$stmon <- factor(data$stmon)



keeps<-c("hhid","stmon","styear","no2ppb", "pm25ugm3","pm10ugm3")
data<-data[keeps]

data$stssn <- ifelse((data$stmon==3 | data$stmon==4 | data$stmon==9 | data$stmon==10), 2,
                     ifelse((data$stmon==1 | data$stmon==2 | data$stmon==11 | data$stmon==12), 1, 3))
                               
data$stssn <- factor(data$stssn,
                    levels = c(1,2,3),
                    labels = c("Winter", "Intermed.", "Summer"))

datano2 <- subset(data,!is.na(no2ppb))
datapm25 <- subset(data,!is.na(pm25ugm3))
datapm10 <- subset(data,!is.na(pm10ugm3))


#tables of year/season/pollutant
summarise(group_by(datano2, styear, stssn),
          n=length(no2ppb), mean=mean(no2ppb), sd=sd(no2ppb))

summarise(group_by(datapm25, styear, stssn),
          n=length(pm25ugm3), mean=mean(pm25ugm3), sd=sd(pm25ugm3))

summarise(group_by(datapm10, styear, stssn),
          n=length(pm10ugm3), mean=mean(pm10ugm3), sd=sd(pm10ugm3))

#create frequency of measurement by year and season
#no2
freqno2 <- count(datano2,"hhid")
describe(freqno2)
datano2 <- merge(datano2, freqno2, c("hhid"))
table(datano2$freq,datano2$styear)
table(datano2$freq,datano2$stssn)


#pm25
freq25 <- count(datapm25,"hhid")
describe(freq25)
datapm25 <- merge(datapm25, freq25, c("hhid"))
table(datapm25$freq,datapm25$styear)
table(datapm25$freq,datapm25$stssn)


#pm10
freq10 <- count(datapm10,"hhid")
describe(freq10)
datapm10 <- merge(datapm10, freq10, c("hhid"))
table(datapm10$freq,datapm10$styear)
table(datapm10$freq,datapm10$stssn)



#drop those without 3 measurement periods
data <- merge(data, freq, c("hhid"))



data %>%                    # take the data.frame "data"
  mutate(freq = n_distinct(hhid))
  
data %>%                    # take the data.frame "data"
  filter(!is.na(no2ppb)) %>%    # Using "data", filter out all rows with NAs in aa 
  group_by(hhid) %>%          # Then, with the filtered data, group it by "bb"
  mutate(count = n_distinct(styear))
  summarise(Unique_Elements = n_distinct(styear))   # Now summarise with unique elements per group

data %>%
  filter(!is.na(no2ppb)) %>%    # Using "data", filter out all rows with NAs in aa 
  group_by(count) %>%          # Then, with the filtered data, group it by "bb"
  summarise(Unique_Elements = n_distinct(styear))   # Now summarise with unique elements per group

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

data87 <- subset(data, styear==87)
#no2 conc over season for 1987 only
ggplot(data87,aes(stssn, no2ppb)) +
  geom_boxplot() +
  #facet_wrap(~styear) +
  stat_summary(fun.data=give.n, geom="text") +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Season")


#no2 conc over season and year
ggplot(data,aes(stssn, no2ppb)) +
  geom_boxplot() +
  facet_wrap(~styear) +
  stat_summary(fun.data=give.n, geom="text") +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Season")

#no2 conc over season and year
ggplot(data,aes(stssn, no2ppb)) +
  geom_boxplot() +
  facet_wrap(~styear) +
  stat_summary(fun.data=give.n, geom="text") +
  labs(y=expression(paste(NO[2], " Conc. (ppb)"))
       ,x="Season")

#pm2.5 conc over season and year
ggplot(data,aes(stssn, pm25ugm3)) +
  geom_boxplot() +
  facet_wrap(~styear) +
  stat_summary(fun.data=give.n, geom="text") +
  labs(y=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")"))
       ,x="Season")

#pm10 conc over season and year
ggplot(data,aes(stssn, pm10ugm3)) +
  geom_boxplot() +
  facet_wrap(~styear) +
  stat_summary(fun.data=give.n, geom="text") +
  labs(y=expression(paste(PM[10], " Conc. (", mu, g/m^3,")"))
       ,x="Season")


#create frequency of participants
freqno2 <- count(datano2,"hhid")
data <- merge(datano2,freqno2,c("hhid"))
describe(freq)
describeBy(data,group="styear")

summary(factor(datano2$hhid))



ggplot(data=datano2) + geom_density(mapping = aes(x=styear, colour=stssn))
ggplot(data=datapm25) + geom_density(mapping = aes(x=styear, colour=stssn))
ggplot(data=datapm10) + geom_density(mapping = aes(x=styear, colour=stssn))

ggplot(datano2,aes(x=no2ppb,fill=styear))+geom_density(alpha=.3)
ggplot(datapm25,aes(x=pm25ugm3,fill=styear))+geom_density(alpha=.3)
ggplot(datapm10,aes(x=pm10ugm3,fill=styear))+geom_density(alpha=.3)

ggplot(datano2,aes(x=no2ppb,fill=stssn))+geom_density(alpha=.3)
ggplot(datapm25,aes(x=pm25ugm3,fill=stssn))+geom_density(alpha=.3)
ggplot(datapm10,aes(x=pm10ugm3,fill=stssn))+geom_density(alpha=.3)

ggplot(data,aes(x=no2ppb,fill=styear))+geom_density(alpha=.3) + facet_wrap(~stssn)
ggplot(data,aes(x=pm25ugm3,fill=styear))+geom_density(alpha=.3) + facet_wrap(~stssn)
ggplot(data,aes(x=pm10ugm3,fill=styear))+geom_density(alpha=.3) + facet_wrap(~stssn)

ggplot(data,aes(x=no2ppb,fill=stssn))+geom_density(alpha=.3) + facet_wrap(~styear)
ggplot(data,aes(x=pm25ugm3,fill=stssn))+geom_density(alpha=.3) + facet_wrap(~styear)
ggplot(data,aes(x=pm10ugm3,fill=stssn))+geom_density(alpha=.3) + facet_wrap(~styear)

ggplot(data=subset(data,styear==87 | styear==88),aes(no2ppb,pm25ugm3,colour=stssn), label = rownames(hhid)) + 
  geom_point() +
  geom_smooth(aes(method="lm"), se=FALSE) +
  labs(x="PM2.5 Conc.")
facet_wrap(~freq)

ggplot(data=data,aes(no2ppb,pm25ugm3,colour=stssn)) + 
  geom_point() +
  geom_smooth(aes(method="lm"), se=FALSE) +
  facet_wrap(~freq)

#### ISES 2017 Pres and Poster Stats/Graphs ####
#read in all p5 data
p5all<- read.csv("~/Dropbox/P5_TAPS_TEMP/P5_Master.csv")

#subset out the monitor data
data<-subset(p5all,hhid<5001)

#descriptive graphs
data$freq <- factor(data$freq)
data$styear <- factor(data$styear)
data$stssn <- factor(data$stssn)
data$stmon <- factor(data$stmon)

no2raw_season <- ggplot(subset(data, styear==87),aes(stssn, no2ppb)) +
  geom_boxplot() +
  stat_summary(fun.data=give.n, geom="text") +
  labs(y=expression(paste(NO[2], " Conc. (ppb)")),x="Season") +
  theme(text = element_text(size=20))

setwd("/Users/nathanlothrop/Dropbox/P5_TAPS_TEMP/P5/Plots")
ggsave(filename = "no2raw_87_season.jpg", plot = no2raw_season, width = 8.25, height = 5.5, dpi = 300)

#create dataset of those with more than 1 observation period in 1987
rdata_multi <- merge(rdata, no2freq_87, c("hhid"))

rdata_multi$no2freq <- rdata_multi$freq

rdata_multi <- filter(rdata_multi, no2freq>1)

rdata_multi$styear.y <- NULL
rdata_multi$freq <- NULL
rdata_multi$no2freq <- NULL


#### Read in and clean concentration data ####
#read in all p5 data
p5all<- read.csv("~/Dropbox/P5_TAPS_TEMP//P5_Master.csv")

#subset out the monitor data
monitor<-subset(p5all,hhid>5001)
data<-subset(p5all,hhid<5001)


#find and remove single obs homes in 1987
no2freq <- data %>%  
  group_by(styear, hhid) %>%
  mutate(freq = n())

no2freq_87 <- filter(no2freq, styear==87)

#look into what homes have <2 measures, also including 2 back to back (this can't count as multi measures)

no2freq_2obs <- data %>%  
  group_by(styear, stmon, hhid) %>%
  summarise(freq = n())

no2freq_2obs <- data.frame(no2freq_2obs)

no2freq_87_2obs <- filter(no2freq_2obs, styear==87)

#pull out those that have 2 obs periods in different seasons
no2freq_totobs <- no2freq_87_2obs %>%  
  group_by(hhid) %>%
  summarise(freq = n())

no2freq_totobs <- data.frame(no2freq_totobs)

no2freq_totobs <- filter(no2freq_totobs, freq==2)

no2freq_totobs$styear <- NULL
no2freq_totobs$freq <- NULL
no2freq_totobs$stmon <- NULL

#summary stats for temoprally corrected no2
rdata %>% summarise(Mean = mean(no2adj), SD = sd(no2adj), Min = min(no2adj), Max = max(no2adj))

#summary stats for whole year
datano2 %>% summarise(Mean = mean(no2ppb), SD = sd(no2ppb), Min = min(no2ppb), Max = max(no2ppb))



#freq <- count(data,"hhid")
#freq <- merge(freq, data, c("hhid"))

#data$freq <- factor(data$freq)

# datano2 <- subset(data,!is.na(no2ppb))
# 
# datano2$styear <- factor(datano2$styear)
# ggplot(data=datano2) + geom_bar(mapping = aes(x=styear))
# 
# describe(datano2$freq)
# str(datano2)
# 
# ggplot(data=data)+geom_point(mapping=aes(y=no2ppb,alpha=count))
# 
# ggplot(data,aes(x=no2ppb,fill=styear))+geom_density(alpha=.3)
# 
# describe(data$no2ppb)

# monitor89 <- subset(monitor,styear==89)
# table(monitor89$stmon, monitor89$hhid)
# table(monitor89$pm25ugm3, monitor89$hhid)
# table(monitor89$no2ppb, monitor89$hhid)
# data$pm25adj<-data$pm25ugm3 - data$pm25.avgdif
# 
# monitor89pm25 <- subset(monitor89, !is.na(pm25ugm3))
# 
# describe(monitor89pm25$pm25ugm3)
# 
# monitor89no2 <- subset(monitor89, !is.na(no2ppb))
# describe(monitor89no2$no2ppb)
# 
# 
# monitor90 <- subset(monitor,styear==90)
# 
# monitor90pm25 <- subset(monitor, !is.na(pm25ugm3))
# 
# describe(monitor90pm25$pm25ugm3)
# 
# ggplot(data=monitor89pm25, aes(x=stmon, fill=as.factor(hhid))) +
#   geom_histogram(binwidth=1)
# 
# 
# ggplot(data=monitor90pm25, aes(x=stmon, fill=as.factor(hhid))) +
#   geom_histogram(binwidth=1)
# 
# str(data)
# describe(data$no2ppb)
# describe(data$pm25ugm3)
# describe(data$pm10ugm3)

#ggpairs(data=data, columns=c(data$no2ppb))

#### Calculate temporally corrected concentrations ####

########TEST WITH JUST hhid 9992 as monitor

#monitor <- subset(monitor, hhid==9992)

#do annual avg by monitor hhid
pm25avg<-aggregate(x=monitor$pm25ugm3, by=list(monitor$hhid,monitor$styear), FUN=mean, na.rm=T)
pm10avg<-aggregate(x=monitor$pm10ugm3, by=list(monitor$hhid,monitor$styear), FUN=mean, na.rm=T)
no2avg<-aggregate(x=monitor$no2ppb, by=list(monitor$hhid,monitor$styear), FUN=mean, na.rm=T)

#do monthly avg by monitor hhid
pm25monavg<-aggregate(x=monitor$pm25ugm3, by=list(monitor$hhid, monitor$stmon,monitor$styear), FUN=mean, na.rm=T)
pm10monavg<-aggregate(x=monitor$pm10ugm3, by=list(monitor$hhid, monitor$stmon,monitor$styear), FUN=mean, na.rm=T)
no2monavg<-aggregate(x=monitor$no2ppb, by=list(monitor$hhid, monitor$stmon,monitor$styear), FUN=mean, na.rm=T)


names(pm25avg)[1]<-"hhid"
names(pm25avg)[2]<-"styear"
names(pm25avg)[3]<-"pm25avg"

names(pm10avg)[1]<-"hhid"
names(pm10avg)[2]<-"styear"
names(pm10avg)[3]<-"pm10avg"

names(no2avg)[1]<-"hhid"
names(no2avg)[2]<-"styear"
names(no2avg)[3]<-"no2avg"

names(pm25monavg)[1]<-"hhid"
names(pm25monavg)[2]<-"stmon"
names(pm25monavg)[3]<-"styear"
names(pm25monavg)[4]<-"pm25mavg"

names(pm10monavg)[1]<-"hhid"
names(pm10monavg)[2]<-"stmon"
names(pm10monavg)[3]<-"styear"
names(pm10monavg)[4]<-"pm10mavg"

names(no2monavg)[1]<-"hhid"
names(no2monavg)[2]<-"stmon"
names(no2monavg)[3]<-"styear"
names(no2monavg)[4]<-"no2mavg"

describe(pm25monavg)

#merge avgs to monitor data
monitor<-merge(monitor, pm25avg, by = c("hhid","styear"), all = TRUE)
monitor<-merge(monitor, pm10avg, by = c("hhid","styear"), all = TRUE)
monitor<-merge(monitor, no2avg, by = c("hhid","styear"), all = TRUE)
monitor<-merge(monitor, pm25monavg, by = c("hhid","styear","stmon"), all = TRUE)
monitor<-merge(monitor, pm10monavg, by = c("hhid","styear","stmon"), all = TRUE)
monitor<-merge(monitor, no2monavg, by = c("hhid","styear","stmon"), all = TRUE)

#drop all other data from monitor
keeps<-c("hhid","stmon","styear","pm25avg","pm10avg","no2avg","pm25mavg","pm10mavg","no2mavg")
monitor<-monitor[keeps]

names(monitor)[1]<-"mhhid"

#merge monitor data to sample data by starting month and year
data<-merge(data, monitor, by=c("stmon", "styear"), all=TRUE)

count(monitor, 'mhhid')
count(monitor, 'stmon')
count(monitor, 'styear')



#pull in continuous reference site data from PDEQ

#pull in continuous daily avg from Pima County DEQ from 1986-1990 via EPA AQS Data Mart: https://aqs.epa.gov/api
pm25.cont <- read.csv("~/Dropbox/P5_TAPS_TEMP//Data/PM25.csv")
pm10.cont <- read.csv("~/Dropbox/P5_TAPS_TEMP//Data/PM10.csv")
no2.cont <- read.csv("~/Dropbox/P5_TAPS_TEMP//Data/NO2.csv")


#pull out and format date fields to match P5 expo formatting
pm25.cont$stdate<-as.Date(pm25.cont$Date.GMT, "%m/%d/%Y")
pm25.cont$stmon<-as.numeric(format(pm25.cont$stdate, "%m"))
pm25.cont$stday<-as.numeric(format(pm25.cont$stdate, "%d"))
pm25.cont$styear<-as.numeric(format(pm25.cont$stdate, "%y"))

pm10.cont$stdate<-as.Date(pm10.cont$Date.GMT, "%m/%d/%Y")
pm10.cont$stmon<-as.numeric(format(pm10.cont$stdate, "%m"))
pm10.cont$stday<-as.numeric(format(pm10.cont$stdate, "%d"))
pm10.cont$styear<-as.numeric(format(pm10.cont$stdate, "%y"))

no2.cont$stdate<-as.Date(no2.cont$Date.GMT, "%m/%d/%Y")
no2.cont$stmon<-as.numeric(format(no2.cont$stdate, "%m"))
no2.cont$stday<-as.numeric(format(no2.cont$stdate, "%d"))
no2.cont$styear<-as.numeric(format(no2.cont$stdate, "%y"))





# #do annual avg 
# pm25avg.cont<-aggregate(x=pm25.cont$Sample.Measurement, by=list(pm25.cont$styear), FUN=mean, na.rm=T)
# 
# names(pm25avg.cont)[1]<-"styear"
# names(pm25avg.cont)[2]<-"pm25avg"


# #do monthly avg
# pm25monavg.cont<-aggregate(x=pm25.cont$Sample.Measurement, by=list(pm25.cont$stmon, pm25.cont$styear), FUN=mean, na.rm=T)
# 
# names(pm25monavg.cont)[1]<-"stmon"
# names(pm25monavg.cont)[2]<-"styear"
# names(pm25monavg.cont)[3]<-"pm25monavg"
# 
# 
# #merge continuous data avgs by starting month and year to determine 
# data.cont<-merge(pm25monavg.cont,pm25avg.cont,  by=c("styear"), all=TRUE)
# 
# #create avg dif from annual avg by month
# data.cont$pm25avgdif<-(data.cont$pm25monavg - data.cont$pm25avg)


#do annual avg for all years
pm25contavg <- mean(pm25.cont$Sample.Measurement, na.rm=TRUE)
pm10contavg <- mean(pm10.cont$Sample.Measurement, na.rm=TRUE)
no2contavg <- mean(no2.cont$Sample.Measurement, na.rm=TRUE)


# names(pm25avg.cont)[1]<-"styear"
# names(pm25avg.cont)[2]<-"pm25avg"

#do monthly avg regardless of year
pm25monavg.cont<-aggregate(x=pm25.cont$Sample.Measurement, by=list(pm25.cont$stmon), FUN=mean, na.rm=T)
pm10monavg.cont<-aggregate(x=pm10.cont$Sample.Measurement, by=list(pm10.cont$stmon), FUN=mean, na.rm=T)
no2monavg.cont<-aggregate(x=no2.cont$Sample.Measurement, by=list(no2.cont$stmon), FUN=mean, na.rm=T)



names(pm25monavg.cont)[1]<-"stmon"
names(pm25monavg.cont)[2]<-"pm25monavg"

names(pm10monavg.cont)[1]<-"stmon"
names(pm10monavg.cont)[2]<-"pm10monavg"

names(no2monavg.cont)[1]<-"stmon"
names(no2monavg.cont)[2]<-"no2monavg"

# #find the avg % dif for seasonal (monthly correction)
# pm25monavg.cont$pm25.avgdif<-(pm25monavg.cont$pm25monavg - pm25contavg)/pm25contavg
# pm10monavg.cont$pm10.avgdif<-(pm10monavg.cont$pm10monavg - pm10contavg)/pm10contavg
# no2monavg.cont$no2.avgdif<-(no2monavg.cont$no2monavg - no2contavg)/no2contavg

#do avg dif
pm25monavg.cont$pm25.avgdif<-pm25monavg.cont$pm25monavg - pm25contavg
pm10monavg.cont$pm10.avgdif<-pm10monavg.cont$pm10monavg - pm10contavg
no2monavg.cont$no2.avgdif<-no2monavg.cont$no2monavg - no2contavg

#find the avg dif for seasonal (monthly correction)
#pm25monavg.cont$avgdif<-pm25monavg.cont$pm25monavg - pm25contavg

#merge continuous data avgs by starting month and year to determine 
#data.cont<-merge(pm25monavg.cont,pm25avg.cont,  by=c("stmon"), all=TRUE)

#create avg dif from annual avg by month
#data.cont$pm25avgdif<-(pm25monavg.cont$pm25monavg - pm25contavg)


#merge sample data to % avg dif
data<-merge(data,pm25monavg.cont, by=c("stmon"), all=TRUE)
data<-merge(data,pm10monavg.cont, by=c("stmon"), all=TRUE)
data<-merge(data,no2monavg.cont, by=c("stmon"), all=TRUE)

# #calc temporally adjusted sample conc by sampling month with pct dif
# data$pm25adj<-data$pm25ugm3 + (data$pm25ugm3 * data$pm25.avgdif)
# data$pm25unadj<-data$pm25ugm3
# 
# data$pm10adj<-data$pm10ugm3 + (data$pm10ugm3 * data$pm10.avgdif)
# data$pm10unadj<-data$pm10ugm3
# 
# data$no2adj<-data$no2ppb + (data$no2ppb * data$no2.avgdif)
# data$no2unadj<-data$no2ppb

#calc temporally adjusted sample conc by sampling month
data$pm25adj<-data$pm25ugm3 - data$pm25.avgdif
data$pm25unadj<-data$pm25ugm3

data$pm10adj<-data$pm10ugm3 - data$pm10.avgdif
data$pm10unadj<-data$pm10ugm3

data$no2adj<-data$no2ppb - data$no2.avgdif
data$no2unadj<-data$no2ppb


#sum temporally adjusted sample conc by year
pm25adj.avg<-aggregate(x=data$pm25adj, by=list(data$hhid, data$styear), FUN=mean, na.rm=T)
pm25unadj.avg<-aggregate(x=data$pm25unadj, by=list(data$hhid, data$styear), FUN=mean, na.rm=T)

pm10adj.avg<-aggregate(x=data$pm10adj, by=list(data$hhid, data$styear), FUN=mean, na.rm=T)
pm10unadj.avg<-aggregate(x=data$pm10unadj, by=list(data$hhid, data$styear), FUN=mean, na.rm=T)

no2adj.avg<-aggregate(x=data$no2adj, by=list(data$hhid, data$styear), FUN=mean, na.rm=T)
no2unadj.avg<-aggregate(x=data$no2unadj, by=list(data$hhid, data$styear), FUN=mean, na.rm=T)





names(pm25adj.avg)[1]<-"hhid"
names(pm25adj.avg)[2]<-"styear"
names(pm25adj.avg)[3]<-"pm25ugm3_adj"

names(pm25unadj.avg)[1]<-"hhid"
names(pm25unadj.avg)[2]<-"styear"
names(pm25unadj.avg)[3]<-"pm25ugm3.unadj"

names(pm10adj.avg)[1]<-"hhid"
names(pm10adj.avg)[2]<-"styear"
names(pm10adj.avg)[3]<-"pm10ugm3_adj"

names(pm10unadj.avg)[1]<-"hhid"
names(pm10unadj.avg)[2]<-"styear"
names(pm10unadj.avg)[3]<-"pm10ugm3.unadj"

names(no2adj.avg)[1]<-"hhid"
names(no2adj.avg)[2]<-"styear"
names(no2adj.avg)[3]<-"no2ppb_adj"

names(no2unadj.avg)[1]<-"hhid"
names(no2unadj.avg)[2]<-"styear"
names(no2unadj.avg)[3]<-"no2ppb.unadj"

#merge adjusted and unadjusted annual mean concentrations
data.pm25<-merge(pm25adj.avg, pm25unadj.avg, by=c("hhid","styear"), all=TRUE)
data.pm10<-merge(pm10adj.avg, pm10unadj.avg, by=c("hhid","styear"), all=TRUE)
data.no2<-merge(no2adj.avg, no2unadj.avg, by=c("hhid","styear"), all=TRUE)

data.pm<-merge(data.pm25,data.pm10, by=c("hhid","styear"), all=TRUE)
data<-merge(data.pm,data.no2, by=c("hhid","styear"), all=TRUE)

#compare adjusted and unadjusted annual means
plot(data$pm25ugm3_adj, data$pm25ugm3.unadj)
plot(data$pm10ugm3_adj, data$pm10ugm3.unadj)
plot(data$no2ppb_adj, data$no2ppb.unadj)

datapm25 <- subset(data,!is.na(data$pm25ugm3_adj))

# ggplot(data=datapm25,aes(x=datapm25$pm25ugm3_adj, fill=datapm25$styear))+
#   geom_histogram(binwidth=1)


datapm25.lm<-lm(data$pm25ugm3_adj~data$pm25ugm3.unadj)
summary(datapm25.lm)

datapm10.lm<-lm(data$pm10ugm3_adj~data$pm10ugm3.unadj)
summary(datapm10.lm)

datano2.lm<-lm(data$no2ppb_adj~data$no2ppb.unadj) 
summary(datano2.lm)

#ggpairs(data[,c(3:8)])

# ggplot(data,aes(no2_adj,no2_unadj, color=styear))+
#   geom_point()




#corr matrix
data.conc<-data[3:8]
data.corr<-cor(data.conc,use="pairwise.complete.obs",method="pearson")
corrplot(data.corr, method="shade",addCoef.col="black")

#rename fields for GIS
names(data)[3]<-"pm25_adj"
names(data)[4]<-"pm25_unadj"
names(data)[5]<-"pm10_adj"
names(data)[6]<-"pm10_unadj"
names(data)[7]<-"no2_adj"
names(data)[8]<-"no2_unadj"

count(data, 'hhid')
count(data, 'stmon')
count(data, 'styear')

#write table of homes with air pollution data
write.table(data, "~/Dropbox/P5_TAPS_TEMP//Data/P5Data.txt", sep="\t", row.names = F)

#must open in excel and delete 1st numbered column, move headings to approp. cols and resave
#then can be read into ArcGIS where it is joined to full address set and exported to "P5.gdb/SITES/AddresseswAP"

#RUN most recent P5 script in P5.gdb toolbox

#read in GIS data

#lu <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/lu.txt")
#census <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/pop_hh.txt")
#elev <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/elev.txt")
#roadll <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/roadlength_load.txt")
#roadllm <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/roadlength_load_maj.txt")
#roadnr <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/roadnear.txt")
#mroadnr <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/roadnearmaj.txt")











#### Read in ArcGIS outputs ####


# read in predictor tables from GIS, append batches, keep applicable fields and relable

#BUS ROUTES DISTANCE TO
#read in predictors from GIS model script
busrtnr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/BusrtNearM1.txt", header = F, sep=" ")
busrtnr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/BusrtNearM2.txt", header = F, sep=" ")

#append batches together
busrtnr <- rbind(busrtnr1, busrtnr2)

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
#read in predictors from GIS model script
busrtl1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/BusrtLength1.txt", header = F, sep=" ")
busrtl2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/BusrtLength2.txt", header = F, sep=" ")

#append batches together
busrtl <- rbind(busrtl1, busrtl2)

# keep HHID and dist to fields
keep<-c(3:5)
busrtl<-busrtl[keep]

#rename remaining fields
names(busrtl)[1]<-"hhid"
names(busrtl)[2]<-"BuffM"
names(busrtl)[3]<-"ROADLENGTH"

summary(is.na(busrtl))

#RAILWAY DISTANCE TO
#read in predictors from GIS model script
railnr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RailNearM1.txt", header = F, sep=" ")
railnr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RailNearM2.txt", header = F, sep=" ")

#append batches together
railnr <- rbind(railnr1, railnr2)

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
#read in predictors from GIS model script
railyardnr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RailYardNearM1.txt", header = F, sep=" ")
railyardnr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RailYardNearM2.txt", header = F, sep=" ")

#append batches together
railyardnr <- rbind(railyardnr1, railyardnr2)

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
#read in predictors from GIS model script
raill1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RailLength1.txt", header = F, sep=" ")
raill2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RailLength2.txt", header = F, sep=" ")

#append batches together
raill <- rbind(raill1, raill2)

# keep HHID and dist to fields
keep<-c(3:5)
raill<-raill[keep]

#rename remaining fields
names(raill)[1]<-"hhid"
names(raill)[2]<-"BuffM"
names(raill)[3]<-"ROADLENGTH"

summary(is.na(raill))


#AIRPORT DISTANCE TO
#read in predictors from GIS model script
airnr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/AirNear1.txt", header = F, sep=" ")
airnr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/AirNear2.txt", header = F, sep=" ")

#append batches together
airnr <- rbind(airnr1, airnr2)

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
#read in predictors from GIS model script
minenr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MineNear1.txt", header = F, sep=" ")
minenr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MineNear2.txt", header = F, sep=" ")

#append batches together
minenr <- rbind(minenr1, minenr2)

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
roadnr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RoadNear1.txt", header = F, sep=" ")
roadnr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RoadNear2.txt", header = F, sep=" ")

#append batches together
roadnr <- rbind(roadnr1, roadnr2)

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
for (i in 1:length(roadnr[,2])){
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
keep<-c(1:2,4:7)
roadnr<-roadnr[keep]

#MAJOR ROADS DISTANCE TO
#read in predictors from GIS model script
mroadnr1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MRoadNear1.txt", header = F, sep=" ")
mroadnr2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MRoadNear2.txt", header = F, sep=" ")

#append batches together
mroadnr <- rbind(mroadnr1, mroadnr2)

# keep HHID and dist to fields
keep<-c(3:5)
mroadnr<-mroadnr[keep]

#rename remaining fields
names(mroadnr)[1]<-"trafmajor"
names(mroadnr)[2]<-"hhid"
names(mroadnr)[3]<-"NRDistM"

#transforms traffic counts into true values
mroadnr$trafmajor<-mroadnr$trafmajor * 1000

mroadnr$distintvmajor1<-1/mroadnr$NRDistM
mroadnr$distintvmajor2<-(1/mroadnr$NRDistM)^2
mroadnr$intmajorinv1<-mroadnr$trafmajor * (1/mroadnr$NRDistM)
mroadnr$intmajorinv2<-mroadnr$trafmajor * ((1/mroadnr$NRDistM)^2)

#drop the distance variable (not a regression predictor)
keep<-c(1:2,4:7)
mroadnr<-mroadnr[keep]


#ALL ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
#read in predictors from GIS model script
roadll1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RoadLL1.txt", header = F, sep=" ")
roadll2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/RoadLL2.txt", header = F, sep=" ")

#append batches together
roadll <- rbind(roadll1, roadll2)

# keep HHID and dist to fields
keep<-c(3:6)
roadll<-roadll[keep]

#rename remaining fields
names(roadll)[1]<-"hhid"
names(roadll)[2]<-"BuffM"
names(roadll)[3]<-"ROADLENGTH"
names(roadll)[4]<-"TRAFLOAD"


#MAJOR ROADS TRAFFIC LOAD AND LENGTH IN BUFFER
#read in predictors from GIS model script
mroadll1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MRoadLL1.txt", header = F, sep=" ")
mroadll2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MRoadLL2.txt", header = F, sep=" ")

#append batches together
mroadll <- rbind(mroadll1, mroadll2)

# keep HHID and dist to fields
keep<-c(3:6)
mroadll<-mroadll[keep]

#rename remaining fields
names(mroadll)[1]<-"hhid"
names(mroadll)[2]<-"BuffM"
names(mroadll)[3]<-"ROADLENGTH"
names(mroadll)[4]<-"TRAFLOAD"



# #AIRPORTS AREA IN BUFFER
# #read in predictors from GIS model script
# airarea1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/AirArea1.txt", header = F, sep=" ")
# airarea2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/AirArea2.txt", header = F, sep=" ")
# 
# #append batches together
# airarea <- rbind(airarea1, airarea2)
# 
# # keep HHID and dist to fields
# keep<-c(3:5)
# airarea<-airarea[keep]
# 
# #rename remaining fields
# names(airarea)[1]<-"hhid"
# names(airarea)[2]<-"BuffM"
# names(airarea)[3]<-"Area"



# #MINES AREA IN BUFFER
# #read in predictors from GIS model script
# minesarea1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MinesArea1.txt", header = F, sep=" ")
# minesarea2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/MinesArea2.txt", header = F, sep=" ")
# 
# #append batches together
# minesarea <- rbind(minesarea1, minesarea2)
# 
# # keep HHID and dist to fields
# keep<-c(3:5)
# minesarea<-minesarea[keep]
# 
# #rename remaining fields
# names(minesarea)[1]<-"hhid"
# names(minesarea)[2]<-"BuffM"
# names(minesarea)[3]<-"Area"


#LAND USE AREA IN BUFFER
#read in predictors from GIS model script
lu1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/lu1.txt", header = F, sep=" ")
lu2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/lu2.txt", header = F, sep=" ")

#append batches together
lu <- rbind(lu1, lu2)

# keep HHID and dist to fields
keep<-c(3:6)
lu<-lu[keep]

#rename remaining fields
names(lu)[1]<-"grid_code"
names(lu)[2]<-"hhid"
names(lu)[3]<-"BuffM"
names(lu)[4]<-"Area"


#prep GIS data for merging
#keep<-c(3:4,9:10)
#lu<-lu[keep]


lu$grid_code<-as.numeric(as.character(lu$grid_code))
# lu<-subset(lu,lu$grid_code==21 | 
#              lu$grid_code==22 |
#              lu$grid_code==23 |
#              lu$grid_code==85 |
#              (lu$grid_code>30 & lu$grid_code<79) 
#              )
#lu$lu<-"NULL"

for (i in 1:length(lu[,2])){
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
#read in predictors from GIS model script
census1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/Census1.txt", header = F, sep=" ")
census2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/Census2.txt", header = F, sep=" ")

#append batches together
census <- rbind(census1, census2)

# keep HHID and dist to fields
keep<-c(3:6)
census<-census[keep]

#rename remaining fields
names(census)[1]<-"hhid"
names(census)[2]<-"BuffM"
names(census)[3]<-"pop"
names(census)[4]<-"hh"


#ELEVATION
#read in predictors from GIS model script
elev1 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/Elev1.txt", header = F, sep=" ")
elev2 <- read.delim("~/Dropbox/P5_TAPS_TEMP//Data/Elev2.txt", header = F, sep=" ")

#append batches together
elev <- rbind(elev1, elev2)

# keep HHID and dist to fields
keep<-c(3:4)
elev<-elev[keep]



#rename remaining fields
names(elev)[1]<-"hhid"
names(elev)[2]<-"elev"

#square root elevation
elev$elev <- sqrt(elev$elev)


#keep<-c(3,5:7)
#census<-census[keep]

#keep<-c(3,11)
#elev<-elev[keep]
#names(elev)[2]<-"elev"

#keep<-c(3,5,7:8)
#roadll<-roadll[keep]

# keep<-c(3,5,7:8)
# roadllm<-roadllm[keep]
# 
# #near all roads
# keep<-c(10:11,13)
# roadnr<-roadnr[keep]




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
names(rail.len)[2]<-"ll_300"
names(rail.len)[3]<-"ll_500"
names(rail.len)[4]<-"ll_1000"

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

#### Combine GIS predictors and outcomes - Summary Stats of Analysis Data ####

#merge air pollution to GIS data

#subset final air pollution data by year

#sets data to all starting in 1987
#data<-subset(data,data$styear==87)

#use reduce function
pdata<-Reduce(function(x,y) merge(x,y, all=TRUE),
              list(lu,
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

#replace values temporally corrected to ND with 0
#ap.data<-rdata[,c(1,3:8)]


#correct predictors by replacing NA to 0
pdata[, 2:ncol(pdata)][is.na(pdata[, 2:ncol(pdata)])] <- 0

#filter 87 year averages
rdata <- subset(data, data$styear==87)
rownames(rdata) <- NULL

#forward stepwise linear regression according to ESCAPE protocol

#corr matrix
library(corrplot)

data.corr<-cor(rdata,use="pairwise.complete.obs",method="spearman")
corrplot(rdata, method="shade",addCoef.col="black")

hist(rdata$no2_adj)
hist(rdata$pm25_adj)
hist(rdata$pm10_adj)



#drop bogus vals
#ap.data[ap.data<0]<-0



#drop observations with 0s for predictor that can't be 0
#rdata<-subset(rdata,distinvnear1>0)


#correct concentrations temporally corrected to values<0 with 0
#rdata[rdata<0]<-0

summary(rdata$no2_adj)
summary(rdata$pm25_adj)
summary(rdata$pm10_adj)


#box plots of pollutants
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2, 2))
no2.box<-ap.data$no2_adj
boxplot(no2.box,main=expression(paste(NO[2], " Concentration")), ylab=expression(paste(NO[2], " Conc. (ppb)")))

pm25.box<-ap.data$pm25_adj
boxplot(pm25.box,main=expression(paste(PM[2.5], " Concentration")), ylab=expression(paste(PM[2.5], " Conc. (", mu, g/m^3,")")))

pm10.box<-ap.data$pm10_adj
boxplot(pm10.box,main=expression(paste(PM[10], " Concentration")), ylab=expression(paste(PM[10], " Conc. (", mu, g/m^3,")")))

#ggpairs(ap.data[,c(2,4,6)])

#create regression dataset
rdata <- merge(rdata,pdata,by=c("hhid"))

#removing hhid 3955 outlier
rdata <- subset(rdata, hhid!=3955)

#create dataset of those with more than 1 observation period in 1987
rdata_multi <- merge(rdata, no2freq_87, c("hhid"))

rdata_multi$no2freq <- rdata_multi$freq

rdata_multi <- filter(rdata_multi, no2freq>1)

rdata_multi$styear.y <- NULL
rdata_multi$freq <- NULL
rdata_multi$no2freq <- NULL

#drop HHID 1354 and 1202 for 1987 as they have 2 measurements, but they're back to back weeks
rdata_multi <- filter(rdata_multi, hhid!=1354 & hhid!=1202)


#create dataset of those with 2 obs in opposite seasons
rdata_2obs <- merge(no2freq_totobs,rdata, by=c("hhid"))

#summary stats for temoprally corrected no2
rdata %>%
  summarise(Mean = mean(no2_adj),
            SD = sd(no2_adj),
            Min = min(no2_adj),
            Max = max(no2_adj),
            total = n())

#summary stats for temoprally corrected no2, geo means excluding 1 negative value
rdata %>%
  filter(no2_adj>0) %>%
  summarise(GeoMean = exp(mean(log(no2_adj))),
            GeoSD = exp(sd(log(no2_adj))))

#summary stats for all observations in 1987
datano2 %>%
  filter(styear==87) %>%
  summarise(Mean = mean(no2ppb),
            SD = sd(no2ppb),
            GeoMean = exp(mean(log(no2ppb))),
            GeoSD = exp(sd(log(no2ppb))),
            Min = min(no2ppb),
            Max = max(no2ppb),
            total = n())


# #NOTU: run this script to log predicted air pollution values
# rdata$no2_adj<-log(rdata$no2_adj)
# rdata$pm25_adj<-log(rdata$pm25_adj)
# rdata$pm10_adj<-log(rdata$pm10_adj)
# 
# hist(rdata$no2_adj)
# hist(rdata$pm25_adj)
# hist(rdata$pm10_adj)

#### Univariate linear regressions ####
#univariate regressions to find starting variable for NO2
sink("~/Dropbox/P5_TAPS_TEMP//Data/r_no2adj.txt")#saves output to text file
lapply( rdata[,-1], function(x) summary(lm(rdata$no2_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM2.5
sink("~/Dropbox/P5_TAPS_TEMP//Data/r_pm25adj.txt")#saves output to text file
lapply( rdata[,-1], function(x) summary(lm(rdata$pm25_adj ~ x)) )
sink()#stops diverting output

#univariate regressions to find starting variable for PM10
sink("~/Dropbox/P5_TAPS_TEMP//Data/r_pm10adj.txt")#saves output to text file
lapply( rdata[,-1], function(x) summary(lm(rdata$pm10_adj ~ x)) )
sink()#stops diverting output

#write table of ap data only
write.table(ap.data, "~/Dropbox/P5_TAPS_TEMP//Data/apdata.txt", sep="\t")

#write table of homes with regression data to make input table
write.table(rdata, "~/Dropbox/P5_TAPS_TEMP//Data/rdata.txt", sep="\t")

# no2fit<-lm(no2_adj~.,data=rdata)
# leaps<-regsubsets(no2_adj~.,data=rdata,really.big=T)
# summary(leaps)
# plot(leaps,scale="r2")
# 
# pm25fit<-lm(pm25_adj~.,data=rdata)
# leaps<-regsubsets(pm25_adj~.,data=rdata)
# summary(leaps)
# plot(leaps,scale="r2")


library(car)


#### LUR model development- ESCAPE Method - NO2  ####
#Regression model development with ESCAPE method



summary(lm(no2_adj~nt_5000 + cm_5000 + ug_5000 +
             intinvnear1 + rl_1000 + tl_50 + tl_300 +
             rlm_100 +
             rlm_300 +
             rlm_500 +
             distintvmine2 +
             bl_1000
           ,data=rdata))

#remove least significant predictors 1 by 1 until all have pvalue<0.1
summary(lm(no2_adj~nt_5000 + cm_5000 + ug_5000 +
             intinvnear1 + 
#             rl_1000 + 
              (tl_300-tl_50) +
#             rlm_100 +
#             rlm_300 +
             rlm_500 +
             distintvmine2
           ,data=rdata))



#final model for leave 1 out cross validation
no2adj <- (lm(no2_adj~nt_5000 +
                #cm_5000 +
                #ug_5000 +
                intinvnear1 + 
                (tl_300-tl_50) +
                rlm_500 +
                distintvmine2
              ,data=rdata))
summary(no2adj)

no2adj_E_A <- (lm(no2_adj~nt_5000 +
                #cm_5000 +
                #ug_5000 +
                intinvnear1 + 
                (tl_300-tl_50) +
                rlm_500 +
                distintvmine2
              ,data=rdata))


#check for multicollinearity
vif(no2adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata)-length(no2adj$coefficients)-2)) 
plot(no2adj, which=4, cook.levels=cutoff, labels.id = rdata$hhid)


#HHID 3955 has high cooks d, rerun without it

# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(no2_adj~nt_5000 + 
                 intinvnear1 + 
                 (tl_300-tl_50) +
                 rlm_500 +
                 distintvmine2
               ,data=rdata, trControl=train_control, method="lm")
# summarize results
print(model)


fitno2<-summary(no2adj) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_E_A<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_E_A)[2]<-"no2_r_E_A"
no2_r_E_A <- dplyr::select(no2_r_E_A, hhid, no2_r_E_A)


keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(no2adj)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(no2adj, labels.id = rdata$hhid)






# # # Logged outcome using ESCAPE # # #
#univariate regressions to find starting variable for NO2
#starting with nt_5000

#drop HHID 1354 due to very high cooks d
rdata <- filter(rdata, hhid!=1354 & hhid!=1202)


#final model for leave 1 out cross validation using logged outcome
logno2adj <- (lm(log(no2_adj)~
                   #nt_5000 +
                   cm_5000 +
                   #nt_100 +
                   elev +
                   #tl_50 +
                   rl_500 +
                   #rlm_300 +
                   rlm_1000
              ,data=rdata))
summary(logno2adj)

no2adj_lE_A <- (lm(log(no2_adj)~
                   #nt_5000 +
                   cm_5000 +
                   #nt_100 +
                   elev +
                   #tl_50 +
                   rl_500 +
                   #rlm_300 +
                   rlm_1000
                 ,data=rdata))
#check for multicollinearity
vif(logno2adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata)-length(logno2adj$coefficients)-2)) 
plot(logno2adj, which=4, cook.levels=cutoff, labels.id = rdata$hhid)


# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(log(no2_adj)~
                 cm_5000 +
                 elev +
                 rl_500 +
                 rlm_1000
               ,data=rdata, trControl=train_control, method="lm")
# summarize results
print(model)


fitno2<-summary(logno2adj) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_lE_A<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_lE_A)[2]<-"no2_r_lE_A"
no2_r_lE_A <- dplyr::select(no2_r_lE_A, hhid, no2_r_lE_A)

keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(logno2adj)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(logno2adj, labels.id = rdata$hhid)




# # # Untransformed outcome using ESCAPE - >=2 measurement periods # # #
#univariate regressions to find starting variable for NO2
#starting with nt_5000





#univariate regressions to find starting variable for NO2
sink("~/Dropbox/P5_TAPS_TEMP//Data/r_no2adj_multi.txt")#saves output to text file
lapply( rdata_multi[,-1], function(x) summary(lm(rdata_multi$no2_adj ~ x)) )
sink()#stops diverting output

#final model for leave 1 out cross validation using logged outcome
no2adj <- (lm(no2_adj~elev +
                #cm_300 +
                #cm_1000 +
                cm_5000 +
                #lr_100 +
                intinvnear1 +
                #tl_50 +
                #rl_500 +
                rlm_500 +
                bl_1000
                 ,data=rdata_multi))
summary(no2adj)

no2adj_E_M <- (lm(no2_adj~elev +
                #cm_300 +
                #cm_1000 +
                cm_5000 +
                #lr_100 +
                intinvnear1 +
                #tl_50 +
                #rl_500 +
                rlm_500 +
                bl_1000
              ,data=rdata_multi))




#final model for leave 1 out cross validation using logged outcome
no2adj <- (lm(no2_adj~elev +
                cm_5000 +
                intinvnear1 +
                rlm_500 +
                bl_1000
              ,data=rdata_multi))
summary(no2adj)

#check for multicollinearity
vif(no2adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed


# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata_multi)-length(no2adj$coefficients)-2)) 
plot(no2adj, which=4, cook.levels=cutoff, labels.id = rdata_multi$hhid)


# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(no2_adj~elev +
                 cm_5000 +
                 intinvnear1 +
                 rlm_500 +
                 bl_1000
               ,data=rdata_multi, trControl=train_control, method="lm")
# summarize results
print(model)


fitno2<-summary(no2adj) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_E_M<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_E_M)[2]<-"no2_r_E_M"
no2_r_E_M <- dplyr::select(no2_r_E_M, hhid, no2_r_E_M)

keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(logno2adj)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(no2adj, labels.id = rdata_multi$hhid)


# # # Log transformed outcome using ESCAPE - >=2 measurement periods # # #
#univariate regressions to find starting variable for NO2

#univariate regressions to find starting variable for NO2
sink("~/Dropbox/P5_TAPS_TEMP//Data/r_logno2adj_multi.txt")#saves output to text file
lapply( rdata_multi[,-1], function(x) summary(lm(log(rdata_multi$no2_adj) ~ x)) )
sink()#stops diverting output

#final model for leave 1 out cross validation using logged outcome
no2adj <- (lm(log(no2_adj)~elev +
                #cm_500  +
                #cm_1000 +
                cm_5000 +
                #lr_1000 +
                rl_1000 +
                rlm_300 +
                distintvmine1
              ,data=rdata_multi))
summary(no2adj)

no2adj_lE_M <- (lm(log(no2_adj)~elev +
                #cm_500  +
                #cm_1000 +
                cm_5000 +
                #lr_1000 +
                rl_1000 +
                rlm_300 +
                distintvmine1
              ,data=rdata_multi))



#final model for leave 1 out cross validation using logged outcome
# no2adj <- (lm(no2_adj~elev +
#                 cm_5000 +
#                 intinvnear1 +
#                 rlm_500 +
#                 bl_1000
#               ,data=rdata_multi))
# summary(no2adj)

#check for multicollinearity
vif(no2adj) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed


# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata_multi)-length(no2adj$coefficients)-2)) 
plot(no2adj, which=4, cook.levels=cutoff, labels.id = rdata_multi$hhid)


# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(log(no2_adj)~elev +
                 cm_5000 +
                 rl_1000 +
                 rlm_300 +
                 distintvmine1
               ,data=rdata_multi, trControl=train_control, method="lm")
# summarize results
print(model)


fitno2<-summary(no2adj) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_lE_M<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_lE_M)[2]<-"no2_r_lE_M"
no2_r_lE_M <- dplyr::select(no2_r_lE_M, hhid, no2_r_lE_M)


keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(logno2adj)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(no2adj, labels.id = rdata_multi$hhid)


#### LUR Development - Brauer (Vancouver) Method - NO2 ####
#Regression model development using Henderson et al. 2007 method (Application of land use regression to estimate long-term concentrations of traffic-related nitrogen oxides and fine particulate matter)
# The following model-building algorithm was used: 
# (1) Rank all variables by the absolute strength of their correlation with the measured pollutant.
# (2) Identify the highest-ranking variable in each sub-category.
# (3) Eliminate other variables in each sub-category that are correlated (Pearson's r>0.6) with the most highly ranked variable.
# (4) Enter allremainingvariables intoastepwise linear regression.
# (5) Remove from the available pool any variables that have
#       (a) insignificant t-statistics (R)0.05) and/or (b) coefficients that are inconsistent with a priori assumptions.
# 6) Repeat steps 4 and 5 to convergence and remove any variable that contributes less than 1% to the R2 value for a parsimonious final


# (1) Rank all variables by the absolute strength of their correlation with the measured pollutant.
# (2) Identify the highest-ranking variable in each sub-category.

#in road length category (incl bus route and rail)
cor <- cor(as.matrix(rdata[,c(7,67:77,87:89,92:96)],type='pearson'))
cordf <- as.data.frame(as.table(cor))
cordfsub <- subset(cordf, cordf$Var1=='no2_adj')
cordfsub <- cordfsub[order(-cordfsub$Freq),]
cordfsub <- subset(cordfsub, Var2!='no2_adj')
cordfsub <- cordfsub[1,]
topcorvar <- toString(cordfsub[1,2])

# (3) Eliminate other variables in each sub-category that are correlated (Pearson's r>0.6) with the most highly ranked variable.
cordfsub2 <- subset(cordf, cordf$Var1==topcorvar)
cordfsub2 <- cordfsub2[order(-cordfsub2$Freq),]
cordfsub2 <- subset(cordfsub2, Freq<0.6)
cordfsub2 <- cordfsub2[,2:3]
cordfsub2 <- subset(cordfsub2,Var2!='no2_adj')
print(cordfsub2)

rlvars <- as.list(levels(cordfsub2[,1]))


#in vehicle density category (incl bus and rail and air)
cor <- cor(as.matrix(rdata[,c(7,45,48:66,81:86,90:91)],type='pearson'))
cordf <- as.data.frame(as.table(cor))
cordfsub <- subset(cordf, cordf$Var1=='no2_adj')
cordfsub <- cordfsub[order(-cordfsub$Freq),]
cordfsub <- subset(cordfsub, Var2!='no2_adj')
cordfsub <- cordfsub[1,]
topcorvar <- toString(cordfsub[1,2])

# (3) Eliminate other variables in each sub-category that are correlated (Pearson's r>0.6) with the most highly ranked variable.
cordfsub2 <- subset(cordf, cordf$Var1==topcorvar)
cordfsub2 <- cordfsub2[order(-cordfsub2$Freq),]
cordfsub2 <- subset(cordfsub2, Freq<0.6)
cordfsub2 <- cordfsub2[,2:3]
cordfsub2 <- subset(cordfsub2,Var2!='no2_adj')
print(cordfsub2)

tdvars <- as.list(levels(cordfsub2[,1]))


#in land use category (incl. mining)
cor <- cor(as.matrix(rdata[,c(7,9:33,79:80)],type='pearson'))
cordf <- as.data.frame(as.table(cor))
cordfsub <- subset(cordf, cordf$Var1=='no2_adj')
cordfsub <- cordfsub[order(-cordfsub$Freq),]
cordfsub <- subset(cordfsub, Var2!='no2_adj')
cordfsub <- cordfsub[1,]
topcorvar <- toString(cordfsub[1,2])

# (3) Eliminate other variables in each sub-category that are correlated (Pearson's r>0.6) with the most highly ranked variable.
cordfsub2 <- subset(cordf, cordf$Var1==topcorvar)
cordfsub2 <- cordfsub2[order(-cordfsub2$Freq),]
cordfsub2 <- subset(cordfsub2, Freq<0.6)
cordfsub2 <- cordfsub2[,2:3]
cordfsub2 <- subset(cordfsub2,Var2!='no2_adj')
print(cordfsub2)

luvars <- as.list(levels(cordfsub2[,1]))


#in pop density (incl. household density)
cor <- cor(as.matrix(rdata[,c(7,34:43)],type='pearson'))
cordf <- as.data.frame(as.table(cor))
cordfsub <- subset(cordf, cordf$Var1=='no2_adj')
cordfsub <- cordfsub[order(-cordfsub$Freq),]
cordfsub <- subset(cordfsub, Var2!='no2_adj')
cordfsub <- cordfsub[1,]
topcorvar <- toString(cordfsub[1,2])

# (3) Eliminate other variables in each sub-category that are correlated (Pearson's r>0.6) with the most highly ranked variable.
cordfsub2 <- subset(cordf, cordf$Var1==topcorvar)
cordfsub2 <- cordfsub2[order(-cordfsub2$Freq),]
cordfsub2 <- subset(cordfsub2, Freq<0.6)
cordfsub2 <- cordfsub2[,2:3]
cordfsub2 <- subset(cordfsub2,Var2!='no2_adj')
print(cordfsub2)

popvars <- as.list(levels(cordfsub2[,1]))

#elev is included on face validity

#combine lists and delete no2_adj
varlist <- c(rlvars, tdvars, luvars, popvars)
varlist[which(names(varlist) %in% c("no2_adj"))] <- NULL

##### Feed all variable lists into backwards regression

preds <- do.call(paste, c(as.list(varlist), sep=" + ", collapse=NULL))

lmform <- as.formula(paste("no2_adj ~ elev + ", preds, sep="", collapse=NULL))

# (4) Enter allremainingvariables intoastepwise linear regression.
# (5) Remove from the available pool any variables that have
#       (a) insignificant t-statistics (R)0.05) and/or (b) coefficients that are inconsistent with a priori assumptions.

fit<-lm(lmform, data=rdata)
summary(fit)

leaps_seqrep<-regsubsets(lmform, data=rdata,
                  nbest=1,
                  nvmax = 10,
                  method = "seqrep",
                  really.big = TRUE)

#leap<-regsubsets(lmform, data=rdata,nbest=1,nvmax = 11,really.big = T)

summary.out <- summary(leaps_seqrep)
leapsvars <- unlist(as.list(names(which(summary.out$which[which.max(summary.out$adjr2),]==TRUE))))

leapfit <- lm(no2_adj ~ elev +
                rl_1000 +
                rlm_500 +
                bl_300 +
                intinvnear1 + 
                #rlm_50 +
                cm_5000 +
                #nt_500 +
                distintvmine1
                #pop_300
              ,data=rdata)
summary(leapfit)

no2adj_A_A <- lm(no2_adj ~ elev +
                rl_1000 +
                rlm_500 +
                bl_300 +
                intinvnear1 + 
                #rlm_50 +
                cm_5000 +
                #nt_500 +
                distintvmine1
              #pop_300
              ,data=rdata)

#check for multicollinearity
vif(leapfit) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata)-length(leapfit$coefficients)-2)) 
plot(leapfit, which=4, cook.levels=cutoff, labels.id = rdata$hhid)


# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(no2_adj ~ elev +
                 rl_1000 +
                 rlm_500 +
                 bl_300 +
                 intinvnear1 + 
                 cm_5000 +
                 distintvmine1
               ,data=rdata, trControl=train_control, method="lm")
# summarize results
print(model)
fitno2<-summary(leapfit) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_A_A<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_A_A)[2]<-"no2_r_A_A"
no2_r_A_A <- dplyr::select(no2_r_A_A, hhid, no2_r_A_A)


keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(leapfit)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(leapfit,labels.id = rdata$hhid)

 # # #Logged variables # # #
##### Feed all variable lists into backwards regression

preds <- do.call(paste, c(as.list(varlist), sep=" + ", collapse=NULL))

lmform <- as.formula(paste("log(no2_adj) ~ elev + ", preds, sep="", collapse=NULL))

# (4) Enter allremainingvariables intoastepwise linear regression.
# (5) Remove from the available pool any variables that have
#       (a) insignificant t-statistics (R)0.05) and/or (b) coefficients that are inconsistent with a priori assumptions.


fit<-lm(lmform, data=rdata)
summary(fit)

logleaps_seqrep<-regsubsets(log(no2_adj) ~ elev + rl_25 + rl_50 + rl_100 + rl_300 + rl_500 + rl_1000 + rlm_25 + rlm_50 + rlm_100 + rlm_300 + rlm_500 + ll_300 + ll_500 + ll_1000 + bl_25 + bl_50 + bl_100 + bl_300 + bl_500 +  trafnear + intinvnear1 + intinvnear2 + trafmajor + distintvmajor1 + distintvmajor2 + intmajorinv1 + intmajorinv2 + tl_25 + tl_50 + tl_100 + tl_300 + tl_500 + tl_1000 + tlm_25 + tlm_50 + tlm_100 + tlm_300 + tlm_500 + tlm_1000 + distintvair1 + distintvair2 + distintvrail1 + distintvrail2 + distintvrailyard1 + distintvrailyard2 + distintvbusrt1 + distintvbusrt2 +  ag_100 + ag_300 + ag_500 + ag_1000 + ag_5000 + cm_100 + cm_300 + cm_500 + cm_1000 + cm_5000 + lr_100 + lr_300 + lr_500 + lr_1000 + lr_5000 + nt_100 + nt_300 + nt_500 + nt_1000 + nt_5000 + ug_100 + ug_300 + ug_500 + ug_1000 + ug_5000 + distintvmine1 + distintvmine2 +  hh_100 + hh_300 + hh_500 + hh_1000 + hh_5000 + pop_100 + pop_300 + pop_500 + pop_1000 + pop_5000, data=rdata_multi,
                         nbest=1,
                         nvmax = 10,
                         method = "seqrep",
                         really.big = TRUE)

#leap<-regsubsets(lmform, data=rdata,nbest=1,nvmax = 11,really.big = T)

summary.out <- summary(logleaps_seqrep)
logleapsvars <- unlist(as.list(names(which(summary.out$which[which.max(summary.out$adjr2),]==TRUE))))
logleapsvars

leapfit <- lm(log(no2_adj) ~ elev +
                #rlm_300 +
                rlm_500 +
                #tl_50 +
                #tlm_100 +
                #cm_1000 +
                #lr_1000 +
                #nt_300 +
                nt_1000 
                #pop_100
              ,data=rdata)
summary(leapfit)

no2adj_lA_A <- lm(log(no2_adj) ~ elev +
                #rlm_300 +
                rlm_500 +
                #tl_50 +
                #tlm_100 +
                #cm_1000 +
                #lr_1000 +
                #nt_300 +
                nt_1000 
              #pop_100
              ,data=rdata)

#check for multicollinearity
vif(leapfit) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata)-length(leapfit$coefficients)-2)) 
plot(leapfit, which=4, cook.levels=cutoff, labels.id = rdata$hhid)


# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(log(no2_adj) ~ elev +
                 rlm_500 +
                 nt_1000 
               ,data=rdata, trControl=train_control, method="lm")
# summarize results
print(model)
fitno2<-summary(leapfit) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_lA_A<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_lA_A)[2]<-"no2_r_lA_A"
no2_r_lA_A <- dplyr::select(no2_r_lA_A, hhid, no2_r_lA_A)

keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(leapfit)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(leapfit)

# # # #More than 1 sampling period for 1987 # # # #

preds <- do.call(paste, c(as.list(varlist), sep=" + ", collapse=NULL))

lmform <- as.formula(paste("no2_adj ~ elev + ", preds, sep="", collapse=NULL))

# (4) Enter allremainingvariables intoastepwise linear regression.
# (5) Remove from the available pool any variables that have
#       (a) insignificant t-statistics (R)0.05) and/or (b) coefficients that are inconsistent with a priori assumptions.

fit<-lm(lmform, data=rdata_multi)
summary(fit)

leaps_seqrep<-regsubsets(lmform, data=rdata_multi,
                         nbest=1,
                         nvmax = 10,
                         method = "seqrep",
                         really.big = TRUE)

#leap<-regsubsets(lmform, data=rdata,nbest=1,nvmax = 11,really.big = T)

summary.out <- summary(leaps_seqrep)
leapsvars <- unlist(as.list(names(which(summary.out$which[which.max(summary.out$adjr2),]==TRUE))))
leapsvars

leapfit <- lm(no2_adj ~ elev +
                rl_1000 +
                rlm_500 +
                bl_300 +
                intinvnear1 + 
                distintvrail1 +
                #nt_1000 + 
                #ug_500 +
                distintvmine1 
                #pop_300
              ,data=rdata_multi)
summary(leapfit)

no2adj_A_M <- lm(no2_adj ~ elev +
                rl_1000 +
                rlm_500 +
                bl_300 +
                intinvnear1 + 
                distintvrail1 +
                #nt_1000 + 
                #ug_500 +
                distintvmine1 
              #pop_300
              ,data=rdata_multi)

#check for multicollinearity
vif(leapfit) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata_multi)-length(leapfit$coefficients)-2)) 
plot(leapfit, which=4, cook.levels=cutoff, labels.id = rdata_multi$hhid)


# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(no2_adj ~ elev +
                 rl_1000 +
                 rlm_500 +
                 bl_300 +
                 intinvnear1 + 
                 distintvrail1 +
                 distintvmine1 
               ,data=rdata_multi, trControl=train_control, method="lm")
# summarize results
print(model)
fitno2<-summary(leapfit) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_A_M<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_A_M)[2]<-"no2_r_A_M"
no2_r_A_M <- dplyr::select(no2_r_A_M, hhid, no2_r_A_M)

keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(leapfit)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(leapfit, labels.id = rdata_multi$hhid)

# # # #More than 1 sampling period for 1987 # # # #
####Logged values

preds <- do.call(paste, c(as.list(varlist), sep=" + ", collapse=NULL))

lmform <- as.formula(paste("log(no2_adj) ~ elev + ", preds, sep="", collapse=NULL))

# (4) Enter allremainingvariables intoastepwise linear regression.
# (5) Remove from the available pool any variables that have
#       (a) insignificant t-statistics (R)0.05) and/or (b) coefficients that are inconsistent with a priori assumptions.

fit<-lm(lmform, data=rdata_multi)
summary(fit)

leaps_seqrep<-regsubsets(log(no2_adj) ~ elev + rl_25 + rl_50 + rl_100 + rl_300 + rl_500 + rl_1000 + rlm_25 + rlm_50 + rlm_100 + rlm_300 + rlm_500 + ll_300 + ll_500 + ll_1000 + bl_25 + bl_50 + bl_100 + bl_300 + bl_500 +  trafnear + intinvnear1 + intinvnear2 + trafmajor + distintvmajor1 + distintvmajor2 + intmajorinv1 + intmajorinv2 + tl_25 + tl_50 + tl_100 + tl_300 + tl_500 + tl_1000 + tlm_25 + tlm_50 + tlm_100 + tlm_300 + tlm_500 + tlm_1000 + distintvair1 + distintvair2 + distintvrail1 + distintvrail2 + distintvrailyard1 + distintvrailyard2 + distintvbusrt1 + distintvbusrt2 +  ag_100 + ag_300 + ag_500 + ag_1000 + ag_5000 + cm_100 + cm_300 + cm_500 + cm_1000 + cm_5000 + lr_100 + lr_300 + lr_500 + lr_1000 + lr_5000 + nt_100 + nt_300 + nt_500 + nt_1000 + nt_5000 + ug_100 + ug_300 + ug_500 + ug_1000 + ug_5000 + distintvmine1 + distintvmine2 +  hh_100 + hh_300 + hh_500 + hh_1000 + hh_5000 + pop_100 + pop_300 + pop_500 + pop_1000 + pop_5000, data=rdata_multi,
                         nbest=1,
                         nvmax = 10,
                         method = "seqrep",
                         really.big = TRUE)

#leap<-regsubsets(lmform, data=rdata,nbest=1,nvmax = 11,really.big = T)

summary.out <- summary(leaps_seqrep)
leapsvars <- unlist(as.list(names(which(summary.out$which[which.max(summary.out$adjr2),]==TRUE))))
leapsvars

leapfit <- lm(log(no2_adj) ~ elev +
                #rl_300 +
                rl_1000 +
                rlm_300 +
                ll_300 +
                bl_500 +
                #intinvnear1 +
                cm_5000 +
                #ug_500 +
                distintvmine1
              ,data=rdata_multi)
summary(leapfit)

no2adj_lA_M <- lm(log(no2_adj) ~ elev +
                rl_1000 +
                rlm_300 +
                ll_300 +
                bl_500 +
                #intinvnear1 +
                cm_5000 +
                #ug_500 +
                distintvmine1
              ,data=rdata_multi)

#check for multicollinearity
vif(leapfit) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed
#based on ESCAPE protocol

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(rdata_multi)-length(leapfit$coefficients)-2)) 
plot(leapfit, which=4, cook.levels=cutoff, labels.id = rdata_multi$hhid)



# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(log(no2_adj) ~ elev +
                 rl_1000 +
                 rlm_300 +
                 ll_300 +
                 bl_500 +
                 cm_5000 +
                 distintvmine1
               ,data=rdata_multi, trControl=train_control, method="lm")
# summarize results
print(model)
fitno2<-summary(leapfit) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r_lA_M<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r_lA_M)[2]<-"no2_r_lA_M"
no2_r_lA_M <- dplyr::select(no2_r_lA_M, hhid, no2_r_lA_M)

keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
plot.lm(leapfit)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(leapfit, labels.id = rdata_multi$hhid)

#### Calculate AIC for all models ####
AIC(no2adj_E_A)
AIC(no2adj_lE_A)
AIC(no2adj_E_M)
AIC(no2adj_lE_M)
AIC(no2adj_A_A)
AIC(no2adj_lA_A)
AIC(no2adj_A_M)
AIC(no2adj_lA_M)

#### Write out residuals ####
no2resids <- data.frame(rdata)

#use reduce function
no2resids<-Reduce(function(x,y) merge(x,y, all=TRUE),
              list(no2resids,
                   no2_r_E_A,
                   no2_r_lE_A,
                   no2_r_E_M,
                   no2_r_lE_M,
                   no2_r_A_A,
                   no2_r_lA_A,
                   no2_r_A_M,
                   no2_r_lA_M
              ))

#### Plot observed vs predicted ####
# Plot observed vs predicted
ggplot(data = rdata, aes(x = rdata$no2_adj, y = fitted(no2adj_E_A), label = rdata$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata, aes(x = log(rdata$no2_adj), y = fitted(no2adj_lE_A), label = rdata$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata_multi, aes(x = rdata_multi$no2_adj, y = fitted(no2adj_E_M), label = rdata_multi$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata_multi, aes(x = log(rdata_multi$no2_adj), y = fitted(no2adj_lE_M), label = rdata_multi$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata, aes(x = rdata$no2_adj, y = fitted(no2adj_A_A), label = rdata$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata, aes(x = log(rdata$no2_adj), y = fitted(no2adj_lA_A), label = rdata$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata_multi, aes(x = rdata_multi$no2_adj, y = fitted(no2adj_A_M), label = rdata_multi$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()

ggplot(data = rdata_multi, aes(x = log(rdata_multi$no2_adj), y = fitted(no2adj_lA_M), label = rdata_multi$hhid)) +
  geom_point() +
  geom_abline() +
  geom_text()


#### Mixed-effects model with best fit model (alt approach, log outcome, multiple sampling) ####
data<-subset(p5all,hhid<5001)

data <- dplyr::select(data, hhid, styear,stmon,stssn,no2ppb)

merdata <- merge(data,pdata,c("hhid"))
merdata <- filter(merdata, hhid!=1354 & hhid!=1202)

#create a cold, hot, middle month seasonal distro
merdata$stssn2 <- ifelse(merdata$stssn == 1, 2,
                         ifelse(merdata$stssn == 3, 1, merdata$stssn))

merdata$stssn2 <- factor(merdata$stssn2)

#subset out 1987 values only
merdata <- filter(merdata, styear==87)

model1 <- lmer(log(no2ppb) ~ elev +
                 rl_1000 +
                 rlm_300 +
                 #ll_300 +
                 bl_500 +
                 intinvnear1 +
                 cm_5000 +
                 distintvmine1 +
                factor(stssn) +
                (1 | hhid)
               ,data=merdata)
summary(model1)
AIC(model1)

###### FINAL Mixed model ########
model4 <- lmer(log(no2ppb) ~ elev +
                 rl_1000 +
                 rlm_300 +
                 #ll_300 +
                 bl_500 +
                 intinvnear1 +
                 cm_5000 +
                 distintvmine1 +
                 factor(stssn2) +
                 (1 | hhid)
               ,data=merdata)
summary(model4)
AIC(model4)

no2_r_lM_A <- residuals(model4)

no2_r_lM_A<-merge(no2_r_lM_A,merdata, by=c("row.names"), all=T)
names(no2_r_lM_A)[2]<-"no2_r_lM_A"
no2_r_lM_A <- dplyr::select(no2_r_lM_A, hhid, no2_r_lM_A)



###### FINAL Mixed model ########


model2 <- lmer(log(no2ppb) ~ elev +
                 rl_1000 +
                 rlm_300 +
                 #ll_300 +
                 bl_500 +
                 intinvnear1 +
                 cm_5000 +
                 distintvmine1 +
                 factor(stmon) +
                 (1 | hhid)
               ,data=merdata)
summary(model2)
AIC(model2)

model3 <- lmer(log(no2ppb) ~ elev +
                 rl_1000 +
                 rlm_300 +
                 #ll_300 +
                 bl_500 +
                 intinvnear1 +
                 cm_5000 +
                 distintvmine1 +
                 (1 | hhid)
               ,data=merdata)
summary(model3)
AIC(model3)

anova(model1, model2, model3)
AIC(model1)
AIC(model2)
AIC(model3)

summary(model1)

#check for multicollinearity
#vif(leapfit) # problem?
#NOTE if vif>3, then exclude from model, starting with largest VIF first if needed
#based on ESCAPE protocol

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(merdata)-length(model$coefficients)-2)) 
plot(model, which=4, cook.levels=cutoff, labels.id = merdata$hhid)



# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(log(no2ppb) ~ elev +
                 rl_1000 +
                 rlm_300 +
                 ll_300 +
                 bl_500 +
                 intinvnear1 +
                 cm_5000 +
                 distintvmine1 +
                 factor(stssn) +
                 (1 | hhid)
               ,data=merdata, trControl=train_control, method="lm")
# summarize results
print(model)
fitno2<-summary(model) #final model

#pull out residuals
attributes(fitno2)
fitno2.r<-fitno2$residuals

no2_r<-merge(fitno2.r,rdata, by=c("row.names"), all=T)
names(no2_r)[2]<-"no2_r_lM_all"

keep<-c(2:3)
no2_r<-no2_r[,keep]

#summary(gvlma(fitno2))
#plot.lm(leapfit)

#crPlots(fitno2)

par(mfrow = c(2, 2))
plot(model, labels.id = merdata$hhid)




#### PM2.5 LUR IN PROGRESS ####
#PM2.5

summary(lm(pm25_adj~
             distintvair2 +
             ug_500 +
             rlm_100 +
             elev +
             bl_1000
           ,data=rdata))


#remove least significant predictors 1 by 1 until all have pvalue<0.1

summary(lm(pm25_adj~
             distintvair2 
#             ug_500  
#             rlm_100 +
#             elev
           ,data=rdata))


#final model for leave 1 out cross validation
pm25final<-lm(pm25_adj~distintvair2, data=rdata)

# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(pm25_adj~
                 distintvair2
               ,data=rdata, trControl=train_control, method="lm")
# summarize results
print(model)

#final model
pm25final<-lm(pm25_adj~distintvair2, data=rdata)

fitpm25<-summary(pm25final) #final model

#pull out residuals
attributes(fitpm25)
fitpm25.r<-fitpm25$residuals

pm25_r<-merge(fitpm25.r,rdata, by=c("row.names"), all=T)
names(pm25_r)[2]<-"pm25_r"

keep<-c(2:3)
pm25_r<-pm25_r[,keep]

#summary(gvlma(fitpm25))
plot.lm(pm25final)

#crPlots(fitpm25)

par(mfrow = c(2, 2))
plot(pm25final)

#summary(gvlma(lm(pm25_adj~rlm_1000 + nt_1000 + hh_100 + tl_25+ tl_100, data=rdata)))
plot.lm(pm25final)

#### PM10 IN PROGRESS ####
#PM10

# 
# #PM10 Final model removing predictors with pvalue>0.1, startign with least sig pvalue first
# summary(lm(pm10_adj~distinvnear1 + distintvmajor2, data=rdata))
# 
# pm10final<-lm(pm10_adj~distinvnear1 + distintvmajor2, data=rdata)
# 
# fitpm10<-summary(pm10final) #final model
# 
# #pull out residuals
# attributes(fitpm10)
# fitpm10.r<-fitpm10$residuals
# 
# pm10_r<-merge(fitpm10.r,rdata, by=c("row.names"), all=T)
# names(pm10_r)[2]<-"pm10_r"
# 
# keep<-c(2:3)
# pm10_r<-pm10_r[,keep]
# 
# #summary(gvlma(fitpm10))
# plot.lm(pm10final)
# 
# #crPlots(fitpm10)
# 
# par(mfrow = c(2, 2))
# plot(pm10final)
# 
# #summary(gvlma(lm(pm10_adj~rlm_1000 + nt_1000 + hh_100 + tl_25+ tl_100, data=rdata)))
# plot.lm(pm10final)
# #crPlots(pm10final)

#### Merge residuals and write output ####

results1<-merge(rdata,no2_r, by=c("hhid"), all=T)
results<-merge(results1,pm25_r, by=c("hhid"), all=T)

results<-Reduce(function(x,y) merge(x,y, all=TRUE),
              list(rdata,
                   no2_r,
                   pm25_r
              ))

#results<-merge(results2,pm10_r, by=c("hhid"), all=T)

#correct residuals and concs by replacing NA to -99
#results[,1:76][is.na(results[,1:76])] <- -99

write.table(no2resids, "~/Dropbox/P5_TAPS_TEMP//Data/no2results.csv", sep=",", row.names=FALSE)

write.table(results, "~/Dropbox/P5_TAPS_TEMP//Data/results.csv", sep=",", row.names=FALSE)







