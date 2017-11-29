# clean data
# 1. To limit scope of this activity - filter out incidents that happened before 1.1.17
# 2. Strip out columns that are not necessary for this analysis

# source("getDetroitData.R")

library(dplyr)

# Filter dates > 1/1/17
# Blight violations
detBlightDate <- detBlight
detBlightDate$Violation.Date <- as.Date(detBlightDate$Violation.Date, "%m/%d/%Y")
detBlightDate <- detBlightDate %>% filter(Violation.Date >= '2017-01-01')

# demolitions
detDemDate <- detDemolitions
detDemDate$Demolition.Date <- as.Date(detDemDate$Demolition.Date, "%m/%d/%Y")
detDemDate <- detDemDate %>% filter(Demolition.Date >= '2016-01-01' & Demolition.Date < '2017-01-01')

# 311 incidents
det311Date <- det311
det311Date$ticket_created_date_time <- as.character(det311Date$ticket_created_date_time)
det311Date$ticket_created_date_time <- as.POSIXct(strptime(det311Date$ticket_created_date_time, "%m/%d/%Y %H:%M:%S %p"))
det311Date$ticket_created_date <- as.Date(det311Date$ticket_created_date_time)
det311Date <- det311Date %>% filter(ticket_created_date >= '2016-01-01' & ticket_created_date < '2017-01-01')

# Crime incidents post 12/6/16
detCrimeDate <- detCrime1216_pres
detCrimeDate$Incident.Date...Time <- as.character(detCrimeDate$Incident.Date...Time)
detCrimeDate$Incident.Date...Time <- as.POSIXct(strptime(detCrimeDate$Incident.Date...Time, "%m/%d/%Y %H:%M:%S %p"))
detCrimeDate$Incident.Date <- as.Date(detCrimeDate$Incident.Date...Time)
detCrimeDate <- detCrimeDate %>% filter(Incident.Date >= '2017-01-01')

# Crime incidents pre 12/6/16
detCrimeDate <- detCrime1909_1216
detCrimeDate$Incident.Date...Time <- as.character(detCrimeDate$INCIDENTDATE)
detCrimeDate$Incident.Date...Time <- as.POSIXct(strptime(detCrimeDate$Incident.Date...Time, "%m/%d/%Y %H:%M:%S %p"))
detCrimeDate$Incident.Date <- as.Date(detCrimeDate$Incident.Date...Time)
detCrimeDate <- detCrimeDate %>% filter(Incident.Date >= '2016-01-01' & Incident.Date < '2017-01-01')

# Remove columns that are not needed for analysis
dBlight <- detBlightDate %>% select(Violation.Latitude, Violation.Longitude)
dDemo <- detDemDate %>% select(Price, Commercial.Building, Latitude, Longitude, Neighborhood)
d311 <- det311Date %>% select(issue_type, lat, lng)
dCrime <- detCrimeDate %>% select(Offense.Category, Latitude, Longitude)

# verify structures
str(dBlight)
str(dDemo)
str(d311)
str(dCrime)

summary(dBlight) # contains NA lats and longs that will need omission.
summary(dDemo)
summary(d311)
summary(dCrime)


# remove date data
rm(det311Date, detBlightDate, detCrimeDate, detDemDate)

# rename columns 
colnames(dBlight) <- c("lat", "long")
colnames(dDemo) <- c("d.price","commercial", "lat", "long", "ng.hood")
colnames(d311) <- c("inc.type", "lat", "long")
colnames(dCrime) <- c("crm.type", "lat", "long")

# round lat long to 5 sig figs
dBlight[,sapply(dBlight, is.numeric)] <- as.data.frame(sapply(dBlight[,sapply(dBlight, is.numeric)], round, digits = 5))
dDemo[,sapply(dDemo, is.numeric)] <- as.data.frame(sapply(dDemo[,sapply(dDemo, is.numeric)], round, digits = 5))
d311[,sapply(d311, is.numeric)] <- as.data.frame(sapply(d311[,sapply(d311, is.numeric)], round, digits = 5))
dCrime[,sapply(dCrime, is.numeric)] <- as.data.frame(sapply(dCrime[,sapply(dCrime, is.numeric)], round, digits = 5))

# omit blight violations without gps lat and long
dBlight <- na.omit(dBlight)

# change dDemo$d.price to numeric
dDemo$d.price <- as.numeric(sub('$', "", as.character(dDemo$d.price), fixed = TRUE))

# remove "DPW - " and " - DPW USE ONLY" from incident factors in d311
a <- gsub("DPW - ", "", as.character(d311$inc.type))
a <- gsub(" - DPW USE ONLY", "", as.character(a))
a <- as.factor(a)
d311$inc.type <- a
