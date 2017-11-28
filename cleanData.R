# clean data
# 1. To limit scope of this activity - filter out incidents that happened before 1.1.17
# 2. Strip out columns that are not necessary for this analysis

library(dplyr)

# Filter dates > 1/1/17
# Blight violations
detBlightDate <- detBlight
detBlightDate$Violation.Date <- as.Date(detBlightDate$Violation.Date, "%m/%d/%Y")
detBlightDate <- detBlightDate %>% filter(Violation.Date >= '2017-01-01')

# demolitions
detDemDate <- detDemolitions
detDemDate$Demolition.Date <- as.Date(detDemDate$Demolition.Date, "%m/%d/%Y")
detDemDate <- detDemDate %>% filter(Demolition.Date >= '2017-01-01')

# 311 incidents
det311Date <- det311
det311Date$ticket_created_date_time <- as.character(det311Date$ticket_created_date_time)
det311Date$ticket_created_date_time <- as.POSIXct(strptime(det311Date$ticket_created_date_time, "%m/%d/%Y %H:%M:%S %p"))
det311Date$ticket_created_date <- as.Date(det311Date$ticket_created_date_time)
det311Date <- det311Date %>% filter(ticket_created_date >= '2017-01-01')

# Crime incidents
detCrimeDate <- detCrime1216_pres
detCrimeDate$Incident.Date...Time <- as.character(detCrimeDate$Incident.Date...Time)
detCrimeDate$Incident.Date...Time <- as.POSIXct(strptime(detCrimeDate$Incident.Date...Time, "%m/%d/%Y %H:%M:%S %p"))
detCrimeDate$Incident.Date <- as.Date(detCrimeDate$Incident.Date...Time)
detCrimeDate <- detCrimeDate %>% filter(Incident.Date >= '2017-01-01')

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

# remove date data
rm(det311Date, detBlightDate, detCrimeDate, detDemDate)

# 