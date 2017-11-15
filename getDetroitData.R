# Get Detroit Map Data
# download csvs from https://data.detroitmi.gov/

# Detroit crime data from Dec 6th 2016 to present
detCrime1216_pres <- read.csv("https://data.detroitmi.gov/api/views/6gdg-y3kf/rows.csv?accessType=DOWNLOAD")

# 311 issues submission from Dec 3rd 2014 to present
det311 <- read.csv("https://data.detroitmi.gov/api/views/fwz3-w3yn/rows.csv?accessType=DOWNLOAD")

# Demolition permits from Jan 1st 2014 to present
detDemolitions <- read.csv("https://data.detroitmi.gov/api/views/rv44-e9di/rows.csv?accessType=DOWNLOAD")

# Blight violations (tickets) from 2006 - present
detBlight <- read.csv("https://data.detroitmi.gov/api/views/ti6p-wcg4/rows.csv?accessType=DOWNLOAD")
