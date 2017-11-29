# Mutate data
# 1. Add character id column
# 2. Group and count by character id
# 3. Join data sets on id
# 4. Add blight classification on id

# source("cleanData.R")

# add loc character id column for each data set

d311$loc.id <- gsub("[^0-9]", "", paste(d311$lat, d311$long, sep = ""))
dBlight$loc.id <- gsub("[^0-9]", "", paste(dBlight$lat, dBlight$long, sep = ""))
dCrime$loc.id <- gsub("[^0-9]", "", paste(dCrime$lat, dCrime$long, sep = ""))
dDemo$loc.id <- gsub("[^0-9]", "", paste(dDemo$lat, dDemo$long, sep = ""))
