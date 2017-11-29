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

# extend out factors to dummy variables for counting

# d311 

library(dummies)
a <- dummy(d311$inc.type, sep = "")
colnames(a) <- gsub("inc.type", "", colnames(a), fixed = TRUE)
d311 <- cbind(d311, a)

# dCrime
a <- dummy(dCrime$crm.type, sep = "")
colnames(a) <- gsub("crm.type", "", colnames(a), fixed = TRUE)
dCrime <- cbind(dCrime, a)

# group columns on location id and sum incidents
temp.gr.311 <- d311[, 4:ncol(d311)] %>% group_by(loc.id) %>% summarise_all(funs(sum))
temp.gr.Blight <- dBlight %>% group_by(loc.id) %>% count()
temp.gr.Crime <- dCrime[, 4:ncol(dCrime)] %>% group_by(loc.id) %>% summarise_all(funs(sum))
# No id grouping results from demo data, consider removing next line (i.e. same as dDemo)
temp.gr.Demo <- dDemo %>% select(loc.id, d.price) %>% group_by(loc.id) %>% summarise_all(funs(sum))

# create master list of unique loc.ids with lat and long from all dfs
unique.loc <- d311 %>% select(loc.id, lat, long)
unique.loc <- rbind(unique.loc, dBlight %>% select(loc.id, lat, long))
unique.loc <- rbind(unique.loc, dCrime %>% select(loc.id, lat, long))
unique.loc <- rbind(unique.loc, dDemo %>% select(loc.id, lat, long))
unique.loc <- unique.loc %>% select(loc.id, lat, long) %>% group_by(loc.id, lat, long) %>% count()

# join all temp dataset columns to unique.loc - master list of unique incident locations.

# clean up final data

# rename columns

# remove uncategorized crime incidents (lat = 32.... long = -127)
