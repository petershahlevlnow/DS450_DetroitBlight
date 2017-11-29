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
temp.gr.Crime <- dCrime[, 5:ncol(dCrime)] %>% group_by(loc.id) %>% summarise_all(funs(sum))
temp.gr.Blight <- dBlight %>% group_by(loc.id) %>% summarise(nBlight = n())

# No id grouping results from demo data, consider removing next line (i.e. same as dDemo)
temp.gr.Demo <- dDemo %>% select(loc.id, d.price, commercial) %>% group_by(loc.id) #%>% summarise_all(funs(sum))

# bind frequencies to temps
c <- d311[, 4:ncol(d311)] %>% group_by(loc.id) %>% summarise(n311 = n())
temp.gr.311 <- cbind(temp.gr.311, n311 = c$n311)

c <- dCrime[, 5:ncol(dCrime)] %>% group_by(loc.id) %>% summarise(nCrime = n())
temp.gr.Crime <- cbind(temp.gr.Crime, nCrime = c$nCrime)
                                                                                           
# create master list of unique loc.ids with lat and long from all dfs
unique.loc <- d311 %>% select(loc.id, lat, long)
unique.loc <- rbind(unique.loc, dBlight %>% select(loc.id, lat, long))
unique.loc <- rbind(unique.loc, dCrime %>% select(loc.id, lat, long))
unique.loc <- rbind(unique.loc, dDemo %>% select(loc.id, lat, long))
unique.loc <- unique.loc %>% select(loc.id, lat, long) %>% group_by(loc.id, lat, long) %>% count()

# join all temp dataset columns to unique.loc - master list of unique incident locations.
a <- merge(unique.loc, temp.gr.311, by.x = "loc.id", by.y = "loc.id", all.x = TRUE)
a <- merge(a, temp.gr.Crime, by.x = "loc.id", by.y = "loc.id", all.x = TRUE)
a <- merge(a, temp.gr.Demo, by.x = "loc.id", by.y = "loc.id", all.x = TRUE)
a <- merge(a, temp.gr.Blight, by.x = "loc.id", by.y = "loc.id", all.x = TRUE)

# join neighborhood to locations
d <- dCrime %>% select(ng.hood, loc.id) %>% group_by(ng.hood, loc.id) %>% count()
a <- merge(a, d[, c("ng.hood", "loc.id")], by.x = "loc.id", by.y = "loc.id", all.x = TRUE)
detAll <- a

# clean up final data

# rename columns

# remove uncategorized crime incidents (lat = 32.... long = -127)

# rm data that's not needed past this point
rm(a, c, d, a_p, temp.gr.311, temp.gr.Blight, temp.gr.Crime, temp.gr.Demo, unique.loc)
rm(d311, dBlight, dCrime, dDemo)
rm(detCrime1909_1216)
