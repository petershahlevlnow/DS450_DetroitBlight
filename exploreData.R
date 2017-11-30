# explore data
# 1. plot frequencies of incidents on a google map

source("mutationsData.R")

# get google map
library(ggplot2)
library(ggmap)
library(reshape2)

#  citation: 
#  D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL
#  http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

centers <- lapply(detAll[, c("lat", "long")], median)
detMap <- get_googlemap(center = c(lon = centers$long, lat = centers$lat), 
                        size = c(640, 640), 
                        scale = 1,
                        zoom = 11,
                        maptype = "roadmap")

# melt frequencies for plotting
melt.detAll <- melt(detAll[, c("lat", "long", "nCrime", "n311", "nDemo", "nBlight")], id = c("lat", "long"))

# plot frequencies on map
detMap.freq <- ggmap(detMap) + geom_point(data = sample_n(melt.detAll %>% group_by(variable), 1000), 
                                          aes(x = long, y = lat, color = variable, size = value),  alpha = 0.3)

detMap.freq

rm(a)
