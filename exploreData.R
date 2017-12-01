# explore data
# 1. plot frequencies of incidents on a google map

source("mutationsData.R")

# get google map
library(ggplot2)
library(ggmap)
library(reshape2)
library(gridExtra)

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

# plot contour density on map

detMap.den.crime <- ggmap(detMap) + stat_density2d(data = melt.detAll %>% filter(variable == "nCrime"), 
                                         aes(x = long, y = lat), bins = 30)

# plot raster 
# set color ramp
colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))

detMap.den.crime <- ggmap(detMap) + stat_density2d(data = sample_n(melt.detAll %>% filter(variable == "nCrime"), 5000), 
                                                   aes(x = long, y = lat, fill = ..density..),  
                                                   geom = "tile", contour = FALSE, alpha = 0.3) +
                                    scale_fill_gradientn(colours=colfunc(400)) + ggtitle("Criminal Incidents 2016")

detMap.den.311 <- ggmap(detMap) + stat_density2d(data = sample_n(melt.detAll %>% filter(variable == "n311"), 5000), 
                                                 aes(x = long, y = lat, fill = ..density..),  
                                                 geom = "tile", contour = FALSE, alpha = 0.3) +
                                  scale_fill_gradientn(colours=colfunc(400)) + ggtitle("311 Incidents 2016")

detMap.den.demo <- ggmap(detMap) + stat_density2d(data = sample_n(melt.detAll %>% filter(variable == "nDemo"), 5000), 
                                                 aes(x = long, y = lat, fill = ..density..),  
                                                 geom = "tile", contour = FALSE, alpha = 0.3) +
                                    scale_fill_gradientn(colours=colfunc(400)) + ggtitle("Demolitions 2016")

detMap.den.blight <- ggmap(detMap) + stat_density2d(data = sample_n(melt.detAll %>% filter(variable == "nBlight"), 5000), 
                                                  aes(x = long, y = lat, fill = ..density..),  
                                                  geom = "tile", contour = FALSE, alpha = 0.3) +
                                   scale_fill_gradientn(colours=colfunc(400)) + ggtitle("Blight Violations YTD 2017")


# detMap.den.crime
# detMap.den.311
# detMap.den.demo
# detMap.den.blight

grid.arrange(detMap.den.crime, detMap.den.311, detMap.den.demo, detMap.den.blight, nrow = 2, ncol = 2)
rm(a)
