###

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)

rm(list=ls()) 

# Load LUT data
p1 <- readOGR(dsn = "/Users/Sam/Dropbox/Currently working on/Small carnivores rodent ecosystem services/GIS", layer = "LUT generalised") # do not convert singlepart to multipart, else intersect encouters error 
temp <- p1$LUT # save LUT data
p1 <- as(p1, 'SpatialPolygons') # Remove attribute data. 
LUT <- SpatialPolygonsDataFrame(p1, data.frame(LUT=temp), match.ID=F) # Add LUT data back in. Not sure it was necessary to remove all attribute data then add back in the LUT data, but this fixed the errors I was having
head(LUT) # check looks ok
plot(LUT) # check looks ok

# Load all grid cells
p1 <- readOGR(dsn = "/Users/Sam/Dropbox/Currently working on/Small carnivores rodent ecosystem services/GIS", layer = "Grids combined")
temp <- p1$Gridnr # save grid cell numbers
p1 <- as(p1, 'SpatialPolygons') # Remove attribute data
grid <- SpatialPolygonsDataFrame(p1, data.frame(grid=temp), match.ID=F) # Add grid cell numbers back in. 

LUT <- spTransform(LUT, CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) # project LUT to UTM. Better for making measurements, although because will be using % this doesn't really matter.
grid <- spTransform(grid, CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) # project grid to UTM. Better for making measurements, although because will be using % this doesn't really matter.

# check layers match up 
plot(LUT)
plot(grid, add = TRUE)

# intersect from raster package
pi <- intersect(LUT, grid)

# Extract areas from polygon objects then attach as attribute
areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) {slot(x, 'area')}))
row.names(areas) <- sapply(pi@polygons, FUN=function(x) {slot(x, 'ID')})
# Combine attributes info and areas 
attArea <- spCbind(pi, areas)

# For each grid, get area per LUT type
results <- aggregate(area~grid + LUT, data=attArea, FUN=sum)
library(dplyr)
results <- arrange(results, grid, LUT) # sort by grid number then LUT
detach("package:dplyr", unload=TRUE) # unload dplyr, as functions it masks seem to intefere with re-running code above
results$percent <- (results$area / 900.795523119574) # Calculate percentages by dividing by area of each cell (in metres, divided by 100 to make precentages). Add to results
results$percent=round(results$percent,1) # round results to 1 dp
head(results)

# Now subset
cam.cells <- read.csv("cam.cells.csv") # read cells used
results.long.subset <- results[results$grid %in% cam.cells$Gridnr,] # Subset results to only cells Lourens used in occupancy analysis
results.long.subset$area = NULL
write.csv(results.long.subset, "results.long.subset.csv")


# library(tidyr)
# results.wide <- spread(results, key = LUT, value = percent) # transpose results to wide format
# 
# # Subset results to only cells Lourens used in occupancy analysis
# cam.cells <- read.csv("cam.cells.csv") # read cells used
# head(cam.cells)
# results.subset <- results.wide[results.wide$grid %in% cam.cells$Gridnr,] # Subset results to only cells Lourens used in occupancy analysis
# results.subset$area = NULL
# results.subset
# write.csv(results.subset, "results3.csv")



head(cam.cells)
grid.percents <- read.csv("grid.percents.csv")
head(grid.percents)

a <- merge(grid.percents, cam.cells, by.x="grid", by.y="Gridnr")
head(a)
write.csv(a, "a.csv")

covs <- read.csv("covs.csv")

# b contains a row for each cam, containing app percent LUT and other covariate data
b <- merge(a, covs, by.x="station", by.y="StationID", all=TRUE)
b$Gridnr = NULL
head(b)
write.csv(b, "b.csv")


# covariates3 <- read.csv("covariates3.csv")
# b2 <- merge(b, covariates3, by.x = "station", by.y = "StationID", all = TRUE)
# head(b2)
# write.csv(b2, "b2.csv")

# load list of camera trap stations at which each carnivore species was recorded
species.cams <- read.csv("species.cams.csv")
head(species.cams)

# filter species cam list to just first species (of 9 in total)
library(dplyr)
sp1 <- filter(species.cams, species.no == 1)
sp1

# create dataframe used in subesquent for loop. Contains a row for wach cam trap, with a column for each covariate. Now also includes new column showing on which cam trap station (there was only 1 station for species 1 was detected on only 1 station)
c <- merge(b, sp1, by.x = "station", by.y = "station", all = TRUE)
head(c)
c$i <- 1

# Now run for loop to do this for each of the remaining species in turn (saving as c.csv). This should be close to what Lourens wants. 
# for (i in 2:9){
#    sp <- filter(species.cams, species.no == i)
#    df <- merge(b, sp, by.x = "station", by.y = "station", all = TRUE)
#    c <- rbind(c,df)
# }
# head(c)
# write.csv(c, "c.csv")



for (i in 2:9){
        sp <- filter(species.cams, species.no == i)
        df <- merge(b, sp, by.x = "station", by.y = "station", all = TRUE)
        df$i <- i
        c <- rbind(c,df)
}
write.csv(c, "c.csv")
d <- merge(c, species.cams, by.x = "i", by.y = "species.no", all = FALSE)
head(c)
head(species.cams)

d <- merge(c, species.cams, by.x=c("species2", "station"), by.y=c("species", "StationID"))
