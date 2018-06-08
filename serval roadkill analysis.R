# Code for analysing serval roadkill data on the N3

rm(list=ls()) # clears memory

library(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer") # import buffer of N3. Buffer is 40m, split into 42 N3 sections each 10 km long (section 42 is short)
roadkill <- readOGR(dsn = ".", layer = "roadkill") # load  roadkill records on N3 from 2014-2016

# plot road buffer and roadkill points to check they look right
plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

library(GISTools)
roadkillCount <- poly.counts(roadkill, buffer) # Count subset of roadkills within each 10km section of buffer & store as res

cov <- c(1:42) # create matrix m with 1 column showing 10k section number (1 to 42)

cov <- cbind(cov, roadkillCount) # add res as new column to matrix showing roadkill counts per 10km section 

colnames(cov) <- c("section", "roadkill") # rename columns of cov
cov <- as.data.frame(cov) # convert to dataframe
cov$section <- as.factor(cov$section) # converts section to factor
head(cov)


library(ggplot2)
plot_roadkill <- ggplot(data=cov, aes(x=section, y=roadkill)) +
  geom_bar(stat="identity")
plot_roadkill

#####################################################################################################
#####################################################################################################
#####################################################################################################

###########
# Capturing site covariates
###########

#####
# calculate mean mm of annual rainfall in each section
# used 10 random sample points from each 10km section of 40m N3 buffer
#####

# cov <- as.data.frame(c(1:42)) # create dataframe cov to hold site covariates, with 1 column showing the 10k section number (1 to 42)
# colnames(cov)[1] <- "section"

library(raster)
library(rgdal)

files <- list.files(path="/Users/SamWilliams/Downloads/wc2.0_30s_prec", pattern="tif$", full.names=TRUE) # select all the tiff files in the directory. This precipitation data in raster format is available from http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_30s_prec.zip
s <- stack(files) # stack all of them using r raster library
shp <- shapefile("random") # read the point shape file
ext <- extract(s, shp) # extract the values from raster stack.

annual.precip <- apply(ext,1, sum) # sum the total monthly rainfall per sample point to calculate total annual rainfall

annual.precip <- cbind(annual.precip, shp$X10k.sect) # add section number to matrix

mean.precip <- aggregate(annual.precip[,1], list(annual.precip[,2]), mean) # use aggregate to calculate mean precipitation value within each 10km section, and adds to mean.precip dataframe

colnames(mean.precip) <- c("section","precipitation") # changes column names

mean.precip

cov <- cbind(cov, mean.precip$precipitation) # adds precipitation to data frame containing site covariates
names(cov)[length(names(cov))] <- "precip" # renames last column
head(cov)


###

# Determine mode habitat per section (using new habitat raster from Lourens)
# Created a buffer of *10km* radius around road (road already divided into sections of 10km length)
# Created 100 random sampling points within each section, and sampled habitat type in each section

library(rgdal)
random100.pts <- readOGR(dsn = ".", layer = "100 random")

# # Use rpivotTable to check that each 10k section has 100 random sampling points
# library(rpivotTable)
# points100 <- as.data.frame(random100.pts)
# rpivotTable(points100)

files <- list.files(path="/Users/samwilliams/Desktop/Land cover/r3", pattern="tif$", full.names=TRUE) # load land cover raster conatining habitat data
s <- stack(files) # stack raster using r raster library
shp <- shapefile("100 random") # read the sampling point point shape file
ext <- extract(s, shp) # extract the values for habutat type at each sampling point from raster stack

df <- cbind(ext, shp$X10k.sect) # add section number to matrix

colnames(df) <- c("habitat","section") # changes column names
class(df)
df <- as.data.frame(df) # converst to data frame
class(df)
class(df$habitat) # habitat variable is currently numeric, but should be factor
class(df$section) # section variable is currently numeric, but should be factor
df$habitat <- as.factor(df$habitat) # change habitat variable to factor
df$section <- as.factor(df$section) # change section variable to factor
class(df$habitat) # check that habitat variable is now a factor
class(df$section) # check that section variable is now a factor
head(df) 
dim(df)

# create function to calculate the most common habitat type for the sampling points in each section
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

habitat <- aggregate(habitat ~ section, df, Mode) # calculate the most common habitat type  for the sampling points inb each section
head(habitat)
dim(habitat)

cov <- cbind.data.frame(cov, habitat$habitat) # adds habitat to data frame containing covariates
names(cov)[length(names(cov))]<- "habitat" # renames last column
head(cov)


###

# Use new raster from Lourens to calculate proportion of 10km buffer for  eack 10km section made up of wetland 

# Counting land use pixels within each 10km buffer aection
wetlandRaster <- raster("/Users/samwilliams/Desktop/Land cover/wetland.tif") # import wetland raster
plot(wetlandRaster)
summary(wetlandRaster) # has 1s (wetland) and 0s (everything else). Need to change 0s to NA
values(wetlandRaster)[values(wetlandRaster) < 1] = NA # Changes to NA
summary(wetlandRaster) # looks like 0s have been replaced with NAs

bufferSplit <- shapefile("/Users/samwilliams/Google Drive/Synced/Serval roadkill paper/GIS/10km buffer split 2.shp") # import 10km buffer split into 10km sections
head(bufferSplit) # looks lke bufferSplit is not ordered by section
bufferSplit <- bufferSplit[order(bufferSplit$X10k.sect),] # sort bufferSplit by section, to avoid problems later by calculating covariates using buffers with inconsistent ordering
head(bufferSplit) # looks better

projection(bufferSplit) # check that the projections of the buffer and raster match
projection(wetlandRaster)

temp <- extract(wetlandRaster, bufferSplit, fun=function(x, ...) length(na.omit(x))/length(x)) # caclulates proportion of raster cells that have value of 1 wihin each buffer section (1 representing wetland in this case, and NA representing everything else)
head(temp)

cov <- cbind.data.frame(cov, temp) # adds wetland to covariates
names(cov)[length(names(cov))]<- "wetland" # renames last column
head(cov)

plot_wetland <- plot_waterPermanent <- ggplot(data = cov, aes(x = section, y = wetland)) +
  geom_bar(stat="identity")
plot_wetland

###

# Use new raster from Lourens to calculate proportion of 10km buffer for  eack 10km section made up of water permanent 

# Counting land use pixels within each 10km buffer aection
waterPermanentRaster <- raster("/Users/samwilliams/Desktop/Land cover/water permanent.tif") # import water permanent raster
plot(waterPermanentRaster)
summary(waterPermanentRaster) # has 1s (wetland) and 0s (everything else). Need to change 0s to NA
values(waterPermanentRaster)[values(waterPermanentRaster) < 1] = NA # Changes to NA
summary(waterPermanentRaster) # looks like 0s have been replaced with NAs

projection(bufferSplit) # check that the projections of the buffer and raster match
projection(waterPermanentRaster)

temp <- extract(waterPermanentRaster, bufferSplit, fun=function(x, ...) length(na.omit(x))/length(x)) # caclulates proportion of raster cells that have value of 1 wihin each buffer section (1 representing water permnent in this case, and NA representing everything else)
head(temp)

cov <- cbind.data.frame(cov, temp) # adds water permanent to covariates
names(cov)[length(names(cov))]<- "waterPermanent" # renames last column
head(cov)

plot_waterPermanent <- ggplot(data = cov, aes(x = section, y = waterPermanent)) +
  geom_bar(stat="identity")
plot_waterPermanent

###

# Use new raster from Lourens to calculate proportion of 10km buffer for  eack 10km section made up of water seasonal 

# Counting land use pixels within each 10km buffer aection
waterSeasonalRaster <- raster("/Users/samwilliams/Desktop/Land cover/water seasonal.tif") # import water seasonal raster
plot(waterSeasonalRaster)
summary(waterSeasonalRaster) # has 1s (wetland) and 0s (everything else). Need to change 0s to NA
values(waterSeasonalRaster)[values(waterSeasonalRaster) < 1] = NA # Changes to NA
summary(waterSeasonalRaster) # looks like 0s have been replaced with NAs

plot(bufferSplit, add = TRUE)

projection(bufferSplit) # check that the projections of the buffer and raster match
projection(waterSeasonalRaster)

temp <- extract(waterSeasonalRaster, bufferSplit, fun=function(x, ...) length(na.omit(x))/length(x)) # caclulates proportion of raster cells that have value of 1 wihin each buffer section (1 representing water permnent in this case, and NA representing everything else)
head(temp)

cov <- cbind.data.frame(cov, temp) # adds water permanent to covariates
names(cov)[length(names(cov))]<- "waterSeasonal" # renames last column

head(cov)
create_report(cov)

plot_waterSeasonal <- ggplot(data = cov, aes(x = section, y = waterSeasonal)) +
  geom_bar(stat="identity")

library(cowplot)
plot_grid(plot_roadkill,  plot_wetland, plot_waterPermanent, plot_waterSeasonal)

###

# Find mean distance to rivers within each 10km section 

library(rgdal)
points <- readOGR(dsn = ".", layer = "randomUTM") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)

rivers <- readOGR(dsn = ".", layer = "gis.osm_waterways_free_1") # these data on waterways available from http://download.geofabrik.de/africa/south-africa.html

require(rgeos)
shortest.dists <- numeric(nrow(points))
for (i in seq_len(nrow(points))) {
  shortest.dists[i] <- gDistance(points[i,], rivers)
}

head(shortest.dists)

shortest.dists <- cbind(shortest.dists, points$X10k.sect)
colnames(shortest.dists)[2] <- "section"

shortest.dists.rivers <- shortest.dists # saves distances to river for each sampling point for later analysis of minimum distance to EITHER river or waterbody

# mean.river.dist <- aggregate(shortest.dists[, 1], list(shortest.dists[,2]), mean) # calculates mean minimum distance to river within each 10km section 
# colnames(mean.river.dist) <- c("section","river.distance") # changes column names

# head(mean.river.dist)

# cov <- cbind(cov, mean.river.dist$river.distance) # adds mean distance to river to m
# names(cov)[length(names(cov))]<- "river.dist" # renames last column

# head(cov)
# write.csv(cov, file = "cov.csv") # export cov to cov.csv

# beep(2) # if want a beep when complete

###

# Find mean distance to waterbodies within each 10km section 

library(rgdal)
points <- readOGR(dsn = ".", layer = "randomUTM") # reads in random points shapefile (10 random points per 10km section in 40m buffer)

waterbodies <- readOGR(dsn = ".", layer = "waterbodies")

require(rgeos)
shortest.dists <- numeric(nrow(points))
for (i in seq_len(nrow(points))) {
  shortest.dists[i] <- gDistance(points[i,], waterbodies)
}

head(shortest.dists)

shortest.dists <- cbind(shortest.dists, points$X10k.sect)
colnames(shortest.dists)[2] <- "section"
shortest.dists.waterbodies <- shortest.dists # saves distances to river for each sampling point for later analysis of minimum distance to EITHER river or waterbody

# mean.waterbody.dist <- aggregate(shortest.dists[, 1], list(shortest.dists[, 2]), mean) # calculates mean distance to waterbody within each 10km section
# colnames(mean.waterbody.dist) <- c("section","waterbody.distance") # changes column names
# 
# head(mean.waterbody.dist)
# 
# cov <- cbind(cov, mean.waterbody.dist$waterbody.distance) # adds distance to waterbody to m 
# names(cov)[length(names(cov))]<- "waterbody.dist" # renames last column
# 
# head(cov)
# write.csv(cov, file = "cov.csv") # export cov to cov.csv
# 
# beep(2)

###

# Calculate the minimum distance to any water (EITHER river or waterbody) for each sampling point and take the the mean per 10km section 

water <- cbind(shortest.dists.rivers, shortest.dists.waterbodies)
water <- as.data.frame(water)
water$closest.water <- pmin( water[,1], water[,3] )

closest.water.df <- aggregate(water[, 5], list(water[, 2]), mean) # calculates mean distance to closest water within each 10km section
colnames(closest.water.df) <- c("section","water.dist") # changes column names

head(closest.water.df)

cov <- cbind(cov, closest.water.df$water.dist) # adds distance to waterbody to cov 
names(cov)[length(names(cov))]<- "water.dist" # renames last column

head(cov)

# write.csv(cov, file = "cov.csv") # export cov to cov.csv
# beep(2)

######

# Calculate mean road width

library(rgdal)
points <- readOGR(dsn = ".", layer = "randomUTM") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)
points.df <- as.data.frame(points)

library(raster)
mean.width <- aggregate(points.df[, 3], list(points.df[, 6]), mean) # cacluclates mean road width per section
colnames(mean.width) <- c("section","mean.road.width") # changes column names

cov <- cbind(cov, mean.width$mean.road.width) # adds road width to m
names(cov)[length(names(cov))]<- "road.width" # renames last column

head(cov)
library(beepr)
beep(2)

###



#####################
# Sample speed limits
#####################

# Determine mode habitat per section

library(rgdal)
random.pts <- readOGR(dsn = ".", layer = "random")
speed <- readOGR(dsn = ".", layer = "speed buffer") 

plot(speed)

a <- over(random.pts, speed, fn = NULL) # creates dataframe with attributes of speed limit layer for each random sampling point
a$section <- random.pts$X10k.sect # add section number

# now find most common speed limit within sampling points for each section
library(data.table)
b <-  setDT(a)[,list(speed={
  tt <- table(speed)
  names(tt)[which.max(tt)]
}),section]

cov <- cbind.data.frame(cov, b$speed) # adds speed to data frame containing capture histories
names(cov)[length(names(cov))]<- "speed" # renames last column
head(cov)

#######################
# Sample traffic volume
#######################

# Determine mode annual traffic volume per km per section

library(rgdal)
random.pts <- readOGR(dsn = ".", layer = "random")
traffic <- readOGR(dsn = ".", layer = "traffic buffer") 

plot(traffic)

a <- over(random.pts, traffic, fn = NULL) # creates dataframe with attributes of traffic layer for each random sampling point
a$section <- random.pts$X10k.sect # add section number

# now find most common traffic volume at sampling points for each section
library(data.table)
b <-  setDT(a)[,list(traffic={
  tt <- table(traffic)
  names(tt)[which.max(tt)]
}),section]

b$traffic <- as.numeric(b$traffic)

cov <- cbind.data.frame(cov, b$traffic) # adds traffic to data frame containing capture histories
names(cov)[length(names(cov))]<- "traffic" # renames last column
head(cov)

##########################################################
# Calculate number of infrastructure waypoints per section
##########################################################

library(rgdal)
infrastructure <- readOGR(dsn = "/Users/SamWilliams/Google Drive/Synced/Serval roadkill paper/GIS/", layer = "infrastructure") 

proj4string(infrastructure) # check CRS of infrastructure layer
proj4string(buffer) # check CRS of road buffer layer
bufferWGS84 <- spTransform(buffer, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # transform CRS of buffer to match infrastructure
proj4string(bufferWGS84) # check CRS of both laters matches

library(GISTools)
poly.counts(infrastructure, bufferWGS84) -> res # Count number of infrastructure points within each 10km section of buffer & store as res

cov <- cbind(cov, res) # adds infrastructure to covariate data frame
names(cov)[length(names(cov))]<- "infrastructure" # renames last column
head(cov)


######################################################
# Calculate number of guineafowl waypoints per section
######################################################

# Filtered guineafowl & owl roadkill data to just 2014-2016

library(rgdal)
guineafowl <- readOGR(dsn = "/Users/SamWilliams/Google Drive/Synced/Serval roadkill paper/GIS/", layer = "guineafowl") 

library(GISTools)
poly.counts(guineafowl, bufferWGS84) -> res # Count number of guinea fowl points within each 10km section of buffer & store as res

cov <- cbind(cov, res) # adds guinea fowl to covariate data frame
names(cov)[length(names(cov))]<- "guineafowl" # renames last column
head(cov)

##########################################################
# Calculate number of owl waypoints per section
##########################################################

# Filtered guineafowl & owl roadkill data to just 2014-2016

library(rgdal)
owls <- readOGR(dsn = "/Users/SamWilliams/Google Drive/Synced/Serval roadkill paper/GIS/", layer = "owls") 

plot(owls)

library(GISTools)
res <- poly.counts(owls, bufferWGS84) # Count number of owl points within each 10km section of buffer & store as res

cov <- cbind(cov, res) # adds infrastructure to covariate data frame
names(cov)[length(names(cov))]<- "owls" # renames last column
head(cov)

# write covariates to csv
write.csv(cov, file = "cov.csv") # export cov to cov.csv

create_report(cov)


###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
# Modelling



