rm(list=ls())

require(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer")
roadkill <- readOGR(dsn = ".", layer = "roadkill")
roadkill$DATEYMD <- as.Date(roadkill$DATEYMD)

plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# counts roadkill in polygons overall
library(GISTools)
poly.counts(roadkill, buffer) -> res
m <- cbind(res)
m

# How to cbind results
# m <- cbind(m, res)
# m


# roadkill.df <- data.frame(roadkill) # saves roadkill as a dataframe so can subset it

# Convert to date numbers tofrom factors to integers (need to do this via characters)
# roadkill.df$DATENUM <- as.character(roadkill.df$DATENUM)
# roadkill.df$DATENUM <- as.integer(roadkill.df$DATENUM) 

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

startDate = 41639
endDate = 41648

roadkill.subset = subset(roadkill, roadkill$DATENUM > startDate & roadkill$DATENUM < endDate) #subsets to week 1 of 2014

m <- c(1:42) # create matrix with 1 column showing 10k section number (1 to 42)


poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within buffer
# res # check count looks correct
m <- cbind(m, res) # add new column to matrix showing roadkill counts per 10km section 
m

startDate = startDate + 7 # moves subset start date to start at beginning of second week
endDate = endDate + 7 # moves subset end date to end at second week

roadkill.subset = subset(roadkill, roadkill$DATENUM > startDate & roadkill$DATENUM < endDate) #subsets to week 2 of 2014

poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within buffer
# res # check count looks correct
m <- cbind(m, res) # add new column to matrix showing roadkill counts per 10km section 
m

###

# try for loop

rm(list=ls())

require(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer")
roadkill <- readOGR(dsn = ".", layer = "roadkill")
roadkill$DATEYMD <- as.Date(roadkill$DATEYMD)

plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

m <- c(1:42) # create matrix with 1 column showing 10k section number (1 to 42)

for (i in 41640:42728){
  startDate <- i
  endDate <- i + 6
  roadkill.subset = subset(roadkill, roadkill$DATENUM >= startDate & roadkill$DATENUM <= endDate) # subsets roadkill by week from first week in 2014
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within buffer & store as res

  m <- cbind(m, res) # add new column to matrix showing roadkill counts per 10km section 
}

write.csv(m, file = "m.csv") # save output to m.csv






###

# trying to skip 6 days of the week




m <- c(1:42) # create matrix with 1 column showing 10k section number (1 to 42)
for (i in 41640:42728) {
  if (i %% 7){
    next
  }
  startDate <- i
  endDate <- i + 6
  roadkill.subset = subset(roadkill, roadkill$DATENUM > startDate & roadkill$DATENUM < endDate) # subsets roadkill by week from first week in 2014
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within buffer & store as res
  
  m <- cbind(m, res) # add new column to matrix showing roadkill counts per 10km section 
}

write.csv(m, file = "m.csv") # save output to m.csv


###

for (i in d){
  startDate <- i
  print
  endDate <- i + 6
  roadkill.subset = subset(roadkill, roadkill$DATENUM >= startDate & roadkill$DATENUM <= endDate) # subsets roadkill by week from first week in 2014
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within buffer & store as res
  
  m <- cbind(m, res) # add new column to matrix showing roadkill counts per 10km section 
}

###
# try for loop

rm(list=ls())

d <- c(41640,41647,41654,41661,41668,41675,41682,41689,41696,41703,41710,41717,41724,41731,41738,41745,41752,41759,41766,41773,41780,41787,41794,41801,41808,41815,41822,41829,41836,41843,41850,41857,41864,41871,41878,41885,41892,41899,41906,41913,41920,41927,41934,41941,41948,41955,41962,41969,41976,41983,41990,41997,42004,42011,42018,42025,42032,42039,42046,42053,42060,42067,42074,42081,42088,42095,42102,42109,42116,42123,42130,42137,42144,42151,42158,42165,42172,42179,42186,42193,42200,42207,42214,42221,42228,42235,42242,42249,42256,42263,42270,42277,42284,42291,42298,42305,42312,42319,42326,42333,42340,42347,42354,42361,42368,42375,42382,42389,42396,42403,42410,42417,42424,42431,42438,42445,42452,42459,42466,42473,42480,42487,42494,42501,42508,42515,42522,42529,42536,42543,42550,42557,42564,42571,42578,42585,42592,42599,42606,42613,42620,42627,42634,42641,42648,42655,42662,42669,42676,42683,42690,42697,42704,42711,42718,42725)

require(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer")
roadkill <- readOGR(dsn = ".", layer = "roadkill")
roadkill$DATEYMD <- as.Date(roadkill$DATEYMD)

plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

m <- c(1:42) # create matrix with 1 column showing 10k section number (1 to 42)

for (i in d){
  startDate <- i
  endDate <- i + 6
  roadkill.subset = subset(roadkill, roadkill$DATENUM >= startDate & roadkill$DATENUM <= endDate) # subsets roadkill by week from first week in 2014 to last week in 2016
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within buffer & store as res
  
  m <- cbind(m, res) # add new column to matrix showing roadkill counts per 10km section 
}

write.csv(m, file = "m.csv") # save output to m.csv


####

# Create capture history
# Sampling events each 1 day long

rm(list=ls()) # clears memory

# Create object d containing start dates of surveys. One survey each week from beginning of 2014 to end of 2016. Dates formatted as general (41640 = 2014-01-01).
d <- seq(41640, 42735, 1) # seq creates a sequence of numbers

require(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer") # import buffer of N3. Buffer is 40m, split into 42 N3 sections each 10 km long (section 42 is short)
roadkill <- readOGR(dsn = ".", layer = "roadkill") # load  roadkill records on N3 from 2014-2016
roadkill$DATEYMD <- as.Date(roadkill$DATEYMD) # Formats date. No longer necessary as used another column for date formated as general to facilitate adding 

# plot road buffer and roadkill points to check they look right
plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

m <- c(1:42) # create matrix m with 1 column showing 10k section number (1 to 42)

for (i in d){
  startDate <- i
  endDate <- i
  roadkill.subset = subset(roadkill, roadkill$DATENUM >= startDate & roadkill$DATENUM <= endDate) # subsets roadkill by day from first day in 2014 to last day in 2016
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within each 10km section of buffer & store as res
  
  m <- cbind(m, res) # add res as new column to matrix showing roadkill counts per 10km section 
}

colnames(m) <- c("section", 1:1096) # rename columns of m
write.csv(m, file = "occupancy_history_1d.csv") # export m to occupancy_history.csv


###########

# Calculating the mean rainfall per section

rm(list=ls())
library(raster)
r <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_01.tif") # reads in precipitation raster

require(rgdal)
points <- readOGR(dsn = ".", layer = "random") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)

values <- extract(r, points) # extracts precipitation values for points
df <- cbind.data.frame(coordinates(points),values) # adds precipitation values to a dataframe
head(df)


df <- cbind(df, points$X10k.sect) # adds section names to each row
df <- cbind(df, points$No) # adds ID number to each row
colnames(df)[4] <- "section" # renames columns
colnames(df)[5] <- "id"
head(df)



mean.precip <- aggregate(df[, 3], list(df$section), mean) # calculates mean precipitation value within each 10km section and adds to mean.precip dataframe
colnames(mean.precip) <- c("section","mean monthly precipitation (mm)") # changes column names
mean.precip


######


# 12 precipitation rasters

rm(list=ls())

require(rgdal)
points <- readOGR(dsn = ".", layer = "random") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)

library(raster)
# import each of the 12 monthly rainfall rasters
r01 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_01.tif") 
r02 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_02.tif") 
r03 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_03.tif") 
r04 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_04.tif") 
r05 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_05.tif") 
r06 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_06.tif") 
r07 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_07.tif") 
r08 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_08.tif") 
r09 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_09.tif") 
r10 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_10.tif") 
r11 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_11.tif") 
r12 <- raster("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_12.tif") 

Rlist = factor(c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r07", "r08", "r09", "r10", "r11", "r12"))

Rlist = as.list(c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r07", "r08", "r09", "r10", "r11", "r12"))

r13 <- list(raster(c("/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_01.tif", "/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_02.tif", "/Users/Sam/Downloads/wc2.0_30s_prec/wc2.0_30s_prec_03.tif")))

class(Rlist)
Rlist
for(i in Rlist) {
  print(i)
}


# df <- data.frame(x=NA, y=NA, precipitation=NA, section=NA, id=NA, month=NA)[numeric(0), ] # create dataframe with named columns
# head(df)

# working up to here

i <-Rlist[1] 
i

for (i in Rlist) {
  i <- as.raster(i)
  values <- extract(i, points, method = 'simple') # extracts precipitation values for points
  
  df <- cbind.data.frame(coordinates(points),values) # adds precipitation values to a dataframe
  df <- cbind(df, points$X10k.sect) # adds section names to each row
  df <- cbind(df, points$No) # adds ID number to each row
  df$month <- rep(1,nrow(df)) # add month column
  
  df01 <- df


}

head(df01)


colnames(df)[3] <- "precipitation" # rename columns
colnames(df)[4] <- "section"
colnames(df)[5] <- "id"
colnames(df)[6] <- "month"

  
mean.precip <- aggregate(df[, 3], list(df$section), mean) # calculates mean precipitation value within each 10km section and adds to mean.precip dataframe
colnames(mean.precip) <- c("section","mean monthly precipitation (mm)") # changes column names

mean.precip

###


df <- data.frame(x=NA, y=NA, precipitation=NA, section=NA, id=NA, month=NA)[numeric(0), ] # create dataframe with named columns
df
class(df)

df <- rbind(df, values) # adds precipitation values to a dataframe
head(df)

y <- apply(X, 2, sum)


#####

# calculate mean mm of annual rainfall in each section
# used 10 random sample points from each 10km section of 40m N3 buffer

library(raster)
library(rgdal)

files <- list.files(path="/Users/Sam/Downloads/wc2.0_30s_prec", pattern="tif$", full.names=TRUE) # select all the tiff files in the directory
s <- stack(files) # stack all of them using r raster library
shp <- shapefile("random") # read the point shape file
ext <- extract(s, shp) # extract the values from raster stack.

annual.precip <- apply(ext,1, sum) # sum the total monthly rainfall per sample point to calculate total annual rainfall

annual.precip <- cbind(annual.precip, shp$X10k.sect) # add section number to matrix

mean.precip <- aggregate(annual.precip[,1], list(annual.precip[,2]), mean) # use aggregate to calculate mean precipitation value within each 10km section, and adds to mean.precip dataframe

colnames(mean.precip) <- c("section","precipitation") # changes column names

mean.precip


###

# Determine mode habitat per section

rm(list=ls())

library(rgdal)
random.pts <- readOGR(dsn = ".", layer = "random")
habitat <- readOGR(dsn = "/Users/Sam/Downloads/nvm2012beta2_wgs84_Geo", layer = "nvm2012beta2_wgs84_Geo")

a <- over(random.pts, habitat, fn = NULL) # creates dataframe with attributes of habitat layer for each random sampling point
a$section <- random.pts$X10k.sect # add section number

# now find most common habitat type within sampling points for each section
library(data.table)
b <-  setDT(a)[,list(NAME={
  tt <- table(NAME)
  names(tt)[which.max(tt)]
}),section]

m$habitat <- b$NAME

###

# Find mean distance to water bodies within each 10km section 

rm(list=ls())

library(rgdal)
points <- readOGR(dsn = ".", layer = "randomUTM") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)

waterbodies <- readOGR(dsn = ".", layer = "waterbodies")

require(rgeos)
shortest.dists <- numeric(nrow(points))
for (i in seq_len(nrow(points))) {
  shortest.dists[i] <- gDistance(points[i,], waterbodies)
}

head(shortest.dists)

shortest.dists <- cbind(shortest.dists, points$X10k.sect)
colnames(shortest.dists)[2] <- "section"

mean.waterbody.dist <- aggregate(shortest.dists[, 1], list(shortest.dists[,2]), mean) # calculates mean precipitation value within each 10km section and adds to mean.precip dataframe
colnames(mean.waterbody.dist) <- c("section","waterbody.distance") # changes column names

head(mean.waterbody.dist)

m <- cbind(m, mean.waterbody.dist$waterbidy.distance) # adds precipitation to data frame containing capture histories
colnames(m)[160] <- "waterbody.distance" # names column containing precipitation

###

# Calculate mean road width

library(rgdal)
points <- readOGR(dsn = ".", layer = "randomUTM") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)
points.df <- as.data.frame(points)

library(raster)
mean.width <- aggregate(points.df[, 3], list(points.df[, 6]), mean) # cacluclates mean road width per section
colnames(mean.width) <- c("section","mean.road.width") # changes column names

m <- cbind(m, mean.width$mean.road.width) # adds road width to m
colnames(m)[162] <- "road.width" # changes column name

head(m)


###

data <- data.frame(A=c(1,3,5),B=c(4,3,6),C=c(2,2,8),D=c(3,3,4))
head(data)
data$E <- pmin( data[,1], data[,2], data[,3] )

head(m)

# good 
m$water.dist <- pmin( m[,160], m[,161] )


###########################################

# Find mean distance to waterbodies within each 10km section 

library(rgdal)
points <- readOGR(dsn = ".", layer = "randomUTM") # reads in ramdom points shapefile (10 random points per 10km section in 40m buffer)

waterbodies <- readOGR(dsn = ".", layer = "waterbodies")

require(rgeos)
shortest.dists2 <- numeric(nrow(points))
for (i in seq_len(nrow(points))) {
  shortest.dists2[i] <- gDistance(points[i,], waterbodies)
}

head(shortest.dists2)

shortest.dists2 <- cbind(shortest.dists2, points$X10k.sect)
colnames(shortest.dists2)[2] <- "section"

mean.river.dist <- aggregate(shortest.dists2[, 1], list(shortest.dists2[,2]), mean) # calculates mean minimum distance to river within each 10km section 
colnames(mean.river.dist) <- c("section","river.distance") # changes column names

head(mean.river.dist)

n <- cbind(m, mean.river.dist$river.distance) # adds mean distance to river to m
colnames(n)[161] <- "waterbody.dist" # names column 

head(n)


###############################
###############################
# min all water
# aaa
water <- cbind(shortest.dists.rivers, shortest.dists.waterbodies)
water <- as.data.frame(water)
water$closest.water <- pmin( water[,1], water[,3] )

closest.water.df <- aggregate(water[, 5], list(water[, 2]), mean) # calculates mean distance to closest water within each 10km section
colnames(closest.water.df) <- c("section","water.dist") # changes column names

head(closest.water.df)

m <- cbind(m, closest.water.df$water.dist) # adds distance to waterbody to m 
colnames(m)[162] <- "water.dist" # names column
head(m)
beep(2)

######











