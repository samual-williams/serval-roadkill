julian <- read.csv("julian.csv")
head(julian)
class(julian$date)
tjulian$date <- as.Date(julian$date)
class(julian$date)

julian$julian = julian(julian$date)
head(julian)
class(julian$julian)


# libraries for plotting 
library (ggmap       , quietly = T) 
library (ggplot2     , quietly = T) 
library (rgdal       , quietly = T) 
library (RgoogleMaps , quietly = T) 
library (raster) 
library(maptools)

#### first transform ser.area2 into lat long - this is the excluded area 
# Define CRS when importing 
ser.area2 <- readShapePoly('Habitat.utm36', delete_null_obj=TRUE, proj4string=CRS("+init=EPSG:32736")) 

# tranform CRS 
ser.area3 <- spTransform(ser.area2, CRS("+proj=longlat +datum=WGS84")) 
# Fortify data for plotting in ggmap 
data <- fortify(ser.area3) 
head(data) 
# Get mask points - use only one session, they are all the same 
mask.points  <-ser.mask2$Summer2015 
mask.points  <-as.data.frame(mask.points) 
coordinates (mask.points) <- c("x", "y") 
proj4string (mask.points) <- CRS("+init=EPSG:32736") 
mask.points2 <- spTransform(mask.points, CRS("+proj=longlat 
                                             +datum=WGS84")) 
data2        <- as.data.frame(mask.points2) 
head(data2) 
### traps 
t.xy<- as.data.frame(sec.tr) 
coordinates(t.xy) <- c("x", "y") 
proj4string(t.xy) <- CRS("+init=EPSG:32736") 
t.xy2 <- spTransform(t.xy, CRS("+proj=longlat +datum=WGS84")) 
data3<- as.data.frame(t.xy2) 

# grap a sattelite image; i used the centre of site as location 
location <- get_map(location = c(lon = 29.1562, lat = -26.5491), zoom 
                    = 12,  maptype="satellite", crop=FALSE) 
plot(location) 

#plot 
ggmap(location, zoom = 12, maptype = 'satellite') + 
  geom_polygon(aes(x = long, y = lat, group=group), data = data, 
               colour = 'black', fill = 'grey', alpha = .2, size = .8)+ 
  geom_point(aes(x = x, y = y), data = data2, 
             colour = 'black', fill = 'black', alpha = 1, size = 1)+ 
  geom_point(aes(x = x, y = y), data = data3, 
             colour = 'yellow', fill = 'black', alpha = 0.5, size = 3) 



########################################################
# Cut code
########################################################

# Create capture history 
# Sampling events each 7 days long

# Create object d containing start dates of surveys. One survey each week from beginning of 2014 to end of 2016. Dates formatted as general (41640 = 2014-01-01).
d <- seq(41640, 42725, 7) # seq creates a sequence of numbers from 41640, 42725 in increments of 7

library(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer") # import buffer of N3. Buffer is 40m, split into 42 N3 sections each 10 km long (section 42 is short)
roadkill <- readOGR(dsn = ".", layer = "roadkill") # load  roadkill records on N3 from 2014-2016

# plot road buffer and roadkill points to check they look right
plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

m <- c(1:42) # create matrix m with 1 column showing 10k section number (1 to 42)

for (i in d){
  startDate <- i
  endDate <- i + 6
  roadkill.subset = subset(roadkill, roadkill$DATENUM >= startDate & roadkill$DATENUM <= endDate) # subsets roadkill by week from first week in 2014 to last week in 2016
  
  library(GISTools)
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within each 10km section of buffer & store as res
  
  m <- cbind(m, res) # add res as new column to matrix showing roadkill counts per 10km section 
}

colnames(m) <- c("section", 1:156) # rename columns of m

# If want to convert capture history counts to capture history binary:
binary <- m[ , 2:157] # create subset of capture history counts to make binary
binary[binary > 0] = 1 # convert counts to binary
m <- c(1:42) # recreate dataframe containing just section names to hold section names and binary capture histories
m <- cbind(m, binary) # combine section names and binary capture histories
colnames(m)[1] <- "section" # rename column 1 to section 

# If want to write to csv file
write.csv(m, file = "hist_7d.csv") # export m to occupancy_history.csv

head(m)

library(beepr)
# beep(2) # if want a beep when complete

########

## Alternatively, if want daily capture history instead of weekly, can run this code instead of the above 

# rm(list=ls()) # clears memory

# Create object d containing start dates of surveys. One survey each week from beginning of 2014 to end of 2016. Dates formatted as general (41640 = 2014-01-01).
d <- seq(41640, 42735, 1) # seq creates a sequence of numbers

require(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer") # import buffer of N3. Buffer is 40m, split into 42 N3 sections each 10 km long (section 42 is short)
roadkill <- readOGR(dsn = ".", layer = "roadkill") # load  roadkill records on N3 from 2014-2016
roadkill$DATEYMD <- as.Date(roadkill$DATEYMD) # Formats date. No longer necessary as used another column for date formated as general to facilitate adding

# # plot road buffer and roadkill points to check they look right
plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM)

m <- c(1:42) # create matrix m with 1 column showing 10k section number (1 to 42)

library(GISTools) # for poly.counts function 

for (i in d){
  startDate <- i
  roadkill.subset = subset(roadkill, roadkill$DATENUM == startDate) # subsets roadkill by day from first day in 2014 to last day in 2016
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within each 10km section of buffer & store as res
  
  m <- cbind(m, res) # add res as new column to matrix showing roadkill counts per 10km section
}

colnames(m) <- c("section", 1:1096) # rename columns of m

# If want to convert capture history counts to capture history binary:
binary <- m[ , 2:1096] # create subset of capture history counts to make binary
binary[binary > 0] = 1 # convert counts to binary
m <- c(1:42) # recreate dataframe containing just section names to hold section names and binary capture histories
m <- cbind(m, binary) # combine section names and binary capture histories
colnames(m)[1] <- "section" # rename column 1 to section

# If want to write to csv file
write.csv(m, file = "hist_1d.csv") # export m to occupancy_history.csv

head(m)

library(beepr)
# beep(2) # if want a beep when complete

############
############
############

## Alternatively, if want monthly capture history instead of weekly, can run this code instead of the above 

# Create capture history
# Sampling events each 30 days long

# rm(list=ls()) # clears memory

# Create object d containing start dates of surveys. One survey each week from beginning of 2014 to end of 2016. Dates formatted as general (41640 = 2014-01-01).
d <- seq(41640, 42725, 30) # seq creates a sequence of numbers from 41640, 42725 in increments of 30

require(rgdal)
buffer <- readOGR(dsn = ".", layer = "buffer") # import buffer of N3. Buffer is 40m, split into 42 N3 sections each 10 km long (section 42 is short)
roadkill <- readOGR(dsn = ".", layer = "roadkill") # load  roadkill records on N3 from 2014-2016

# plot road buffer and roadkill points to check they look right
plot(buffer)
plot(roadkill, col="red" , add=TRUE)

# Format DATENUM as integer (via character)
roadkill$DATENUM <- as.character(roadkill$DATENUM)
roadkill$DATENUM <- as.integer(roadkill$DATENUM) 

m <- c(1:42) # create matrix m with 1 column showing 10k section number (1 to 42)

library(GISTools)
for (i in d){
  startDate <- i
  endDate <- i + 29
  roadkill.subset = subset(roadkill, roadkill$DATENUM >= startDate & roadkill$DATENUM <= endDate) # subsets roadkill by week from first week in 2014 to last week in 2016
  
  poly.counts(roadkill.subset, buffer) -> res # Count subset of roadkills within each 10km section of buffer & store as res
  
  m <- cbind(m, res) # add res as new column to matrix showing roadkill counts per 10km section 
}

colnames(m) <- c("section", 1:37) # rename columns of m

# If want to convert capture history counts to capture history binary:
binary <- m[ , 2:38] # create subset of capture history counts to make binary
binary[binary > 0] = 1 # convert counts to binary
m <- c(1:42) # recreate dataframe containing just section names to hold section names and binary capture histories
m <- cbind(m, binary) # combine section names and binary capture histories
colnames(m)[1] <- "section" # rename column 1 to section 

# If want to write to csv file
write.csv(m, file = "hist_30d.csv") # export m to occupancy_history.csv

head(m)

library(beepr)
# beep(2) # if want a beep when complete



##########################################################################################
# Now capuring site covariates
##########################################################################################

# Determine mode habitat per section (old habitat raster)

library(rgdal)
random.pts <- readOGR(dsn = ".", layer = "random")
habitat <- readOGR(dsn = "/Users/SamWilliams/Downloads/NVM2012_Wgs84_Geo_06072017", layer = "NVM2012_Wgs84_Geo_06072017") # this vegatation data is available from http://bgis.sanbi.org/SpatialDataset/Detail/18

a <- over(random.pts, habitat, fn = NULL) # creates dataframe with attributes of habitat layer for each random sampling point
a$section <- random.pts$X10k.sect # add section number

# now find most common habitat type within sampling points for each section
library(data.table)
b <-  setDT(a)[,list(NAME={
  tt <- table(NAME)
  names(tt)[which.max(tt)]
}),section]

cov <- cbind.data.frame(cov, b$NAME) # adds habitat to data frame containing capture histories
names(cov)[length(names(cov))]<- "habitat" # renames last column
head(cov)

# write.csv(cov, file = "cov.csv") # export cov to cov.csv
# beep(2) # if want a beep when complete



# use new land use raster from Lourens to calculate mean distance from sampling points to wetland

# check projection of land use raster and 100 random sampling points per 10km section (both clipped to a radious of 10km of road)
projection(shp) # the sampling points shapefile is in WGS84 - should convert to UTM for taking measurements 
shpUTM <- spTransform(shp, crs("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
projection(shpUTM) # looks correct now

projection(s) # the land use raster is in WGS84 - should convert to UTM for taking measurements 
library(raster)
sUTM <- projectRaster(s, crs = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
projection(sUTM) # looks correct now

pointsFromRaster <- rasterToPoints(sUTM, fun=function(x){x==3}) # convert raster cells with value "3" (wetland) to matrix
head(pointsFromRaster) # looks okay
class(pointsFromRaster) # it's a matrix, but we need to convert it to a dataframe that can be converted to SpatialPointsDataFrame. Can then use gDistance on this to measure the distance between sampling points and habitat features

xy <- pointsFromRaster[,c(1,2)] # extract xy coordinates from raster points, so that can use this to convert points created from habitat raster to SpatialPointsDataFrame

pointsDF <- as.data.frame(pointsFromRaster) # convert habitat points from raster to dataframe
class(pointsDF) # now habitat points are a dataframe

spdf <- SpatialPointsDataFrame(coords = xy, data = pointsDF,
                               proj4string = CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) # convert habitat points dataframe to SpatialPointsDataFrame

library(rgeos)
wetlandDist <- gDistance(shpUTM, spdf, byid = TRUE) # measure minimum distance between sampling points and wetland. This produces a huge matrix containing the distance between each samokiung point (columns) and each point representing wetland in the land use raster (rows)

library(Rfast)
mins <- colMins(wetlandDist, value = FALSE, parallel = FALSE) # caclulate minimum value (distance from sampling point to wetland) for each row (sampling point)
str(mins)


random100.pts$wetland <- mins # adds minimum distances to sampling points SpatialPointsDataFrame
random100df <- as.data.frame(random100.pts) # converts sampling points SpatialPointsDataFrame to dataframe

closestWetland <- aggregate(random100df[, 3], list(random100df[, 2]), mean) # calculates mean minimum distance to wetland for sampling poiunts within each 10km sections
colnames(closestWetland) <- c("section","wetland") # changes column names

cov <- cbind(cov, closestWetland$wetland) # adds distance to wetland to cov 
names(cov)[length(names(cov))]<- "wetland" # renames last column

head(cov)

library(DataExplorer)
cov$section <- as.factor(cov$section)
create_report(cov)
create_report(mins)
qplot(mins)

head(random100.pts)
library(maptools)
writeSpatialShape(random100.pts, "wetDist")
writeSpatialShape(spdf, "wetlandPoints")
projection(random100.pts)
projection(spdf)

###

# 
# # something is awry
# # create roadkill using big buffer
# projection(roadkill)
# projection(bufferSplit)
# 
# roadkillGeo <- spTransform(roadkill, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# projection(roadkillGeo)
# projection(bufferSplit)
# 
# plot(bufferSplit)
# plot(roadkillGeo, add = TRUE)
# 
# plot(buffer)
# plot(roadkill, add = TRUE)
# 
# dim(roadkill)
# dim(roadkillGeo)
# 
# library(GISTools)
# temp <- poly.counts(roadkill, bufferSplitUTM) 
# head(temp)
# 
# head(bufferSplitUTM)
# 
# projection(roadkill)
# projection(bufferSplitUTM)
# 
# bufferSplitUTM <- spTransform(bufferSplit, CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# head(temp)
# head(cov)
# 
# cov <- cbind.data.frame(cov, temp) 
# names(cov)[length(names(cov))]<- "roadkill2" # renames last column
# head(cov)
# 
# cov$roadkill2-cov$roadkill
# 
# library(DataExplorer)
# create_report(cov)
# 
# ggplot(data = cov, aes(x = roadkill, y = roadkill2)) +
#   geom_point()
# 
# ggplot(data = cov, aes(x = section, y = roadkill2)) +
#   geom_bar(stat = "identity")
# 
# ggplot(data = cov, aes(x = section, y = roadkill)) +
#   geom_bar(stat = "identity")
# 
# head(bufferSplit)
# head(buffer)
# 
# head(roadkill)
# 
# library(dplyr)
# arrange(bufferSplit, asc(X10k.sect))
# names(bufferSplit)
# 
# head(cov)
# tail(cov)
# 
# plot_roadkill
# 
# write.csv(cov, file = "temp.csv")
# 
# library(beepr)
# beep(2)
# head(cov)
# 
# library(ggplot2)
# ggplot(data = cov, aes(x = section, y = roadkill2)) +
#   geom_bar(stat = "identity")
# 
# sum(cov$roadkill)
# sum(cov$roadkill2)
# head(roadkill)
# dim(roadkill)
# summary(roadkill)
# str(roadkill)
# 
# head(cov$roadkill2)
# 
# cov$roadkill2 <- NULL
# 
# bufferSplit$X10k.sect

###

# For each sampling point calculate (area of nearest wetland cluster / distance to nearest wetland cluster). Then take mean for each section.

library(rgdal)
random.wetland <- readOGR(dsn = ".", layer = "random_Wetcluster UTM") # imports shapefile created using NNJoin to spatial join attributes from nearest wetland cluster to each sample point
head(random.wetland)
random.wetland.df <- as.data.frame(random.wetland) # convert to dataframe
random.wetland.df$wetWeight <- random.wetland.df$join_AREA / random.wetland.df$distance # divide area of wetland cluster by distance to that cluster for each sampling point & save as wetWeight (wetland weighted). Larger values correspond to larger or closer wetland clusters
head(random.wetland.df)


mean.wet <- aggregate(random.wetland.df[,25], list(random.wetland.df[,6]), mean) # calculate mean wetWeight for each section
head(mean.wet)
dim(mean.wet)
colnames(mean.wet) <- c("section","wetland") # changes column names

cov <- cbind(cov, mean.wet$wetland) # adds mean wetWeights to cov
names(cov)[length(names(cov))]<- "wetland" # renames last column
head(cov)

###

###############################
# Calculate mode land use type
###############################


randomUTM <- readOGR(dsn = ".", layer = "randomUTM")
names(randomUTM)

head(randomUTM)
class(randomUTM)
randomUTM <- as.data.frame(randomUTM)

# now find most common LUT within sampling points for each section
library(data.table)
b <-  setDT(randomUTM)[,list(LUT={
  tt <- table(LUT)
  names(tt)[which.max(tt)]
}),X10k.sect]

cov <- cbind.data.frame(cov, b$LUT) # adds LUT to data frame containing capture histories
names(cov)[length(names(cov))]<- "LUT" # renames last column

###

##########################################################
# Occupancy modelling
##########################################################

# comparing different sampling periods (1-day, 7-day and 30-day) using only 2014 serval roadkill data

# rm(list=ls()) 

# load packages
library(chron) 
library(reshape)
library(unmarked)
library(AICcmodavg)
library(MuMIn)
library(plyr)
library(beepr)

# Load detection history - 1 day sampling occasions
histAll <- read.csv("hist_1d.csv") # load detection histories 
histAll$X = NULL # drops unwanted column
histAll <- data.frame(histAll[,-1], row.names=histAll[,1]) # makes sections into row names 
hist.1d <- histAll[,1:30] # keeps only detection history for first 30 sampling occasions, so 1 month. I tried this for the first 365 sampling occasions so the sampling period would be the same as for the other capture histories (7 days and 30 days), but the null model wouldn't fit when I did this
head(hist.1d)

# Load detection history - 7 day sampling occasions
histAll <- read.csv("hist_7d.csv") # load detection histories 
histAll$X = NULL # drops unwanted column
histAll <- data.frame(histAll[,-1], row.names=histAll[,1]) # makes sections into row names 
hist.7d <- histAll[,1:52] # keeps only detection history for first 52 sampling occasions, so 1 year
head(hist.7d)

# Load detection history - 30 day sampling occasions
histAll <- read.csv("hist_30d.csv") # load detection histories 
histAll$X = NULL # drops unwanted column
histAll <- data.frame(histAll[,-1], row.names=histAll[,1]) # makes sections into row names 
hist.30d <- histAll[,1:12] # keeps only detection history for first 12 sampling occasions, so 1 year
head(hist.30d)

# Load covariates
cov <- read.csv("cov.csv") # load covariates
cov$X = NULL # remove unwanted column
cov <- cov[-42, ] # remove las row (site 42, the joburg end of N3) as it is shorter than the rest
cov$section <- as.factor(cov$section) # converts section name to factor
cov

cov.num<-cov[,sapply(cov,is.numeric)] # separate out the numeric variables for colinearity check and subsequent standatdisation
library(usdm)
vif(cov.num) # checks numeric covariates for colinearity. Remove any variables where VIF > 4. Here all VIFs are <4 so don't need to exclude any. 

# Now standardise the covariates 
library(vegan)
cov.std<-decostand(cov.num,method="standardize") # standardise the numeric variables
cov.fac<-cov[,sapply(cov,is.factor)]  # extract factor variables
covs<-data.frame(cov.fac, cov.std) # merge factors and standardised covariates back into a single dataframe
head(covs)

# Calculate naive occupancy for 30-day sampling occasions
naive.30d <- sum(apply(hist.30d, 1, sum) > 0)/nrow(hist.30d) 
naive.30d
# (Naive occupancy is same for for 1-day and all other sampling occasions)

############
# Next fit occupancy models using 1-day detection history for 2014

library(unmarked)
hist.1d <- hist.1d[1:41,] # remove row 42 (truncated last 10km section of N3)
um <- unmarkedFrameOccu(y=hist.1d,siteCovs= covs) # Create occupancy object

m0 <- occu(~1~1,um) # fit null nodel 
m0
# Now fit other models. d1-d4 have covariates on detection probability (p), o1-o3 have covariates on occupancy (psi). Haven't yet tried fitting models with more than one covariate on either p, psi, or both p & psi, or interactions 
d1<- occu(~habitat~1,um, starts=c(-1,0,0,0,0,0,0,0,0,0,0)) # I added the "starts" arugument to fix an error I was getting "Error: Hessian is singular.  Try providing starting values or using fewer covariates." This fixed it.
d1 # now fits without hessian error. 
d2 <- occu(~precip~1,um,control=list(maxit=1000, trace=TRUE, REPORT=1))
d2 # Model didn't converge, so added control=list(maxit=1000, trace=TRUE, REPORT=1), which eliminated the error
d3 <- occu(~water.dist~1,um)
d3 # no problems
d4 <- occu(~road.width~1,um)
d4 # no problems

o1 <- occu(~1~habitat,um, starts=c(1,0,0,0,0,0,0,0,0,0,0))
o1 # starts removed hessian error
o2 <- occu(~1~precip,um)
o2 # no problems
o3 <- occu(~1~water.dist,um, control=list(maxit=3000, trace=TRUE, REPORT=1))
o3 # Model didn't converge, so added control argument, which eliminated the error
o4 <- occu(~1~road.width,um, control=list(maxit=1000, trace=TRUE, REPORT=1))
o4 # Model didn't converge, so added control argument, which eliminated the error

# None of the covariates modelled had any effect on p or psi, so did not fit models that indluded more than one covariate. 

# Now compare models with null model using AIC
dlist <- fitList(Null = m0, d1=d1, d2=d2, d3=d3, d4=d4, o1=o1, o2=o2, o3=o3, o4=o4) 
selmod.1d <- modSel(dlist,nullmod="Null")
selmod.1d  

# So some of the models fitted, but some others had errors such as "Hessian is singular" or "Model did not converge", but after googling solutions I added some arguments that appeared to fix the problems.

# Will try longer sampling occasions to see if that improves model fit.

# None of the models are a better fit to the data than the null model. 

############

# Next fit models using 7-day detection history for 2014
# To avoid confusion, I will refer to psi (occupancy) as o, and p (detecion probability) as d, to avoid confusion with p for p-value.

library(unmarked)
hist.7d <- hist.7d[1:41.,] # remove row 42 (last, truncated, road section)
um <- unmarkedFrameOccu(y=hist.7d,siteCovs= covs) # Create occupancy object
m0 <- occu(~1~1,um) # fit null nodel 
m0 # Warning message: In sqrt(diag(vcov(obj))) : NaNs produced
d1<- occu(~habitat~1,um) 
d1 
d2 <- occu(~precip~1,um)
d2 # Estimate = -0.696, SE = 0.277, z =-2.51, P = 0.0121
d3 <- occu(~water.dist~1,um)
d3  
d4 <- occu(~road.width~1,um)
d4 # Warning message: In sqrt(diag(vcov(obj))) : NaNs produced

o1 <- occu(~1~habitat,um)
o1 # Warning message: In sqrt(diag(vcov(obj))) : NaNs produced
o2 <- occu(~1~precip,um)
o2 # Warning message: In function (object): Model did not converge. Try providing starting values or increasing maxit control argment.
o3 <- occu(~1~water.dist,um)
o3 # Warning message: In sqrt(diag(vcov(obj))) : NaNs produced
o4 <- occu(~1~road.width,um)
o4 # Warning message: In function (object): Model did not converge. Try providing starting values or increasing maxit control argment.

# As precip covariate appears to affect detection probability and occupancy independantly, fit a model where it affects both simultaneously 
m1 <- occu(~precip~precip,um)
m1 # Warning message: In sqrt(diag(vcov(obj))) : NaNs produced

# Now compare models with null model using AIC
dlist <- fitList(Null = m0,d1=d1,d2=d2,d3=d3,d4=d4,o1=o1,o2=o2,o3=o3,m1=m1) 
selmod.7d <- modSel(dlist,nullmod="Null")
selmod.7d  

# Lots of models had warnings. The "In sqrt(diag(vcov(obj))) : NaNs produced" warnings appear to be due to insufficient variation in d or o  

# precipitation may be important, but will try longer sampling occasions to see if that improves model fit


############

# Next fit models using 30-day detection history for 2014

library(unmarked)
hist.30d <- hist.30d[1:41,] # remove row 42 (last, truncated, road section)
um <- unmarkedFrameOccu(y=hist.30d,siteCovs= covs) # Create occupancy object

m0 <- occu(~1~1,um) # fit null nodel 
m0
d1<- occu(~habitat~1,um)
d1 
d2 <- occu(~precip~1,um)
d2 # estimate = -0.794 SE = 0.314 z = -2.53, P = 0.0115
d3 <- occu(~water.dist~1,um)
d3 
d4 <- occu(~road.width~1,um)
d4 

o1 <- occu(~1~habitat,um, starts=c(-1,0,0,0,0,0,0,0,0,0,0))
o1 # Hessian warning - added starts argument, which resolved this warning, but gave new error: "In sqrt(diag(vcov(obj))) : NaNs produced". So perhaps insuffient variation in occupany to model
o2 <- occu(~1~precip,um)
o2 # estimate = -1.421, SE = 0.724, z = -1.962, P = 0.0497
o3 <- occu(~1~water.dist,um)
o3 
o4 <- occu(~1~road.width,um, control=list(maxit=3000, trace=TRUE, REPORT=1))
o4 # Model didn't converge, so added control argument, which eliminated the warning

# the precip covariate affected d and o independantly, so fit a model in which with precip on both d and o. 
m1 <- occu(~precip~precip,um)
m1 

# Now compare models with null model using AIC
dlist <- fitList(Null = m0, d1=d1, d2=d2, d3=d3, d4=d4, o2=o2, o3=o3, m1=m1) 
selmod.30d <- modSel(dlist,nullmod="Null")
selmod.30d  

# o4 top model, but am suspicious due to error. 

# d2, o2, m1 (precip) are better than null. They have fairly big dAICs relative to o4, but that may be an artefact of the arguments added to resolve the convergence error for o4.

# Test model fit for these models 
library(AICcmodavg)
mb.gof.test(d2, nsim = 100) # P-value = 0.43, c-hat = 0.56
mb.gof.test(o2, nsim = 100) # P-value = 0.53, c-hat = 0.4 
mb.gof.test(m1, nsim = 100) # P-value = 0.42, c-hat = 0.28
# Models appear to fit well - no evidence of poor fit or overdispersion as p > 0.05 & c-hat < 1

# So precipitation seems to be important. Betas were negative for these models, so looks like greater annual rainfall is inversely proportional to detection probability & occupancy

# d4 next best model, but no better than null

#####################################################
# Comparing 1-day, 7-day and 30-day sammpling periods
#####################################################
# So 30-day sampling period seems much better than 1-day or 7-day. 
# Next will fit occupancy models using 30-day sampling periods for 2014, 2015 and 2016 roadkill data

#####################################################
# Fitting occupancy models using 30-day sampling periods for 2014, 2015 and 2016 roadkill data
#####################################################

# rm(list=ls()) 

# load packages
library(chron) 
library(reshape)
library(unmarked)
library(AICcmodavg)
library(MuMIn)
library(plyr)

source("TEAM library 1.7.R") # contains shrink function for consolidating detection histories from 1-day samapling occasions to (in this case) 30-day sampling occasions
# This is a slightly different approach to that used above, but should yield same results for 2014 as before, and be work better across multiple years (eg. makes it easier to deal with leap years)

# Load detection history - 1 day sampling occasions
histAll <- read.csv("hist_1d.csv") # load detection histories 
histAll$X = NULL # drops unwanted column
histAll <- histAll[-42, ] # remove last0 row (site 42, the joburg end of N3) as it is shorter than the rest
histAll <- data.frame(histAll[,-1], row.names=histAll[,1]) # makes sections into row names 
histAll.1d <- histAll
head(histAll.1d)

hist2014.1d <- histAll.1d[,1:360] # separates out 2014 data (first 360 days, drop remaining 5 days of year as not a multiple of 30)
hist2014.30d <- shrink(hist2014.1d,30) # shrinks this into 30-day samplind periods using function loaded in TEAM library
head(hist2014.30d)

hist2015.1d <- histAll.1d[,366:725] # separates out 2015 data (first 360 days, drop remaining 5 days of year as not a multiple of 30)
hist2015.30d <- shrink(hist2015.1d,30) # shrinks this into 30-day samplind periods using function loaded in TEAM library
head(hist2015.30d)

hist2016.1d <- histAll.1d[,731:1090] # separates out 2015 data (first 360 days, drop remaining 6 days of year (2016 is a leap year) as not a multiple of 30)
hist2016.30d <- shrink(hist2016.1d,30) # shrinks this into 30-day samplind periods using function loaded in TEAM library
head(hist2016.30d)

# Load covariates
cov <- read.csv("cov.csv") # load covariates
head(cov)
cov$X = NULL # remove unwanted column
cov <- cov[-42, ] # remove las row (site 42, the joburg end of N3) as it is shorter than the rest
cov$section <- as.factor(cov$section) # converts section name to factor

cov.num<-cov[,sapply(cov,is.numeric)] # separate out the numeric variables for colinearity check and subsequent standatdisation
library(usdm)
vif(cov.num) # checks numeric covariates for colinearity. Remove any variables where VIF > 4. Here all VIFs are <4 so don't need to exclude any. 

# Now standardise the covariates 
library(vegan)
cov.std<-decostand(cov.num,method="standardize") # standardise the numeric variables
cov.fac<-cov[,sapply(cov,is.factor)]  # extract factor variables
covs<-data.frame(cov.fac, cov.std) # merge factors and standardised covariates back into a single dataframe
head(covs)

#####################
# Occupancy modelling
#####################

######
# 2014
######
# 2014, 12 30-day sampling periods

# Calculate naive occupancy for 30-day sampling occasions
naive2014 <- sum(apply(hist2014.30d, 1, sum) > 0)/nrow(hist2014.30d) 
naive2014 # 0.3902439 

# Next fit occupancy models using 30-day detection history

library(unmarked)
um <- unmarkedFrameOccu(y=hist2014.30d,siteCovs= covs) # Create occupancy object
head(um)

um.df <- cbind(hist2014.30d, covs)
head(um.df) # just to visually check that the unmarked object looks okay

m0 <- occu(~1~1,um) # fit null nodel 
m0

# d models are models on detection 
d1<- occu(~habitat~1,um) 
d1 # no problems
d2 <- occu(~precip~1,um)
d2 # no problems. det(precip)psi(.): Estimate = -0.833 SE = 0.317 z = -2.63 P(>|z|) = 8.64e-03
d3 <- occu(~water.dist~1,um)
d3 # no problems
d4 <- occu(~road.width~1,um)
d4 # no problems
d5 <- occu(~wetland~1,um)
d5 # no problems
d6 <- occu(~speed~1,um) 
d6 # no problems
d7 <- occu(~LUT~1,um) 
d7 # no problems
d8 <- occu(~traffic~1,um) 
d8 # no problems
d9 <- occu(~infrastructure~1,um) 
d9 # no problems
d10 <- occu(~guineafowl~1,um) 
d10 # no problems. Estimate = 0.321, SE = 0.150, z = 2.14, P(>|z|) = 3.25e-02
d11 <- occu(~owls~1,um) 
d11 # no problems. Estimate = 0.405, SE = 0.405, z = 3.18, P(>|z|) = 1.50e-03

# Fitting models with multiple covariates on detection. Made up of models with individual covariated on detection that looked important
d12 <- occu(~precip+guineafowl~1,um) 
d12 # no problems
d13 <- occu(~precip+owls~1,um) 
d13 # no problems
d14 <- occu(~guineafowl+owls~1,um) 
d14 # no problems
d15 <- occu(~precip+guineafowl+owls~1,um) 
d15 # no problems

# o models are models on occupancy
o1 <- occu(~1~habitat,um, starts = c(1,1,0,0,0,0,0,0,0,0,0))
o1 # Starts argumnent added to resolve "Hessian is singular" error
o2 <- occu(~1~precip,um)
o2 # no problems. det(.)psi(precip): Estimate = -1.472, SE = 0.715, z = -2.059, P(>|z|) = 0.0395
o3 <- occu(~1~water.dist,um)
o3 # no probs
o4 <- occu(~1~road.width,um, control=list(maxit=3000, trace=TRUE, REPORT=1))
o4 # control argument removed convergence error
o5 <- occu(~1~wetland,um)
o5 # no problems
o6 <- occu(~1~speed,um) 
o6 # Warning message: In sqrt(diag(vcov(obj))) : NaNs produced
o7 <- occu(~1~LUT,um) 
o7 # no problems
o8 <- occu(~1~traffic,um) 
o8 # no problems
o9 <- occu(~1~infrastructure,um) 
o9 # no problems
o10 <- occu(~1~guineafowl,um) 
o10 # no problems
o11 <- occu(~1~owls,um) 
o11 # no problems
# didn't fit models with more than one covariate on occupancy, as only one model looked important for occupancy

# m models are models on both occupancy and detection 
m1 <- occu(~precip~precip,um)
m1 # no problems. precip was significant on det but not psi (-0.795 0.316  -2.52 1.19e-02)
m2 <- occu(~guineafowl~precip,um)
m2 # no problems
m3 <- occu(~owls~precip,um)
m3 # no problems
m4 <- occu(~precip+guineafowl~precip,um)
m4 # no problems
m5 <- occu(~precip+owls~precip,um)
m5 # no problems. precip -0.719 0.327  -2.20 2.80e-02; owls 0.308 0.133   2.31 2.08e-02
m6 <- occu(~guineafowl+owls~precip,um)
m6 # no problems. owls 0.369 0.144  2.56 1.03e-02


# Now compare models with null model using AIC
dlist <- fitList(Null=m0, d1=d1, d2=d2, d3=d3, d4=d4, d5=d5, d6=d6, d7=d7, d8=d8, d9=d9, d10=d10, d11=d11, d12=d12, d13=d13, d14=d14, d15=d15, o2=o2, o3=o3, o5=o5, o6=o6, o7=o7, o8=o8, o9=o9, o10=o10, o11=o11, m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6) # removed o1 & o4 due to errors
selmod.2014 <- modSel(dlist,nullmod="Null")
selmod.2014

# Top models 
#       nPars    AIC delta   AICwt cumltvWt    Rsq d covs                  o covs
# d13      4 167.89  0.00 0.23637     0.24 0.2837 ~precip+owls            ~1
# d15      5 168.05  0.16 0.21845     0.45 0.3159 ~precip+guineafowl+owls ~1
# m5       5 169.88  1.99 0.08733     0.54 0.2839 ~precip+owls            ~precip
# d14      4 170.17  2.29 0.07532     0.62 0.2418 ~guineafowl+owls        ~1
# d2       3 170.25  2.37 0.07244     0.69 0.2016 ~precip                 ~1
# m6       5 170.72  2.83 0.05748     0.75 0.2688 ~guineafowl+owls        ~precip
# m3       4 170.84  2.96 0.05390     0.80 0.2291 ~owls                   ~precip
# d12      4 170.88  2.99 0.05299     0.85 0.2284 ~precip+guineafowl      ~1
# d11      3 171.96  4.07 0.03089     0.89 0.1671 ~owls                   ~1
# m1       4 172.25  4.37 0.02665     0.91 0.2016 ~precip                 ~precip
# m4       5 172.88  4.99 0.01946     0.93 0.2283 ~precip+guineafowl      ~precip
# o2       3 173.46  5.58 0.01455     0.95 0.1354 ~1                      ~precip
# m2       4 174.15  6.26 0.01033     0.96 0.1631 ~guineafowl             ~precip
# d8       3 174.42  6.53 0.00903     0.97 0.1147 ~traffic                ~1

# So precipitation, owls & guineafowl appear to be important for detection, and precipitation important for occupancy. 

# Test model fit for top models
library(AICcmodavg)
mb.gof.test(d13, nsim = 100) # P-value = 0.64, c-hat = 0.28
mb.gof.test(d15, nsim = 100) # P-value = 0.73, c-hat = 0.34 
mb.gof.test(m5, nsim = 100) # P-value = 0.72, c-hat = 0.24
mb.gof.test(d14, nsim = 100) # P-value = 0.72, c-hat = 0.31
mb.gof.test(m6, nsim = 100) # P-value = 0.69, c-hat = 0.29
mb.gof.test(m3, nsim = 100) # P-value = 0.65, c-hat = 0.29
mb.gof.test(d12, nsim = 100) # P-value = 0.44, c-hat = 0.58
mb.gof.test(d11, nsim = 100) # P-value = 0.57, c-hat = 0.31
mb.gof.test(m1, nsim = 100) # P-value = 0.5, c-hat = 0.45
mb.gof.test(m4, nsim = 100) # P-value = 0.35, c-hat = 0.18
mb.gof.test(o2, nsim = 100) # P-value = 0.47, c-hat = 0.46
mb.gof.test(m2, nsim = 100) # P-value = 0.41, c-hat = 0.47
mb.gof.test(d8, nsim = 100) # P-value = 0.3, c-hat = 0.42
# Models all appear to fit well - no evidence of poor fit (p > 0.05) or overdispersion c-hat < 1)

# 2014 model average of all models within delta AIC of 7 of top model 
library(MuMIn)
best <- list(d13,d15,m5,d14,m6,m3,d12,d11,m1,m4,o2,m2,d8)
avgmod2014 <- model.avg(best, fit=T)
summary(avgmod2014)

######
# 2015
######
# 2015, 12 30-day sampling periods

# Calculate naive occupancy for 30-day sampling occasions
naive2015 <- sum(apply(hist2015.30d, 1, sum) > 0)/nrow(hist2015.30d) 
naive2015 # 0.195122

# Next fit occupancy models using 30-day detection history

library(unmarked)
um <- unmarkedFrameOccu(y=hist2015.30d,siteCovs= covs) # Create occupancy object
head(um)

m0 <- occu(~1~1,um) # fit null nodel 
m0

d1 <- occu(~habitat~1,um) 
d1 # hessian error
d2 <- occu(~precip~1,um)
d2 # no problems
d3 <- occu(~water.dist~1,um)
d3 # no problems
d4 <- occu(~road.width~1,um)
d4 # no problems
d5 <- occu(~wetland~1,um, control=list(maxit=3000, trace=TRUE, REPORT=1))
d5 # didn't converge, so added control argument
d6 <- occu(~speed~1,um) 
d6 # no problems
d7 <- occu(~LUT~1,um) 
d7 # no problems
d8 <- occu(~traffic~1,um) 
d8 # no problems
d9 <- occu(~infrastructure~1,um) 
d9 # no problems
d10 <- occu(~guineafowl~1,um) 
d10 # no problems
d11 <- occu(~owls~1,um) 
d11 # no problems. Estimate = 0.624, SE = 0.144, z = 4.34, P(>|z|) = 1.42e-05

o1 <- occu(~1~habitat,um)
o1 # hessian error
o2 <- occu(~1~precip,um)
o2 # no problems
o3 <- occu(~1~water.dist,um)
o3 # no probs
o4 <- occu(~1~road.width,um)
o4 # no probs
o5 <- occu(~1~wetland,um, control=list(maxit=3000, trace=TRUE, REPORT=1))
o5 # didn't converge, so added control argument
o6 <- occu(~1~speed,um) 
o6 # no problems
o7 <- occu(~1~LUT,um) 
o7 # no problems
o8 <- occu(~1~traffic,um) 
o8 # no problems
o9 <- occu(~1~infrastructure,um, control=list(maxit=3000, trace=TRUE, REPORT=1)) 
o9 # didn't converge, so added control argument
o10 <- occu(~1~guineafowl,um) 
o10 # no problems
o11 <- occu(~1~owls,um) 
o11 # no problems

# Only one model had p < 0.05 (owls on detection), so did not fit models that included more than one covariate

# Now compare models with null model using AIC
dlist <- fitList(Null = m0, d2=d2, d3=d3, d4=d4, d5=d5, d6=d6, d7=d7, d8=d8, d9=d9, d10=d10, d11=d11, o2=o2, o3=o3, o4=o4, o5=o5, o6=o6, o7=o7, o8=o8, o9=o9, o10=o10, o11=o11) # removed d1 & o1 due to hessian errors
selmod.2015 <- modSel(dlist,nullmod="Null")
selmod.2015

# Top models 
#       nPars    AIC delta  AICwt cumltvWt     Rsq      det covs  occ covs
# d11      3  89.87  0.00 0.8951     0.90 0.29653 ~owls     ~1
# o11      3  95.99  6.12 0.0420     0.94 0.16647 ~1        ~owls

# 2015 model average of all models within delta AIC of 7 of top model 
library(AICcmodavg)
mb.gof.test(d11, nsim = 100) # P-value = 0.69, c-hat = 0.4 
mb.gof.test(o11, nsim = 100) # P-value = 0.02, c-hat = 2.08 : poor model fit, and overdispersed

# So in 2015 owls looks important for detection. Owls possibly also important to an extent for occupancy, but not sure.  

# 2015 model average 
library(MuMIn)
best <- list(d11,o11)
avgmod2015 <- model.avg(best, fit=T)
summary(avgmod2015)


######
# 2016
######
# 2016, 12 30-day sampling periods

# Calculate naive occupancy for 30-day sampling occasions
naive2016 <- sum(apply(hist2016.30d, 1, sum) > 0)/nrow(hist2016.30d) 
naive2016 # 0.3414634

# Next fit occupancy models using 30-day detection history

um <- unmarkedFrameOccu(y=hist2016.30d,siteCovs= covs) # Create occupancy object
head(um)

m0 <- occu(~1~1,um) # fit null nodel 
m0 # no problems 

d1<- occu(~habitat~1,um) 
d1 # no problems
d2 <- occu(~precip~1,um)
d2 # no problems
d3 <- occu(~water.dist~1,um)
d3 # no problems
d4 <- occu(~road.width~1,um)
d4 # no problems
d5 <- occu(~wetland~1,um)
d5 # no problems
d6 <- occu(~speed~1,um) 
d6 # no problems
d7 <- occu(~LUT~1,um) 
d7 # no problems
d8 <- occu(~traffic~1,um) 
d8 # no problems
d9 <- occu(~infrastructure~1,um) 
d9 # no problems
d10 <- occu(~guineafowl~1,um) 
d10 # no problems. Estimate = 0.608, SE = 0.300, z = 2.03, P(>|z|) = 4.25e-02
d11 <- occu(~owls~1,um) 
d11 # no problems

o1 <- occu(~1~habitat,um)
o1 # hessian error
o2 <- occu(~1~precip,um)
o2 # no problems
o3 <- occu(~1~water.dist,um)
o3 # no problems
o4 <- occu(~1~road.width,um)
o4 # no problems
o5 <- occu(~1~wetland,um, control=list(maxit=3000, trace=TRUE, REPORT=1))
o5 # didn't converge, so added control argument
o6 <- occu(~1~speed,um) 
o6 # No problems
o7 <- occu(~1~LUT,um) 
o7 # No problems
o8 <- occu(~1~traffic,um) 
o8 # No problems
o9 <- occu(~1~infrastructure,um) 
o9 # No problems
o10 <- occu(~1~guineafowl,um, control=list(maxit=3000, trace=TRUE, REPORT=1)) 
o10 # didn't converge, so added control argument
o11 <- occu(~1~owls,um) 
o11 # No problems

# Only one model had any effect on p or psi (guineafowl on detection), so did not fit models that included more than one covariate

# Now compare models with null model using AIC
dlist <- fitList(Null = m0, d2=d2, d3=d3, d4=d4, d5=d5, d6=d6, d7=d7, d8=d8, d9=d9, d10=d10, d11=d11, o2=o2, o3=o3, o4=o4, o5=o5, o6=o6, o7=o7, o8=o8, o9=o9, o10=o10, o11=o11) # removed d1 & o1 due to hessian errors
selmod.2016 <- modSel(dlist,nullmod="Null")
selmod.2016

# Top models 
#      nPars    AIC delta AICwt cumltvWt     Rsq det covs       occ covs
# o5       3 153.57  0.00 0.435     0.43 0.15071 ~1             ~wetland
# o9       3 157.75  4.18 0.054     0.49 0.05703 ~1             ~infrastructure
# d10      3 157.83  4.27 0.052     0.54 0.05498 ~guineafowl    ~1
# Null     2 158.10  4.53 0.045     0.59 0.00000 ~1             ~1
# d3       3 158.32  4.75 0.040     0.63 0.04351 ~water.dist     ~1
# o4       3 158.33  4.76 0.040     0.67 0.04318 ~1              ~road.width
# o11      3 158.39  4.82 0.039     0.70 0.04185 ~1              ~owls
# o8       3 158.77  5.20 0.032     0.74 0.03267 ~1              ~traffic
# d11      3 159.11  5.54 0.027     0.76 0.02426 ~owls           ~1
# o7       3 159.16  5.59 0.027     0.79 0.02314 ~1              ~LUT
# d4       3 159.18  5.61 0.026     0.82 0.02263 ~road.width      ~1
# o10      3 159.53  5.96 0.022     0.84 0.01410 ~guineafowl      ~1
# d5       3 159.74  6.17 0.020     0.86 0.00888 ~wetland         ~1
# d9       3 159.80  6.24 0.019     0.88 0.00726 ~infrastructure  ~1
# d2       3 159.89  6.32 0.018     0.90 0.00510 ~precip          ~1
# d7       3 159.90  6.33 0.018     0.92 0.00501 ~LUT             ~1
# d8       3 159.95  6.38 0.018     0.93 0.00375 ~traffic         ~1 
# o2       3 160.05  6.49 0.017     0.95 0.00108 ~1               ~precip
# d6       3 160.08  6.51 0.017     0.97 0.00050 ~speed           ~1
# o6       3 160.09  6.52 0.017     0.98 0.00012 ~1               ~speed
# o3       3 160.09  6.52 0.017     1.00 0.00011 ~1               ~water.dist

# So all models were within delta AIC of 7 of one another. There were no other models within delta AIC of the top model (o5: ~1~wetland), but o5 didn't converge initially so I'm not sure if it is reliable. Null  model is  highly ranked, so all models except o5 are no better than the null. So potentially wetland was important in 2016 but it's not very clear.

# Test model fit for top models
library(AICcmodavg)
mb.gof.test(o5, nsim = 100) # P-value = 0.61, c-hat = 0.2
mb.gof.test(o9, nsim = 100) # P-value = 0.26, c-hat = 0.39
mb.gof.test(d10, nsim = 100) # P-value = 0.83, c-hat = 0.11
# Didn't detect probelms with the fit of the top 3 models 

# 2016 model average of top 3 models 
library(MuMIn)
best <- list(o5,o9,d10)
avgmod2016 <- model.avg(best, fit=T)
summary(avgmod2016)

###

