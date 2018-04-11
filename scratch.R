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