
# 1. First read chapters 1, 2, 3 and section 6.1 to 6.4 from
# https://gfc.ucdavis.edu/events/agrin/html/spatial

# 2. Read the file "Operating-Health-Facilities-with-location-2014.csv" (under "data\tza")
d <- read.csv( "C:/Bako1/SERVIR/RCMRD/RPrograming/lab1_data/Operating-Health-Facilities-with-location-2014.csv" , 
               stringsAsFactors=FALSE)
dim(d)
head(d)

# 3. Make a plot of the locations
plot (d[, c('LONGITUDE', 'LATITUDE')])

# 4. how many of the records have missing coordinates (absolute number and rounded percentage)?
xy<-na.omit(d[, c('LONGITUDE', 'LATITUDE')])
nrow(d)
nrow(xy)
nrow(d) - nrow(xy)
round(100 - 100*(nrow(xy)/nrow(d)),1)

# 5. Use the data to create a SpatVector of points
library(terra)
d <- d[!is.na(d$LONGITUDE), ]
sp <- vect(d[,c('LONGITUDE', 'LATITUDE')], atts=d)


# 6. Plot the SpatVector object
library(luna)
library(agrin)
plot(sp)

plot(sp, col=rainbow(7)[as.integer(as.factor(sp$ZONE))])
raster::spplot(sp, 'ZONE')


# 7. What is the coordinate reference system (CRS) of this object?
crs(sp)

# 8. Change the CRS to what it should be
crs(sp) <- "+proj=longlat +datum=WGS84"

# 9. Subset the SpatVector to create one for only Arusha Region 
arusha<- sp[sp$REGION=="Arusha",]

# 10. How many points does the SpatVector for Arusha have?
size(arusha)

# 11. Get a SpatVector for Tanzanian boundaries
library(raster)
tza <- vect(getData('GADM', country='TZA', level=1))
tza


# 12 plot Tanzania with the point locations
plot(tza)
plot(sp)

# 13. now transform the points and polygons object to a planar coordinate reference system 
# that is appropriate for all of Tanzania, and plot them together on a simple map
cr <- "+proj=laea +lat_0=-10 +lon_0=35 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
# or 
cr <- "+proj=laea +lat_0=-10 +lon_0=35 +datum=WGS84"

ptza <- project(tza, cr)
psp <- project(sp, cr)



plot(ptza)
plot(psp)

names(d)

d <- read.csv( "C:/Bako1/SERVIR/RCMRD/RPrograming/lab1_data/Operating-Health-Facilities-with-location-2014.csv" , 
               stringsAsFactors=FALSE)

d <- d[!is.na(d$LONGITUDE), ]
ptza <- getData('GADM', country='TZA', level=1)
unique(d$REGION)
unique(ptza$NAME_1)
vv <- sp::merge(ptza, d, by.x="NAME_1", by.y="REGION")
