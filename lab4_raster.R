# raster data

# Read https://gfc.ucdavis.edu/events/agrin/html/spatial/4-rasterdata.html

# And quickly browse 
# https://gfc.ucdavis.edu/events/agrin/html/spatial/8-rastermanip.html

# We are going to extract annual average rainfall in Tanzania 
# for all years (that we have data for) of Tanzania 
# We are using a raster data that covers Tanzania and more and has monthly data 

# Later on we will use these data to see if variation in rainfall predicts
# variation in maize yield.

# You do not have to fill in very much. But take your time to see
# what is going on

# where is the monthly precipitation data ?
path <- "C:/Bako1/SERVIR/RCMRD/RPrograming/lab4_raster_data/climate/prec"

# get a vector of all the relevant files
ff <- list.files(path=path, pattern='.tif$', full.names=TRUE)

# we want them in order. In this case, sort works (not always!)
ff <- sort(ff)

# now make a SpatRaster from these files
library(terra)
s <- rast(ff)
# you shoul have 408 layers
s

# let's plot
plot(s)
# (by default, only the first 16 are plotted)

# plot the first three:
plot(s[,1:3])

# let's look at the mean for each month
# we use the tappr function. We tell it to make 12 layers (by combining the 1st, with the 13th, 25th, etc. layer --- recycling at work).
rainmonth <- tapp(s, 1:12, mean)
# set the right names
names(rainmonth) <- month.name

rainmonth
plot(rainmonth)

# compute the annual average total rainfall
rain <- sum(rainmonth)
plot(rain)

# and the monthly mean precipitation averaged over all cells
mrain <- global(rainmonth, mean, na.rm=TRUE)
barplot(mrain[,1],  names.arg=rownames(mrain)) 

# we want to compare annual maize yield with annual rainfall. 
# the Tanzania dry season is in the middle of the year
# it may be better to have "rain-years" that start in July and run through June
# that is a bit tricky

# we make explicit indices to group months
# 34 years, 12 months
i <- rep(1:34, each=12)
i[1:36]
# but the first 6 months are the last 6 months of the previous rain-year
i <- c(rep(1,6), i+1)
i[1:36]
# and now we need to remove the last 6 months 
i <- i[1:(length(i)-6)]
i

# stackApply again, now with our new indices to get yearly totals
ss <- tapp(s, i, sum)
# set the layer names
names(ss) <- paste0("Y", 1980:2014)
# the first and the last year are incomplete 
ss <- ss[[ 2:(nlyr(ss)-1) ]]

plot(ss)


# We only want Tanzania
# get the extent of Tanzania
library(raster)
tza <- vect(raster::getData('GADM', country='tza', level=1))
ss <- mask(ss, tza)
plot(ss)

plot(ss[[1:4]])
plot(ss[[5:8]])
plot(ss[[9:12]])
plot(ss[[13:16]])


# save to file
filename <- file.path("C:/Bako1/SERVIR/RCMRD/RPrograming/lab4_raster_data/climate/",
                      'tza_annual_precipitation.tif')
ss <- writeRaster(ss, filename = filename, datatype='INT2S')
ss

# compute the average monthly precipitation for the entire country
x <- global(ss, mean, na.rm=TRUE)
x

