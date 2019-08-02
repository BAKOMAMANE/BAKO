## ----setup, echo=TRUE, include=FALSE-------------------------------------
library(knitr)
library(terra)

#Landsat Data downloading
dir.create('data', showWarnings = FALSE)
if (!file.exists('data/rs/samples.rds')) {
  download.file('https://biogeo.ucdavis.edu/data/rspatial/rsdata.zip', dest = 'data/rsdata.zip')
  unzip('data/rsdata.zip', exdir='data')
}

## ------------------------------------------------------------------------
library(terra)

# Blue
b2 <- rast('data/rs/LC08_044034_20170614_B2.tif')

# Green
b3 <- rast('data/rs/LC08_044034_20170614_B3.tif')

# Red
b4 <- rast('data/rs/LC08_044034_20170614_B4.tif')

# Near Infrared (NIR)
b5 <- rast('data/rs/LC08_044034_20170614_B5.tif')


## ------------------------------------------------------------------------
b2


## ------------------------------------------------------------------------
# coordinate reference system (CRS)
crs(b2)

# Number of cells, rows, columns
ncell(b2)
dim(b2)

# spatial resolution
res(b2)

# Number of bands
nlyr(b2)

# Do the bands have the same extent, number of rows and columns, projection, resolution, and origin 
compareGeom(b2,b3)


## ------------------------------------------------------------------------
s <- c(b5, b4, b3)
# Check the properties of the multi-band image
s


## ------------------------------------------------------------------------
# first create a list of raster layers to use
filenames <- paste0('data/rs/LC08_044034_20170614_B', 1:11, ".tif")
filenames

landsat <- rast(filenames)
landsat


## ----rs2multi, fig.width = 8, fig.height = 8-----------------------------
par(mfrow = c(2,2))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))


## ----truecolor-----------------------------------------------------------
landsatRGB <- c(b4, b3, b2)

# plot 3-band image
plotRGB(landsatRGB)

# for better visualization we apply stretch 
plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")


## ----rs2plotrgb, fig.width = 8, fig.height = 4---------------------------
# par(mfrow = c(1,2), mai = c(0.1, 0.1, 0.1, 0.1)) # increase space between plots
plotRGB(landsatRGB, axes=TRUE, stretch="lin", main="Landsat True Color Composite")

landsatFCC <- c(b5, b4, b3)
plotRGB(landsatFCC, axes=TRUE, stretch="lin", main="Landsat False Color Composite")


## ------------------------------------------------------------------------
# select first 3 bands only
landsatsub1 <- subset(landsat, 1:3)
# same
landsatsub2 <- landsat[[1:3]]

# Number of bands in the original and new data
nlyr(landsat)
nlyr(landsatsub1)
nlyr(landsatsub2)



## ------------------------------------------------------------------------
landsat <- subset(landsat, 1:7)


## ------------------------------------------------------------------------
names(landsat)
names(landsat) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')
names(landsat)


## ---- fig.width = 10-----------------------------------------------------
# Using extent
ext(landsat)
e <- ext(624387, 635752, 4200047, 4210939)

# crop landsat by the extent
landsatcrop <- crop(landsat, e)


## ------------------------------------------------------------------------
writeRaster(landsatcrop, filename="cropped-landsat.tif", overwrite=TRUE)


## ---- rs2pairs1, fig.width = 5, fig.height = 5---------------------------
pairs(landsatcrop[[1:2]], main = "Ultra-blue versus Blue")


## ---- rs2pairs2, fig.width = 5, fig.height = 5---------------------------
pairs(landsatcrop[[4:5]], main = "Red versus NIR")


## ------------------------------------------------------------------------
library(sp)
# load the polygons with land use land cover information
samp <- readRDS('data/rs/samples.rds')

# generate 50 point samples from the polygons 
ptsamp <- spsample(samp, 50, type='regular')

# add the land cover class to the points
ptsamp$class <- over(ptsamp, samp)$class

ptsamp <- vect(ptsamp)

# We use the x-y coordinates to extract the spectral values for the locations 
xy <- as.matrix(geom(ptsamp)[,c('x','y')])

df <- extract(landsat, xy)

# To see some of the reflectance values
head(df)


## ------------------------------------------------------------------------
ms <- aggregate(df, list(ptsamp$class), mean)

# instead of the first column, we use row names 
rownames(ms) <- ms[,1]
ms <- ms[,-1]
ms


## ----rs2spect, fig.width  = 6, fig.height = 4----------------------------
# Create a vector of color for the land cover classes for use in plotting
mycolor <- c('darkred', 'yellow', 'burlywood', 'cyan', 'blue')

#transform ms from a data.frame to a matrix
ms <- as.matrix(ms)

# First create an empty plot
plot(0, ylim=c(0,0.6), xlim = c(1,7), type='n', xlab="Bands", ylab = "Reflectance")

# add the different classes
for (i in 1:nrow(ms)){
  lines(ms[i,], type = "l", lwd = 3, lty = 1, col = mycolor[i])
}

# Title
title(main="Spectral Profile from Landsat", font.main = 2)

# Legend
legend("topleft", rownames(ms), 
       cex=0.8, col=mycolor, lty = 1, lwd =3, bty = "n")
