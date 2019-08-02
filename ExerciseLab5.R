# Data are available in the rs folder ?
path="D:/RCMRD/agrin/rcmrd_workshop/rsdata/"
#Define the work directory
setwd(path)

# Create RasterLayer objects for single Landsat layers (bands)
library(terra)
# Blue
b2 <- rast('data/rs/LC08_044034_20170614_B2.tif')
# Green
b3 <- rast('data/rs/LC08_044034_20170614_B3.tif')
# Red
b4 <- rast('data/rs/LC08_044034_20170614_B4.tif')
# Near Infrared (NIR)
b5 <- rast('data/rs/LC08_044034_20170614_B5.tif')

#Print the variables to check. E.g.
b2

#Image information and statistics

#coordinate reference system (CRS)
crs(b2)

ncell(b2)

dim(b2)

res(b2)


nlyr(b2)

compareGeom(b2,b3)

#  RasterStack creation (an object with multiple layers) from the existing 
# RasterLayer (single band) objects
s <- c(b5, b4, b3)
# Check the properties of the multi-band image
s
# Multi-band image creation using the filenames
# first create a list of raster layers to use
filenames <- paste0('data/rs/LC08_044034_20170614_B', 1:11, ".tif")
filenames
#Rasterize the list of images within one
landsat <- rast(filenames)
# show the content
landsat

# plint the 4 single bands; in case of error select all the bloc and run it
par(mfrow = c(2,2))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))

# Bands combination RGB
landsatRGB <- c(b4, b3, b2)
# plot 3-band image
plotRGB(landsatRGB)

# for better visualization we apply stretch
plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")

#True Color Composite
par(mfrow = c(1,2), mai = c(0.1, 0.1, 0.1, 0.1)) # increase space between plots
plotRGB(landsatRGB, axes=TRUE, stretch="lin", main="Landsat True Color Composite")

landsatFCC <- c(b5, b4, b3)
plotRGB(landsatFCC, axes=TRUE, stretch="lin", main="Landsat False Color Composite")

# use this function to clear the screen and use it better
dev.off()

# Subset and rename bands
# select first 3 bands only
landsatsub1 <- subset(landsat, 1:3)
# same
landsatsub2 <- landsat[[1:3]]
# Number of bands in the original and new data
nlyr(landsat) 

nlyr(landsatsub1)


nlyr(landsatsub2)

#Keeping the 7 first bands in landsat 
landsat <- subset(landsat, 1:7)
# Content and details
landsat

names(landsat)

# Assigning the original name to the bands
names(landsat) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')

names(landsat)

# Spatial subset of Crop
# Display the extent of landsat
ext(landsat)

#Assign an extension to get the AOI
e <- ext(624387, 635752, 4200047, 4210939)
# crop landsat by the extent
landsatcrop <- crop(landsat, e)
#Display the content of the AOI 
landsatcrop

#Saving results to disk
writeRaster(landsatcrop, filename="cropped-landsat.tif", overwrite=TRUE)

#Relation between bands using pairs function
## Between Ultra-blue and Blue
pairs(landsatcrop[[1:2]], main = "Ultra-blue versus Blue")

## Between Red and NIR

pairs(landsatcrop[[4:5]], main = "Red versus NIR")

#Pixels value extraction
library(sp)
# load the polygons with land use land cover information
samp <- readRDS('data/rs/samples.rds')
plot(samp)
text(samp,samp$class)
# generate 50 point samples from the polygons
ptsamp <- spsample(samp, 50, type='regular')
plot(ptsamp)
# add the land cover class to the points
ptsamp$class <- over(ptsamp, samp)$class
ptsamp <- vect(ptsamp)
# We use the x-y coordinates to extract the spectral values for the locations
xy <- as.matrix(geom(ptsamp)[,c('x','y')])
head(xy)
df <- extract(landsat, xy)
# To see some of the reflectance values
head(df)

#Spectral profils
ms <- aggregate(df, list(ptsamp$class), mean)
# instead of the first column, we use row names
rownames(ms) <- ms[,1]
ms <- ms[,-1]
ms

# Plotting the spectral profils 
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
# Basic mathematical operations
library(terra)
raslist <- paste0('data/rs/LC08_044034_20170614_B', 1:11, ".tif")
landsat <- rast(raslist)
landsatRGB <- landsat[[c(4,3,2)]]
landsatFCC <- landsat[[c(5,4,3)]]

# Vegetation index 
#General function
vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# For Landsat NIR = 5, red = 4.
ndvi <- vi(landsat, 5, 4)
plot(ndvi, col=rev(terrain.colors(10)), main = "Landsat-NDVI")

#Alternative way to get the NDVI
# Write a general function that can compute 2-layer NDVI type indices

vi2 <- function(x, y) {
  (x - y) / (x + y)
}

nir <- landsat[[5]]
red <- landsat[[4]]

ndvi2 <- overlay(nir, red, fun = vi2)
plot(ndvi2, col=rev(terrain.colors(10)), main = "Landsat-NDVI")

# or in one line
ndvi2 <- overlay(landsat[[5]], landsat[[4]], fun=vi2)
plot(ndvi2, col=rev(terrain.colors(10)), main="Landsat-NDVI")

#Histogram
# view histogram of data
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
## Warning in .hist1(x, maxcell = maxcell, main = main, plot = plot, ...): 54%
## of the raster cells were used.
axis(side=1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))

#Thresholding
veg <- classify(ndvi, cbind(-Inf, 0.4, NA))
plot(veg, main='Vegetation')

#Map that corresponds to the pick
m <- c(-Inf, 0.25, NA,  0.25, 0.3, 1,  0.3, Inf, NA)
rcl <- matrix(m, ncol = 3, byrow = TRUE)
land <- classify(ndvi, rcl)
plot(land, main = 'What is it?')

# Plotting land over original landsat False Color Composite
plotRGB(landsatRGB, r=1, g=2, b=3, axes=TRUE, stretch="lin", main="Landsat False Color Composite")
plot(land, add=TRUE, legend=FALSE)

# classes for different amount of vegetation
m <- c(-Inf,0.25,1, 0.25,0.3,2, 0.3,0.4,3, 0.4,0.5,4, 0.5,Inf, 5)
rcl <- matrix(m, ncol = 3, byrow = TRUE)
vegc <- classify(ndvi, rcl)

plot(vegc,col = rev(terrain.colors(4)), main = 'NDVI based thresholding')

#Component analysis
set.seed(1)
sr <- sampleRegular(landsat, 10000)
sr <- values(sr)
plot(sr[,c(4,5)], main = "NIR-Red plot")

#Vegetation and soil-line plot 
pca <- prcomp(sr, scale = TRUE)
pca

screeplot(pca)

#pci <- predict(landsat, pca, index = 1:2)
#plot(pci[[1]])

#Unsupervised classification
library(terra)
landsat5 <- rast('data/rs/centralvalley-2011LT5.tif')
names(landsat5) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')
#NDVI Calculation

ndvi <- (landsat5[['NIR']] - landsat5[['red']]) / (landsat5[['NIR']] + landsat5[['red']])

#Kmeans classification
# Extent to crop ndvi layer
e <- ext(-121.807, -121.725, 38.004, 38.072)
# crop landsat by the extent
ndvi <- crop(ndvi, e)
ndvi

nr <- values(ndvi)
str(nr)

# It is important to set the seed generator because `kmeans` initiates the centers in random locations
set.seed(99)
# We want to create 10 clusters, allow 500 iterations, start with 5 random sets using "Lloyd" method
kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500, nstart = 5, algorithm="Lloyd")
# kmeans returns an object of class "kmeans"
str(kmncluster)


# Use the ndvi object to set the cluster values to a new raster
knr <- ndvi
values(knr) <- kmncluster$cluster
knr

# Define a color vector for 10 clusters (learn more about setting the color later)
mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#0000ff","#00ff00","#cbbeb5",
             "#c3ff5b", "#ff7373", "#00ff00", "#808080")
par(mfrow = c(1,2))
plot(ndvi, col = rev(terrain.colors(10)), main = 'Landsat-NDVI')
plot(knr, main = 'Unsupervised classification', col = mycolor )

#Supervised classification
library(terra)
# We read the 6 bands from the Landsat image we previously used
raslist <- paste0('data/rs/LC08_044034_20170614_B', 2:7, ".tif")
landsat <- rast(raslist)
names(landsat) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')

#Reference Data
library(sp)
# load polygons with land use land cover information
samp <- readRDS('data/rs/samples.rds')
#samp <- spTransform(samp, crs(landsat5))
# check the distribution of the polygons
plot(samp)
text(samp, samp$class)

#Random points generation
set.seed(1)
# generate point samples from the polygons
ptsamp <- spsample(samp, 1000, type='random')
# add the land cover class to the points
ptsamp$class <- over(ptsamp, samp)$class
# We convert `ptsamp` to `SpatVector`
ptsamp <- vect(ptsamp)

#Value extraction 
# We use the x-y coordinates to extract the spectral values for the locations
xy <- as.matrix(geom(ptsamp)[,c('x','y')])
df <- extract(landsat, xy)
# Quick check for the extracted values
head(df)
# combine lulc class information with extracted values
sampdata <- data.frame(class = ptsamp$class, df)

#Train the classifier
library(rpart)
# Train the model
cartmodel <- rpart(as.factor(class)~., data = sampdata, method = 'class', minsplit = 5)

# print trained model
print(cartmodel)

# Plot the trained classification tree
plot(cartmodel, uniform=TRUE, main="Classification Tree")
text(cartmodel, cex = 1)

# Now predict the subset data based on the model; prediction for entire area takes longer time
classified <- predict(landsat, cartmodel, na.rm = TRUE)
classified

plot(classified)

class <- c("built","cropland","fallow","open","water")
mycolor <- c('darkred', 'yellow', 'burlywood', 'cyan', 'blue')
classdf <- data.frame(classvalue = c(1,2,3,4,5),
                      classnames = class,
                      color = mycolor, stringsAsFactors = FALSE)

lulc <- app(classified, fun = which.max)
lulcc <- as.factor(lulc)
levels(lulcc) <- classdf$classnames
plot(lulcc, col = classdf$color)

#Model evaluation
set.seed(99)
# number of folds
k <- 5
j <- sample(rep(1:k, each = round(nrow(sampdata))/k))

table(j)

x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class',
                minsplit = 5)
  pclass <- predict(cart, test, na.rm = TRUE)
  # assign class to maximum probablity
  pclass <- apply(pclass, 1, which.max)
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

#Confusion matrix

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
# confusion matrix
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames
rownames(conmat) <- classdf$classnames
print(conmat)

#Overall accuracy
# number of total cases/samples
n <- sum(conmat)
print(n)

# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
print(OA)

#Kappa
# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
print(kappa)
## [1] 0.9530854

#Producer and user
# Producer accuracy
PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
print(outAcc)


#Testing with Random Forest, install the package first 

library(randomForest)
cartmodel2 <- randomForest(as.factor(class)~., data = sampdata, importance=TRUE, proximity = TRUE)
print(cartmodel2)

plot(cartmodel2)
text(cartmodel, cex = 1)

# Now predict the subset data based on the model; prediction for entire area takes longer time
classified <- predict(landsat, cartmodel2, na.rm = TRUE)
classified

plot(classified)


class <- c("built","cropland","fallow","open","water")
mycolor <- c('darkred', 'yellow', 'burlywood', 'cyan', 'blue')
classdf <- data.frame(classvalue = c(1,2,3,4,5),
                      classnames = class,
                      color = mycolor)
lulc <- app(classified, fun = which.max)
lulcc <- as.factor(lulc)
levels(lulcc) <- as.character(classdf$classnames)
plot(lulcc)
#Very Good training 