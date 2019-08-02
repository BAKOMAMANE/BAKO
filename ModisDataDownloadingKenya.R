library(terra)
library(luna)
# lists all products that are currently searchable
prod <- getProducts()
head(prod)

modis <- getProducts("^MOD|^MYD|^MCD")
head(modis)

product <- "MOD09A1"
productInfo(product)

start <- "2019-01-01"
end <- "2019-01-10"


if (!require(agrin)) {
  devtools::install_github("aginsurance/agrin")
}

library(agrin)
ken <- ibli_data("kenya")
ken

i <- ken$NAME_1 == "Marsabit"
aoi <- ken[i,]
plot(ken, col="light gray")
lines(aoi, col="red", lwd=2)


mf <- luna::getModis(product = product,
                     start_date = start, end_date = end,
                     aoi = aoi, download = FALSE)
mf


datadir <- file.path("D:/RCMRD/kenya", "modis")
dir.create(datadir, recursive = TRUE)

mf <- luna::getModis(product=product,
                     start_date=start, end_date=end,
                     aoi=aoi, download=TRUE, path=datadir)
mf

r <- rast(mf[1])
r

# Check coordinate reference system (CRS)
crs(r)
## [1] "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "
# Number of cells, rows, columns
ncell(r)
## [1] 5760000
dim(r)
## [1] 2400 2400   13
# spatial resolution ~ 500 m
res(r)
## [1] 463.3127 463.3127
# Number of bands/layers/subdatasets
nlyr(r)
## [1] 13
# Let's find the name of the layers within this HDF file
names(r)

sub1 <- substr(names(r), 86, 120)
# In case you don't want to count the number of characters
sub2 <- sapply(strsplit(names(r), ":"), "[[", 3)
names(r) <- sub1

# Create an image RGB composite plot
plotRGB(r, r = 1, g = 4, b = 3)


# Disappointing? apply some stretching; see `?plotRGB` for more options
plotRGB(r, r = 1, g = 4, b = 3, stretch="lin")

# now to plot with a title, we have to use `axes=TRUE`
plotRGB(r, r = 1, g = 4, b = 3, stretch="lin",
        axes = TRUE, main = "MODIS True colour composite")

se <- matrix(c(1,2,3,6,11,11,12,14,16,16), ncol=2, byrow=TRUE)
reject <- list(c("10", "11"), c("1100","1101","1110","1111"), "1", c("000","110","111"), "11")
se

str(reject)

library(terra)
mf

r <- rast(mf)

qc <- r[[12]]
plot(qc, main = "Quality")

library(luna)
quality_mask <- modis_mask(qc, 16, se, reject)
plot(quality_mask, main="Quality mask with pixles to be retained")

rmask <- mask(r, quality_mask)

plotRGB(rmask, r = 2, g = 1, b = 4, main='False color composite', stretch="lin")

#Processing

library(agrin)
pol <- ibli_data("marsabit")

rmask

prj <- crs(rmask)
prj

poly <- project(pol, prj)

rclip <- crop(rmask, poly)


# Plot cropped MODIS and add the boundary
plotRGB(rclip, r = 2, g = 1, b = 4,
        main = 'MODIS False color composite (NIR:Red:Green)',
        stretch = "lin" )
# Overlay with boundary
lines(poly, col="blue")

# To ensure all data lies between 0 & 1 as anticipated for reflectance
rimage <- clamp(rclip, 0, 1)

ndvi <- (rclip[[2]] - rclip[[1]]) /(rclip[[2]] + rclip[[1]])
plot(ndvi, main="NDVI")