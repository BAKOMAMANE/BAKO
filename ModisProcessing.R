## ---- echo=FALSE, message=FALSE------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60))
datadir <- file.path(dirname(tempdir()), "modis")
mf <- file.path(datadir, "modis_qualmasked.tif")
library(agrin)
library(terra)
rmask <- rast(mf)


## ----boundary------------------------------------------------------------
library(agrin)

pol <- ibli_data("marsabit")


## ----prj-----------------------------------------------------------------
rmask

prj <- crs(rmask)

prj

poly <- project(pol, prj)


## ----crop----------------------------------------------------------------
rclip <- crop(rmask, poly)

# Plot cropped MODIS and add the boundary
plotRGB(rclip, r = 2, g = 1, b = 4, 
        main = 'MODIS False color composite (NIR:Red:Green)',
        stretch = "lin" )
# Overlay with boundary 
lines(poly, col="blue")



## ----clamp---------------------------------------------------------------
# To ensure all data lies between 0 & 1 as anticipated for reflectance
rimage <- clamp(rclip, 0, 1) 


## ----ndvi----------------------------------------------------------------
ndvi <- (rclip[[2]] - rclip[[1]]) /(rclip[[2]] + rclip[[1]])
plot(ndvi, main="NDVI")


## ----extract-------------------------------------------------------------

ndvi_val <- extract(ndvi, poly, fun=mean, na.rm=TRUE)

ndvi_val[1:5]

