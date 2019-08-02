## ---- global_options, tidy = TRUE, echo=FALSE, message=FALSE-------------
library(knitr)
library(terra)
library(luna)
library(agrin)
opts_chunk$set(tidy.opts=list(width.cutoff=60))


## ----downloadMODIS-------------------------------------------------------
library(terra)
library(luna)

# specify donwload directories
datadir <- file.path(dirname(tempdir()), "modis")
downdir <- file.path(datadir, "raw")
dir.create(downdir, recursive=TRUE, showWarnings=FALSE)

# specify parameters for downloading the files
# dates
start <- "2010-01-01" 
end <- "2010-01-07"

# study area
library(agrin)
aoi <- ibli_data("marsabit") 

# download MODIS tiles
fmod <- getModis(product="MOD09A1", start, end, aoi, download=TRUE, path=downdir)
fmod


## ----qcMat---------------------------------------------------------------
se <- matrix(c(1,2,3,6,11,11,12,14,16,16), ncol=2, byrow=TRUE)
reject <- list(c("10", "11"), c("1100","1101","1110","1111"), "1", c("000","110","111"), "11")


## ----prj-----------------------------------------------------------------
prj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "
pols <- project(aoi, prj)


## ----Preprocess----------------------------------------------------------
workdir <- file.path(datadir, "work")
dir.create(workdir, recursive=TRUE, showWarnings=FALSE)

# Loop over all images and pre-process
for(i in 1:length(fmod)){
  #Load image
  r <- rast(fmod[i])
  
  #Generate quality mask
  quality_mask <- modis_mask(r[[12]], 16, se, reject)
  
  #Select only red and NIR bands
  r <- r[[1:2]]
  
  #Mask out bad quality pixels
  r <- mask(r, quality_mask)
  
  # Clip the image using AOI
  r <- crop(r, pols)
  
  #Ensure all data lies between 0 & 1 
  r <- clamp(r, 0, 1) 
  
  #Compute NDVI
  red <- r[[1]]
  nir <- r[[2]]
  ndvi <- (nir - red)/(nir + red)
  
  filename <- paste0(tools::file_path_sans_ext(basename(fmod[i])), "_ndvi.tif")
  filename <- file.path(workdir, filename)
  writeRaster(ndvi, filename=filename, overwrite=TRUE)
}
