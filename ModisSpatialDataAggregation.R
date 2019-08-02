## ---- global_options, tidy = TRUE, echo=FALSE, message=FALSE-------------
library(knitr)
library(terra)
library(luna)
library(agrin)
opts_chunk$set(tidy.opts=list(width.cutoff=60))


## ----NDVIdownload--------------------------------------------------------
ndvidir <- file.path(dirname(tempdir()), "modis/ndvi")
files <- ibli_data("ndvi", ndvidir)
head(basename(files))


## ----aoi-----------------------------------------------------------------
library(agrin)
aoi <- ibli_data("marsabit")
# project to sinusoidal 
prj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "
pols <- project(aoi, prj)


## ----aoi2----------------------------------------------------------------
pols
plot(pols)


## ----NDVIs---------------------------------------------------------------
#Define period
startyear <- 2008
endyear   <- 2015

#Define season s (LRLD in this case)
sSeason <- "LRLD"
startSeason <- "03-01"
endSeason   <- "09-30"


## ----selectModFs---------------------------------------------------------
selectModisFiles <- function(files, startdate, enddate) {
  base_names <- basename(files)
  dates <- substr(base_names, 10, 16)
  dates <- dateFromYearDoy(dates)
  i <- (dates >= as.Date(startdate)) & (dates <= as.Date(enddate) )
  files[i]
}


## ----NDVIs_Plots, fig.width=10, fig.height=10----------------------------
season <- selectModisFiles(files, paste0(startyear, "-", startSeason), paste0(startyear, "-", endSeason))
sndvi <- rast(season)
plot(sndvi[[1:9]])


## ----NDVIs_Plots2--------------------------------------------------------
ndvi_val <- terra::extract(sndvi, pols, fun=mean, na.rm=TRUE)
plot(ndvi_val[1,], ylab= "NDVI")


## ----UseselectModFs------------------------------------------------------
workdir <- file.path(dirname(tempdir()), "work")
dir.create(workdir, recursive=TRUE, showWarnings=FALSE)

for(year in startyear:endyear) {
  season <- selectModisFiles(files, paste0(year, "-", startSeason), paste0(year, "-", endSeason))
  
  sndvi <- rast(season)
  ndvimean <- mean(sndvi, na.rm = TRUE)
  
  # Shorten band names 
  names(sndvi) <- paste0("ndvi_",dateFromYearDoy(substr(names(sndvi), 10, 16)))
  
  filename=file.path(workdir, paste0( 'MOD09A1.h21v08.',year,"_",sSeason,'_ndvit.tif'))
  
  writeRaster(ndvimean, filename = filename , overwrite=TRUE)
}


## ----MorePlots-----------------------------------------------------------
par(mar = c(2.2, 2.2, 1.2, 0.5)) #c(bottom, left, top, right)
stitle <- paste(year, " NDVIs", sep="")
plot(ndvimean, main = stitle)


## ----NDVISpatial---------------------------------------------------------
# Define season s (LRLD in this case)
sSeason <- "LRLD"
files <- list.files(workdir, pattern=paste0(sSeason,"_ndvit.tif$"), full.names=TRUE)
output <- matrix(nrow=nrow(pols), ncol=length(files))
colnames(output) <- substr(basename(files),16,19)

for(i in 1:length(files)){
  #read the temporal aggregate NDVI for each season
  ndvit <- rast(files[i])
  
  #Extract values that fall within each sub-location
  #and compute a spatial aggregate i.e. NDVIls	
  output[,i] <- extract(ndvit, pols, fun=mean, na.rm=TRUE)
}

# Create a data.frame
res <- data.frame(SLNAME=pols$SUB_LOCATI, IBLI_Zone=pols$IBLI_UNIT, output, stringsAsFactors = FALSE, check.names=FALSE)

# save it as we might want to use it again later.
saveRDS(res, file.path(workdir, paste0(sSeason,"_ndvi_st_mat.rds")))


## ----NDVIls_Plots, eval = FALSE------------------------------------------
## for (i in 1:nrow(output)) {
##   plot(output[i,],xaxt="none", ylab="Spatial-temporal NDVI aggregate", xlab="Year", main=paste0( as.character(pols$SUB_LOCATI[i]), " Sub-location.") )
##   axis(1, at=c(1:length(files)), labels=colnames(output))
##   lines(output[i,])
## }
## 
## 


## ----zscore--------------------------------------------------------------
# z-score computation function
zscore <- function(y){
  (y - mean(y, na.rm=TRUE) ) / (sd(y, na.rm=TRUE))
}



## ----zScores-------------------------------------------------------------
scores <- apply(res[,-c(1:2)], 1, zscore)
scores <- t(scores)
head(scores)

ndvi_st <- data.frame(SLNAME=pols$SUB_LOCATI, IBLI_Zone=pols$IBLI_UNIT, scores, stringsAsFactors = FALSE, check.names=FALSE)
saveRDS(ndvi_st, file.path(workdir, paste0(sSeason,"_zndvi_st_mat.rds")))



## ----PlotzScores, fig.width=12, fig.height=12----------------------------

years <- 2008:2015

par(mfrow=c(2,2))
for(i in 1:4){
  
  plot(years, scores[i,], ylab="zNDVI", xlab="Year", main=pols$SUB_LOCATI[i])
  lines(years, scores[i,])
  
  #Add strike level at zero assumming that payouts would be made any time NDVi deviates below the mean
  abline(h=0, col="red",lty=2)
  
  #linear regression model
  m1 <- lm(scores[i,]~years)
  abline(m1, lty=3, col="blue", lwd=2)
  
  #second order polynomial
  m2 <- lm(scores[i,]~years+I(years^2))
  lines(years, predict(m2), lty=4, col="green", lwd=2)
  
  # Moving average
  rmean5 <- stats::filter(scores[i,], rep(1/2, 2), sides = 2, method= "convolution")
  lines(years, rmean5, lty=6, col="cyan", lwd=2)
  
  #Add a legend
  pos <- ifelse(i==3, "topright", "bottomright")
  
  legend(pos, c("Data","Trigger", "Linear", "Quadratic", "Average"), col=c("black", "red","blue","green","cyan"), lty=c(1,2,3,4,6), cex=1.25)
}
