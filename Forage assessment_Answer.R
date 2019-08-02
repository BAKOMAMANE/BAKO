## ---- global_options, tidy = TRUE, echo=FALSE, message=FALSE-------------
library(knitr)
library(agrin)
library(terra)
library(luna)
opts_chunk$set(tidy.opts=list(width.cutoff=60))


## ----ndvidata------------------------------------------------------------
library(agrin)
ndvidir <- file.path(dirname(tempdir()), "modis/ndvi")
# the first time this takes a while (download)
files <- ibli_data("ndvi", ndvidir)

cat("There are ", length(files), " MODIS Terra 8-day NDVI images. \n")
head(basename(files))



## ----getFiles, include=FALSE---------------------------------------------
library(terra)

# Function for selecting MODIS images within a given season s.
selectModisFiles <- function(files, startdate, enddate) {
  base_names <- basename(files)
  dates <- substr(base_names, 10, 16)
  dates <- dateFromYearDoy(dates)
  i <- dates >= as.Date(startdate) & dates <= as.Date(enddate) 
  files[i]
}

datadir <- file.path(dirname(tempdir()), "modis")
files <- list.files(datadir, pattern="*ndvi.tif", recursive=TRUE, full.names=TRUE)
cat("There are ", length(files), " MODIS Terra 8-day NDVI images. \n")
head(basename(files))


## ----NDVIs,  include=FALSE-----------------------------------------------
#Get the area of interest (AOI/case study) polygon boundary.
library(agrin)
pol <- ibli_data("marsabit")

#Project the case study boundary to MODIS sinusoidal projection system.
prj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
poly <- project(pol, prj)

#Define period
startyear <- 2008
endyear   <- 2015
#Define season s in SRSD for early assessment of forage availability index.
sSeason <- "SRSD"
startSeason <- "10-01"
endSeason   <- "12-31"


## ----NDVIs2,  include=FALSE----------------------------------------------
library(luna)
library(terra)

for(year in startyear:endyear) {
  fn <- paste0(datadir, 'MOD09A1.h21v08.', year, "_", sSeason,'_ndvit.tif')
  if (file.exists(fn)) next
  
  season <- selectModisFiles(files, paste0(year, "-", startSeason), paste0(year, "-", endSeason))
  #Stack season NDVi
  sndvi <- rast(season)
  #Shorten band names 
  names(sndvi) <- paste0("ndvi_", dateFromYearDoy(substr(names(sndvi), 10, 16)))
  # Extract 8-day NDVI spatial aggregates per region (l)
  ndvi_val <- extract(sndvi, poly, fun=mean, na.rm=TRUE)
  #Let us now compute NDVIs
  ndvimean <- mean(sndvi, na.rm = TRUE)
  # write to file NDVIt to file
  writeRaster(ndvimean, filename=fn)
}


## ----NDVIls, echo=FALSE, results="hide"----------------------------------
# List processed NDVI files
sSeason <- "SRSD" #Define season s (LRLD in this case)
files <- list.files(datadir, pattern=glob2rx(paste0("*",sSeason,"_ndvit*.tif$")),recursive=TRUE, full.names=TRUE)


output <- matrix(nrow=nrow(poly), ncol=length(files))
colnames(output) <- substr(basename(files),16,19)

for(i in 1:length(files)){
  #read the temporal aggregate NDVI for each season s
  ndvit <- rast(files[i])
  #Extract values that fall within each sub-location
  vals <- extract(ndvit, poly)
  #Compute a spatial aggregate from NDVIt for each location l i.e. NDVIls
  vals.mean <- rapply(vals, mean, na.rm=TRUE)
  output[,i] <- vals.mean
}

#Create  a data frame and save it
res <- data.frame(SLNAME=poly$SUB_LOCATI, IBLI_Zone=poly$IBLI_UNIT, output, stringsAsFactors = FALSE, check.names=FALSE)


## ----zNDVIls, echo=FALSE, results="hide"---------------------------------
library(matrixStats)
#z-Score computation function
zscore <- function(y){
  (y-rowMeans(y, na.rm=TRUE))/(rowSds(y, na.rm=TRUE))
}

ndvi_st <- output

#Compute zScore
scores <- zscore(ndvi_st)
head(scores)

#Create  a data frame and save it
zndvit_res <- data.frame(SLNAME=poly$SUB_LOCATI, IBLI_Zone=poly$IBLI_UNIT, scores, stringsAsFactors = FALSE, check.names=FALSE)



## ----payouts, echo=FALSE, results="hide"---------------------------------
# Case 1:

# @param zNDVIls is the current season'spatial temporal mean NDVI.
payoutsIBLI <- function(zNDVIls, trigger=-0.842, exit=-2.326){
  if(zNDVIls < exit){
    return(100)
  } else if(trigger > zNDVIls & zNDVIls > exit){
    return( 100 * ( ( trigger- zNDVIls )/(trigger - exit)  ) )
  } else if(zNDVIls > trigger){
    return(0)
  }else
    return("No condition met")
  
}


payoutsIBLI(zndvit_res$`2014`[zndvit_res$SLNAME=="IRIR"])

payoutsIBLI(zndvit_res$`2015`[zndvit_res$SLNAME=="IRIR"])


#Compute for all regions
zndvit_res$payout2014 <- unlist(lapply(zndvit_res$`2014`, FUN=payoutsIBLI, trigger=-0.2))

zndvit_res$payout2015 <- unlist(lapply(zndvit_res$`2015`, FUN=payoutsIBLI, trigger=-0.2))


## Case 2:

# @param zNDVIls is the current season'spatial temporal mean NDVI.
# @param azNDVIls is vector of all annuall spatial-temporal aggregate zNDVIls of an insurance unit.
payoutsIBLI <- function(zNDVIls, azNDVIls){
  trigger <- quantile(azNDVIls,  probs = 0.2, na.rm = T)
  exit    <- min(azNDVIls, na.rm=T)
  rate    <- 5/12 * 100
  if(zNDVIls < exit){
    return(rate)
  } else if(trigger > zNDVIls & zNDVIls > exit){
    return( rate * ( ( trigger - zNDVIls )/(trigger - exit)  ) )
  } else if(zNDVIls > trigger){
    return(0)
  } else
    return("No condition met")
  
}

#In 2014 no condition is met because 2014 zNDVIls is the same as exit
payoutsIBLI(zndvit_res$`2014`[zndvit_res$SLNAME=="IRIR"], zndvit_res$`2014`) 

payoutsIBLI(zndvit_res$`2015`[zndvit_res$SLNAME=="IRIR"], zndvit_res$`2015`)




## ----premium-------------------------------------------------------------
# Define the constants
rho = 1.5 # Risk aversion of the farmer in the zone z
markup = 0.25 # Percentage to add on the premium
#2014
zndvit_res$premium2014 <- mean(zndvit_res$payout2014) * (1 + markup)
#2015
zndvit_res$premium2015 <- mean(zndvit_res$payout2015) * (1 + markup)  

# 3. Calculate income without and with insurance for area yield
zndvit_res$inc_noins2014 <- zndvit_res$`2014`
zndvit_res$inc_ins2014 <- zndvit_res$`2014` + zndvit_res$payout2014 - zndvit_res$premium2014
zndvit_res$mean_noins2014 <- mean(zndvit_res$inc_noins2014)

plot(zndvit_res$inc_noins2014, zndvit_res$inc_ins2014, xlab="No Insurance",ylab="With Insurance", main="2014")