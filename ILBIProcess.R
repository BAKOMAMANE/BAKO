library(terra)
library(luna)
library(agrin)

path <- "D:/RCMRD/agrin/rcmrd_workshop"
ndvidir <- file.path(path, "modis/ndvi")
files <- list.files(path = ndvidir, pattern = '_ndvi.tif',
                    full.names = FALSE)
setwd(ndvidir)
#head(basename(files))
files
#Define period
startyear <- 2008
endyear   <- 2015
#Define season s (LRLD in this case)
sSeason <- "LRLD"
startSeason <- "03-01"
endSeason   <- "09-30"


#Define a function for a given year

selectModisFiles <- function(files, startdate, enddate) {
  base_names <- basename(files)
  dates <- substr(base_names, 10, 16)
  dates <- dateFromYearDoy(dates)
  i <- dates >= as.Date(startdate) & dates <= as.Date(enddate)
  files[i]
}


# Run the fuction
season <- selectModisFiles(files, paste0(startyear, "-", startSeason), paste0(startyear, "-", endSeason))

sndvi <- rast(season)

plot(sndvi[[1:9]])

library(agrin)
aoi <- ibli_data("marsabit")

aoi
head(aoi)

prj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

pols <-project(aoi, prj)

# 8-day NDVI spatial aggregates per region Extraction

ndvi_val <- terra::extract(sndvi,pols, fun=mean, na.rm=TRUE)

plot(ndvi_val[1,], ylab= "NDVI")


#All files processing

for(year in startyear:endyear) {
  season <- selectModisFiles(files, paste0(year, "-", startSeason), paste0(year, "-", endSeason))
  sndvi <- rast(season)
  ndvimean <- mean(sndvi, na.rm = TRUE)
  # Shorten band names
  names(sndvi) <- paste0("ndvi_",dateFromYearDoy(substr(names(sndvi), 10, 16)))
  filename=file.path(ndvidir, paste0( 'MOD09A1.h21v08.',year,"_",sSeason,'_ndvit.tif'))
  writeRaster(ndvimean, filename = filename , overwrite=TRUE)
}

par(mar = c(2.2, 2.2, 1.2, 0.5)) #c(bottom, left, top, right)
stitle <- paste(year, " NDVIs", sep="")
plot(ndvimean, main = stitle)

#Spatial-temporal NDVI mean aggregate (NDVIls)
# Define season s (LRLD in this case)
sSeason <- "LRLD"
files <- list.files(ndvidir, pattern=paste0(sSeason,"_ndvit.tif$"), full.names=TRUE)

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
saveRDS(res, file.path(ndvidir, paste0(sSeason,"_ndvi_st_mat.rds")))

#Plot NDVIls over the analysis years per region
for (i in 1:nrow(output)) {
  plot(output[i,],xaxt="none", ylab="Spatial-temporal NDVI aggregate", xlab="Year", main=paste0( as.character(pols$SUB_LOCATI[i]), " Sub-location.") )
  axis(1, at=c(1:length(files)), labels=colnames(output))
  lines(output[i,])
}

#Z-scores of spatial-temporal aggregates (zNDVIls)


zscore <- function(y){
  (y - mean(y, na.rm=TRUE) ) / (sd(y, na.rm=TRUE))
}


scores <- apply(res[,-c(1:2)], 1, zscore)
scores <- t(scores)
head(scores)

ndvi_st <- data.frame(SLNAME=pols$SUB_LOCATI, IBLI_Zone=pols$IBLI_UNIT, scores, stringsAsFactors = FALSE, check.names=FALSE)
saveRDS(ndvi_st, file.path(workdir, paste0(sSeason,"_zndvi_st_mat.rds")))

#We can show NDVIst deviations over the analysis years per sub-region. This is the approach 
#used for early assessment of forage

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
  lines(predict(m2), lty=4, col="green", lwd=2)
  # Moving average
  rmean5 <- stats::filter(scores[i,], rep(1/2, 2), sides = 2, method= "convolution")
  lines(rmean5, lty=6, col="cyan", lwd=2)
  #Add a legend
  pos <- ifelse(i==3, "topright", "bottomright")
  legend(pos, c("Data","Trigger", "Linear", "Quadratic", "Average"), col=c("black", "red","blue","green","cyan"), lty=c(1,2,3,4,6))
}

#Livestock Loss Prediction
# livestock loss data
loss <- ibli_data("S6C_Livestock_Losses")
# Check existing names before renaming
names(loss)

# Rename the column names
colnames(loss) <- c("hhid", "round", "comment", "lossevent", "year", "month","animal", "cause", "cause_other", "where", "totalloss" ,"adultloss" )
# Recheck new nams
names(loss)

# delete rows with missing years (-98 and -77)
i <- loss$year %in% c("-77", "-98")
loss <- loss[!i,]
# replace "missing" codes with NA
loss[loss=="-98"] <- NA
loss[loss=="-77"] <- NA

#Household Identification Information
house <- ibli_data("S0A_Household_Identification_Information")
dim(house)
## [1] 5538  100
# There are many variables so extract relevant ones only.
house <- house[,c(1:2,4,6,12:15,22:29)]
names(house)

#Now some data cleaning: informaton missing in 
#sloid is available in sublocation

#Convert for factor to character
house$slocid <- as.character(house$slocid)
house$slocid[is.na(house$slocid)] <- house$sublocation[is.na(house$slocid)]
#Check which sub-locations in household data are not in the boundary polygons
poly <- ibli_data("marsabit")
upol <- unique(poly$SUB_LOCATI)
uhh <- unique(house$slocid)
miss <- uhh[! (uhh %in% upol)]
miss
## [1] "KURKUM"    "LOGO LOGO" "LOSIDAN"   "ULAULI"
# compare with
sort(upol)

#We can make corrections

house$slocid[house$slocid=="LOGO LOGO"] <- "LOGOLOGO"
# Replace missing sublocation names with location names
for(i in 1:length(miss)){
  names <- unique(house$location[ house$slocid == miss[i] ] )
  newname <- names[!(names=="")]
  house$slocid[house$slocid ==  miss[i] ] <- newname
}
#Check if there are still any missing names
uhh <- unique(house$slocid)
uhh[!(uhh %in% unique(poly$SUB_LOCATI))]
## character(0)

#Livestock data
stock <- ibli_data("S6A_Livestock_Stock")
head(stock)

#Extract relevant columns
stock <- stock[,1:7]
#Rename columns
names(stock) <- c("hhid","round", "comment", "LivestockID ", "animaltype", "gender", "Herd_Size")
head(stock)

#Merge Survey data
dfr <- merge(house[,c(1:3,4,10)], loss[,c(1:2,5:12)], by=c("hhid","round"))
head(dfr)

#Seasonal household mortality rate per region

startyear = 2008
endyear = 2015
sStart <- 3 #Season start LRLD.
sEnd <- 9  #Season end LRLD.
mort <- matrix(nrow=length(unique(dfr$slocid)), ncol=length(startyear:endyear))
sid <- sort(unique(dfr$slocid)) #Sublocations
colnames(mort) <- as.character(c(startyear:endyear))
rownames(mort) <- sid

#Assume unknown months are December of the same year
dfr$month[is.na(dfr$month)] <- 12
for(r in 1:length(sid)){
  cat("Computing mortality for ", sid[r], "\n")
  rdf <- dfr[dfr$slocid==sid[r],]
  for(year in startyear:endyear){
    deaths  <- array(0, dim=length(sStart:sEnd))
    for (s in sStart:sEnd) {
      #NB: index (s)  will differ for LRLD & SRSD if data exists.
      i = s - 2
      deaths[i] <- mean(rdf$totalloss[rdf$year==as.character(year) & rdf$month== as.character(s)], na.rm=TRUE)
    }
    mort[sid[r], as.character(year)] <- sum(deaths, na.rm=TRUE)
  }
}

mort

#Convert to data frame
mort <- data.frame(SLNAME = sid, mort, stringsAsFactors = FALSE, check.names=FALSE,row.names = NULL)
#saveRDS(mort, file.path(work_folder, "Mortality.rds"))



#Prediction of livestock Mortality

sSeason <- "LRLD"
#Drop 2014
m <- mort[ , -8]
m
# Add IBLI zones
m$IBLI_Zone <- poly$IBLI_UNIT[poly$SUB_LOCATI %in% m$SLNAME]

#load spatial-temporal seasonal NDVi

zndvit_res <- ibli_data(paste0(sSeason,"_zndvi_st_mat.rds"))
# Extract only NDVI IBLI zones available in household data
zndvit_res <- zndvit_res[(zndvit_res$IBLI_Zone %in% m$IBLI_Zone) & (zndvit_res$SLNAME %in% m$SLNAME), ]
n <- zndvit_res
#Ensure they are IIBLI zones and sublocations are identical
identical(sort(unique(n$IBLI_Zone)),sort(unique(m$IBLI_Zone)))
## [1] FALSE
identical(as.character(sort(unique(n$SLNAME))), sort(unique(m$SLNAME)))
## [1] TRUE
#Merge similar zones as per table.
m$IBLI_Zone[m$IBLI_Zone=="CENTRAL MARSABIT"] <- "GADAMOJI"
n$IBLI_Zone[n$IBLI_Zone=="CENTRAL MARSABIT"] <- "GADAMOJI"
m$IBLI_Zone[m$IBLI_Zone=="KARGI"] <- "LOIYANGALANI"
n$IBLI_Zone[n$IBLI_Zone=="KARGI"] <- "LOIYANGALANI"
m$IBLI_Zone[m$IBLI_Zone=="TURBI"] <- "MAIKONA"
n$IBLI_Zone[n$IBLI_Zone=="TURBI"] <- "MAIKONA"
m$IBLI_Zone[m$IBLI_Zone=="MT KULAL"] <- "LOIYANGALANI"
n$IBLI_Zone[n$IBLI_Zone=="MT KULAL"] <- "LOIYANGALANI"


#Here is an example regression for a single zone
zone <- m$IBLI_Zone[1]
#Livestock loss for modelling 2008-2013
y = as.matrix(m[m$IBLI_Zone==zone, 2:7], row.names = NULL)
#NDVI for modelling 2008-2013
x = as.matrix(n[n$IBLI_Zone==zone, 3:8], row.names = NULL)
#NDVI for predicting loss in 2015 (excluded from model)
z = as.matrix(n[n$IBLI_Zone==zone, 10], row.names = NULL)
#Livestock loss for validating NDVI predictions in 2015
val = as.matrix(m[m$IBLI_Zone==zone, 8], row.names = NULL)
data <- data.frame(NDVI=as.vector(t(x)), Loss=as.vector(t(y)))
# Plot the data
plot(data, pch=16)
# Create a linear regression model
l.model <- lm(Loss~NDVI, data=data)
# Add the fitted line
abline(l.model, col="red")

#How did our linear regression model perform?
# make a prediction of mortality from each NDVI
predictMort <- predict.lm(l.model)
# display the predictions
plot(data, pch=16)
points(data$NDVI, predictMort, col = "blue", pch=4)
legend("topright",c("Data","Fitted"),pch= c(16, 4),col=c("black", "blue"))

#We can also do another plot with the 90% confidence interval
library(ggplot2)
ggplot(data = data, aes(x = NDVI, y = Loss)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3", level=0.90) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data at 90% CI")+
  xlim(min(data$NDVI), max(data$NDVI))

#Let us plot the residuals of our model
ggplot(data=data, aes(l.model$residuals)) +
  geom_histogram(binwidth = 2, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

#We can additionally examine the statistics from the model.
summary(l.model)

#Now lets us use the model to make predictions for livestock loss in 2015

#Fit a second order polynomial (quadratic model) and use it to makes predictions too
l.poly = lm(Loss~NDVI+I(NDVI^2), data=data)
#Quadaratic vs linear model
summary(l.model)

summary(l.poly)
#Predict mortality using the data we left out of the model
m.est1 <- predict.lm(l.model, data.frame(NDVI= as.vector(t(z))) )
m.est1

m.est2 <- predict.lm(l.poly, data.frame(NDVI= as.vector(t(z))) )
m.est2

# What is the difference between quadratic and linear model-above ?

#Root Mean Square Error (RMSE) to evaluate the models

rmse <- function(error){
  sqrt(mean(error^2))
}

rmse_lin <- rmse(m.est1-t(val))
rmse_lin
## [1] 16.64148
rmse_quad <- rmse(m.est2-t(val))
rmse_quad
## [1] 15.28618

#Exploration Support Vector Machines prediction 
# and compare with the above models
library(e1071)
svm.model <- svm(Loss ~ NDVI, data=data)
svm.pred <- predict(svm.model, data)
plot(data, pch=16)
points(data$NDVI, svm.pred, col = "blue", pch=4)
legend("topright",c("Data","SVM"),pch= c(16, 4),col=c("black", "blue"))

#Predict mortality using the data we left out of the model
svm.est <- predict(svm.model, data.frame(NDVI= as.vector(t(z))) )
# Compute RMSE
rmse_svm <- rmse(svm.est-t(val))
rmse_svm
## [1] 22.87761


# perform a grid search
tune.svm <- tune(svm, Loss ~ NDVI,  data = data,
                 ranges = list(epsilon = seq(0,1,0.1), cost = 2^(1:9))
)
print(tune.svm)

# Draw the tuning graph
plot(tune.svm)

#Predict mortality using the data we left out of the model
svm.est <- predict(tune.svm$best.model, data.frame(NDVI= as.vector(t(z))) )
# Compute RMSE
rmse_svm <- rmse(svm.est-t(val))
rmse_svm
## [1] 17.49348

#Exercise: Tune the SVM model and determine if there are 
# any differences

zone <- unique(m$IBLI_Zone)
for(ii in 1:length(unique(m$IBLI_Zone))){
  cat("Predicting livestock loss in ", as.character(zone[ii]), "\n")
  y = as.matrix(m[m$IBLI_Zone==zone[ii], 2:7], row.names = NULL)
  x = as.matrix(n[n$IBLI_Zone==zone[ii], 3:8], row.names = NULL)
  z = as.matrix(n[n$IBLI_Zone==zone[ii], 10], row.names = NULL)
  val = as.matrix(m[m$IBLI_Zone==zone[ii], 8], row.names = NULL)
  data <- data.frame(NDVI=as.vector(t(x)), Loss=as.vector(t(y)))
  #Modelling
  linear.model    <- lm(Loss~NDVI, data=data)
  quadratic.model <- lm(Loss~NDVI+I(NDVI^2), data=data)
  svm.model <- svm(Loss ~ NDVI,  data = data)
  #Predictions
  linear.est <- predict.lm(linear.model, data.frame(NDVI= as.vector(t(z))) )
  cat("RMSE for linear model in ", as.character(zone[ii]),
      " is ", rmse(linear.est-t(val)), "\n")
  quadratic.est <- predict.lm(quadratic.model, data.frame(NDVI= as.vector(t(z))) )
  cat("RMSE for quadratic model in ", as.character(zone[ii]),
      " is ", rmse(quadratic.est-t(val)), "\n")
  svm.est <- predict(svm.model, data.frame(NDVI= as.vector(t(z))) )
  cat("RMSE for SVM model in ", as.character(zone[ii]),
      " is ", rmse(svm.est-t(val)), "\n")
  #linear
  plot(data, pch=16, main= paste0(zone[ii], " Livestock loss predictions"))
  points(data$NDVI, predict(linear.model, data), col = "blue", pch=4)
  abline(linear.model, col="red", lty=1)
  legend("topright",c("Data","lm", "Model line"), pch= c(16, 4, NA), col=c("black", "blue", "red"), lty =c(NA,NA, 1) )
}
## Predicting livestock loss in  MAIKONA
## RMSE for linear model in  MAIKONA  is  16.64148
## RMSE for quadratic model in  MAIKONA  is  15.28618
## RMSE for SVM model in  MAIKONA  is  22.87761
## Predicting livestock loss in  GADAMOJI
## RMSE for linear model in  GADAMOJI  is  2.017486
## RMSE for quadratic model in  GADAMOJI  is  5.64951
## RMSE for SVM model in  GADAMOJI  is  4.59601

#Assessment of forage

head(basename(files))

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
