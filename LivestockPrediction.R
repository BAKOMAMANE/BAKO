## ---- global_options, tidy = TRUE, echo=FALSE, message=FALSE-------------
library(knitr)
library(agrin)
opts_chunk$set(tidy.opts=list(width.cutoff=60))



## ----lossvariables, echo=FALSE-------------------------------------------
x <- c("hhid", "Round", "comment", "lossevent", "s6q20a", "s6q20b", "s6q21", "s6q22", "s6q22b", "s6q23", "s6q24", "s6q25", "", "", "", "", "year", "month", "animal", "cause", "cause_other", "where", "totalloss", "adultloss", "unique ID assigned to each interviewed household that is consistent over all rounds", "indicator for annual household surveys done between October-November", "1 if there is comment in the 'S0B Comment.dta' file, 0 otherwise", "Loss event ID", "Year of loss event (between October previous year and September current year)", "Month of loss event (between October previous year and September current year)", "What type of animal?", "What was the reason for this loss event?", "If other, specify the reason", "At your base camp or satellite camp?", "How many of animals were lost?", "Number of adult animals lost (3 years or more for cattle/camel, 6 months or more for sheep/goat)")
x <- matrix(x, ncol=3)
colnames(x) <- c("name", "new name",  "description")
knitr::kable(x, caption="Livestock losses variables")


## ----Household1----------------------------------------------------------
datadir <- file.path(dirname(tempdir()), "modis")
dir.create(datadir, showWarnings=FALSE)
library(agrin)
# livestock loss data
loss <- ibli_data("S6C_Livestock_Losses")
# Check existing names before renaming
names(loss)
# Rename the column names
colnames(loss) <- c("hhid", "round", "comment", "lossevent", "year", "month","animal", "cause", "cause_other", "where", "totalloss"	,"adultloss" )
# Recheck new nams
names(loss)


## ----Household2----------------------------------------------------------
# delete rows with missing years (-98 and -77)
i <- loss$year %in% c("-77", "-98")
loss <- loss[!i,]

# replace "missing" codes with NA
loss[loss=="-98"] <- NA
loss[loss=="-77"] <- NA


## ----hhidtab, echo=FALSE-------------------------------------------------
x <- c("TLU_class","Tropical Livestock Unit Class. 1 TLU is equivalent to 1 cow, 0.7 camel, 10 goat, or 10 sheep/goats (also referred to as 'shoats')","slocid","Sub-location ID","interview_day"," date of the interview","interview_month","month of the interview","dateinterview","date, month and year of the interview","district","name of the district","division","name of the division","location","name of the location","sublocation","sub-location name (original)","sublocation-16","sub-location within the 16 study sub-locations","sub-location-newhh","Sub-location","village","Village","village_newhh","village","hhid","a unique ID assigned to each interviewed household that is consistent over all rounds","Round","indicator for annual household surveys done between October-November","comment","1 if there is comment in 'S0B Comment.dta' file, 0 otherwise")

m <- matrix(x, ncol=2,byrow=TRUE)
colnames(m) <- c("varname", "description")
knitr::kable(m, caption="Houshold data")


## ----hhid----------------------------------------------------------------
house <- ibli_data("S0A_Household_Identification_Information")
dim(house)

# There are many variables so extract relevant ones only.
house <- house[,c(1:2,4,6,12:15,22:29)]	
names(house)


## ----hhid2---------------------------------------------------------------
#Convert for factor to character
house$slocid <- as.character(house$slocid)
house$slocid[is.na(house$slocid)] <- house$sublocation[is.na(house$slocid)]

#Check which sub-locations in household data are not in the boundary polygons
poly <- ibli_data("marsabit")

upol <- unique(poly$SUB_LOCATI)
uhh <- unique(house$slocid)
miss <- uhh[! (uhh %in% upol)]
miss
# compare with
sort(upol)


## ----hhid3---------------------------------------------------------------
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



## ----HerdTable, echo=FALSE-----------------------------------------------
x <- c("hhid", "Unique ID assigned to each interviewed household that is consistent over all rounds", "Round", "Indicator for annual household surveys done between October-November", "comment", "1 if there is comment in 'S0B Comment.dta' file, 0 otherwise","LivestockID", "Livestock type and gender ID","animaltype", "Type of animal","gender", "Gender of animal","s6q1","Number of herded animals that are owned by household")
m <- matrix(x, ncol=2, byrow=TRUE)
colnames(m) <- c("varname", "description")
knitr::kable(m)


## ----HerdStock-----------------------------------------------------------
stock <- ibli_data("S6A_Livestock_Stock")
head(stock)

#Extract relevant columns
stock <- stock[,1:7]	
#Rename columns
names(stock) <- c("hhid","round", "comment", "LivestockID ", "animaltype", "gender", "Herd_Size")
head(stock)


## ----MergeHouse----------------------------------------------------------
dfr <- merge(house[,c(1:3,4,10)], loss[,c(1:2,5:12)], by=c("hhid","round"))
head(dfr)
#saveRDS(df, paste0(work_folder, "output/Household/Household_Merged.rds"))


## ----Mortality-----------------------------------------------------------
startyear = 2008
endyear = 2015
sStart <- 3 #Season start LRLD.
sEnd <- 9  #Season end LRLD.

mort <- matrix(nrow=length(unique(dfr$slocid)), ncol=length(startyear:endyear))
sid <- sort(unique(dfr$slocid)) #Sublocations

colnames(mort) <- as.character(c(startyear:endyear))
rownames(mort) <- sid


## ----Mortality2----------------------------------------------------------

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



## ----IBLIZones, echo=FALSE-----------------------------------------------
#options(knitr.kable.NA = '')
dm <- read.table("IBLI_Zones.txt", header=TRUE, sep="\t", colClasses = "character")
colnames(dm) <- c("ID", "Sub-location", "Insurance zone")
knitr::kable(dm, align="l", escape=FALSE, caption="Insurance zones adopted by IBLI.")


## ----prediction1---------------------------------------------------------
sSeason <- "LRLD"
#Drop 2014 
m <- mort[ , -8] 

# Add IBLI zones 
m$IBLI_Zone <- poly$IBLI_UNIT[poly$SUB_LOCATI %in% m$SLNAME]

#load spatial-temporal seasonal NDVi
zndvit_res <- ibli_data(paste0(sSeason,"_zndvi_st_mat.rds"))

# Extract only NDVI IBLI zones available in household data
zndvit_res <- zndvit_res[(zndvit_res$IBLI_Zone %in% m$IBLI_Zone) & (zndvit_res$SLNAME %in% m$SLNAME), ]
n <- zndvit_res

#Ensure they are IIBLI zones and sublocations are identical
identical(sort(unique(n$IBLI_Zone)),sort(unique(m$IBLI_Zone)))
identical(as.character(sort(unique(n$SLNAME))), sort(unique(m$SLNAME)))

#Merge similar zones as per table.
m$IBLI_Zone[m$IBLI_Zone=="CENTRAL MARSABIT"] <- "GADAMOJI"
n$IBLI_Zone[n$IBLI_Zone=="CENTRAL MARSABIT"] <- "GADAMOJI"

m$IBLI_Zone[m$IBLI_Zone=="KARGI"] <- "LOIYANGALANI"
n$IBLI_Zone[n$IBLI_Zone=="KARGI"] <- "LOIYANGALANI"

m$IBLI_Zone[m$IBLI_Zone=="TURBI"] <- "MAIKONA"
n$IBLI_Zone[n$IBLI_Zone=="TURBI"] <- "MAIKONA"

m$IBLI_Zone[m$IBLI_Zone=="MT KULAL"] <- "LOIYANGALANI"
n$IBLI_Zone[n$IBLI_Zone=="MT KULAL"] <- "LOIYANGALANI"


## ----predictionExample---------------------------------------------------

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



## ----prediction2---------------------------------------------------------
# make a prediction of mortality from each NDVI
predictMort <- predict.lm(l.model)

# display the predictions
plot(data, pch=16)
points(data$NDVI, predictMort, col = "blue", pch=4)
legend("topright",c("Data","Fitted"),pch= c(16, 4),col=c("black", "blue"))


## ----prediction22--------------------------------------------------------
library(ggplot2)
ggplot(data = data, aes(x = NDVI, y = Loss)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3", level=0.90) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data at 90% CI")+
  xlim(min(data$NDVI), max(data$NDVI))


## ----prediction222-------------------------------------------------------
ggplot(data=data, aes(l.model$residuals)) +
  geom_histogram(binwidth = 2, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")


## ----prediction3---------------------------------------------------------
summary(l.model)


## ----prediction4---------------------------------------------------------
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



## ----rmse----------------------------------------------------------------
rmse <- function(error){
  sqrt(mean(error^2))
}
rmse_lin <- rmse(m.est1-t(val))
rmse_lin
rmse_quad <- rmse(m.est2-t(val))
rmse_quad



## ----svm1----------------------------------------------------------------
library(e1071)
svm.model <- svm(Loss ~ NDVI, data=data)
svm.pred <- predict(svm.model, data)
plot(data, pch=16)
points(data$NDVI, svm.pred, col = "blue", pch=4)
legend("topright",c("Data","SVM"),pch= c(16, 4),col=c("black", "blue"))



## ----svm2----------------------------------------------------------------
#Predict mortality using the data we left out of the model
svm.est <- predict(svm.model, data.frame(NDVI= as.vector(t(z))) )
# Compute RMSE
rmse_svm <- rmse(svm.est-t(val))
rmse_svm


## ----svm3----------------------------------------------------------------
# perform a grid search
tune.svm <- tune(svm, Loss ~ NDVI,  data = data,
                 ranges = list(epsilon = seq(0,1,0.1), cost = 2^(1:9))
)
print(tune.svm)
# Draw the tuning graph
plot(tune.svm)



## ----svm4----------------------------------------------------------------
#Predict mortality using the data we left out of the model
svm.est <- predict(tune.svm$best.model, data.frame(NDVI= as.vector(t(z))) )
# Compute RMSE
rmse_svm <- rmse(svm.est-t(val))
rmse_svm



## ----PredictZones--------------------------------------------------------
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


## ----SwitchingRegression, include=FALSE----------------------------------
zone <- unique(m$IBLI_Zone)
for(ii in 1:length(unique(m$IBLI_Zone))){
  cat("Predicting livestock loss in ", as.character(zone[ii]), "\n")
  y = as.matrix(m[m$IBLI_Zone==zone[ii], 2:7], row.names = NULL)
  x = as.matrix(n[n$IBLI_Zone==zone[ii], 3:8], row.names = NULL)
  z = as.matrix(n[n$IBLI_Zone==zone[ii], 10], row.names = NULL)
  val = as.matrix(m[m$IBLI_Zone==zone[ii], 8], row.names = NULL)
  data <- data.frame(NDVI=as.vector(t(x)), Loss=as.vector(t(y)))
  #Modelling
  if(data$NDVI > 0){
    linear.model    <- lm(Loss~NDVI, data=data) 
    quadratic.model <- lm(Loss~NDVI+I(NDVI^2), data=data)
    svm.model <- svm(Loss ~ NDVI,  data = data) 
    #Predictions
    linear.est <- predict.lm(linear.model, data.frame(NDVI= as.vector(t(z))) )
    cat("RMSE for linear model in ", as.character(zone[ii]), 
        " when NDVI > 0 is ", rmse(linear.est-t(val)), "\n")
    quadratic.est <- predict.lm(quadratic.model, data.frame(NDVI= as.vector(t(z))) )
    cat("RMSE for quadratic model in ", as.character(zone[ii]), 
        " when NDVI > 0 is ", rmse(quadratic.est-t(val)), "\n")
    svm.est <- predict(svm.model, data.frame(NDVI= as.vector(t(z))) )
    cat("RMSE for SVM model in ", as.character(zone[ii]), 
        " when NDVI > 0 is ", rmse(svm.est-t(val)), "\n")
  } else {
    
    linear.model2    <- lm(Loss~NDVI, data=data) 
    quadratic.model2 <- lm(Loss~NDVI+I(NDVI^2), data=data)
    svm.model2 <- svm(Loss ~ NDVI,  data = data) 
    #Predictions
    linear.est <- predict.lm(linear.model2, data.frame(NDVI= as.vector(t(z))) )
    cat("RMSE for linear model in ", as.character(zone[ii]), 
        " when NDVI < 0 is ", rmse(linear2.est-t(val)), "\n")
    quadratic2.est <- predict.lm(quadratic2.model, data.frame(NDVI= as.vector(t(z))) )
    cat("RMSE for quadratic model in ", as.character(zone[ii]), 
        " when NDVI < 0 is ", rmse(quadratic2.est-t(val)), "\n")
    svm.est2 <- predict(svm.model2, data.frame(NDVI= as.vector(t(z))) )
    cat("RMSE for SVM model in ", as.character(zone[ii]), 
        " when NDVI < 0 is ", rmse(svm.es2t-t(val)), "\n")
  }
}  

