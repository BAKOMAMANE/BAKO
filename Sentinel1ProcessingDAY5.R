
workdir <- file.path("D:/RCMRD/day5")
datadir <- file.path(workdir, "agrin/sentinel")
dir.create(datadir, showWarnings = FALSE, recursive=TRUE)
library(agrin)
datadir
## [1] "c:/temp/agrin/sentinel"
ff <- crop_data("sentinel1", datadir)
head(ff)

#Use Gep function to get VV
fvv <- grep("VV", ff, value = TRUE)
vv <- rast(fvv)
# make prettier (shorter) layer names
names(vv) <- substr(names(vv), 15, 27)
names(vv)

vv

# Repeat the steps for vh polarization
fvh <- grep("VH", ff, value = TRUE)
vh <- rast(fvh)
names(vh) <- substr(names(vh), 15, 27)
names(vh)
vh

# Field crop data
cropref <- crop_data("crop_ref")
head(cropref)

table(cropref$class)

#Create a vector plot

vecref <- vect(cropref[,1:2], att=cropref, crs=crs(vv))
vecref

spplot(vecref, "class")

#Feature creature

compFeatures <- function(x, name){
  # standard deviation
  stdev <- stdev(x)
  # quantiles
  quant <- app(x, function(i) quantile(i, c(.1, .25, .5, .75, .9)))
  # trend
  n <- nlyr(x)
  trend <- 100 * (mean(x[[1:5]] - mean(x[[(n-4):n]])))
  feat <- c(quant, stdev, trend)
  names(feat) <- paste0(name, c(".1", ".25", ".5", ".75", ".9", ".sd", "trend"))
  return(feat)
}

#Compute seasonal composite features from VV polarized images

fvv <- file.path(workdir, "vv_ft.tif")

if (!file.exists(fvv)) {
  vv_ft <- compFeatures(vv, "vv")
  writeRaster(vv_ft, fvv, overwrite = TRUE)
} else {
  vv_ft <- rast(fvv)
}

vv_ft

plot(vv_ft)

library(raster)

fvh <- file.path(workdir, "vh_ft.tif")
if (!file.exists(fvh)) {
  sd <- stdev(vh)
  mn <- mean(vh)
  vh_ft <- c(mn, sd)
  names(vh_ft) <- c("mean", "stdev")
  writeRaster(vh_ft, fvh, overwrite = TRUE)
} else {
  vh_ft <-- rast(fvh)
}

vh_ft

plot(vh_ft)

#combine vv and vh images 
img = c(vv_ft, vh_ft)

#Model iltering
ref <- extract(img, vecref, drop=TRUE)
ref <- data.frame(class=cropref$class, ref)
head(ref)

#Train the model
library(randomForest)
set.seed(888)
rf.m <- randomForest(class~., data=ref, ntree=250, importance=TRUE)

rf.m

#Model object plotting
cols <- c("green", "yellow", "darkgreen", "magenta", "blue")
plot(rf.m, col=cols, lwd=3)
legend("topright", col=cols , c("OOB", "Grass", "Maize","Pasture","Wheat"), lty=1, lwd=3)

varImpPlot(rf.m, main="")

importance(rf.m)

partialPlot(rf.m, ref, "vvtrend", "Maize")

partialPlot(rf.m, ref, "vvtrend", "Grass")

#Model prediction
rf.pred <- predict(img, rf.m)
rf.pred

#Make a map
rfp <- as.factor(rf.pred)
levels(rfp) <- c("Grassland", "Maize", "Pasture", "Wheat")
plot(rfp, col=c("green", "yellow", "darkgreen", "magenta"))

#Model evaluation
library(luna)
set.seed(530)
k <- kfold(ref, 5)
nrow(ref) / 5
## [1] 400
table(k)

#Get training and testing samples
train <- ref[k != 1, ]
test  <- ref[k == 1, ]

#Build a new model
rf <- randomForest(class~., data=train, ntree=250, importance=TRUE)

#prediction for the test data
pred <- predict(rf, test)

#Create a confusion matrix
conmat <- table(observed=test$class, predicted=pred)
conmat

#Calculate one or overall statistics
# overall accuracy
evaluate(conmat, "overall")
## [1] 0.6425
# kappa
evaluate(conmat, "kappa")
## [1] 0.5223502
# user and producer accuracy
evaluate(conmat, "class")

rf.pred<- predict(img, ref.m)
