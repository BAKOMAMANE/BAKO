# 1. Apply your knowledge!
 
# example data
zones <- c('Central', 'Eastern', 'Lake', 'Northern', 'Southern', 'Southern Highlands', 'Western') 
set.seed(2016)
v1 <- sample(100, length(zones))
v2 <- sample(100, length(zones))
m <- cbind(v1, v2)


# we want to compute a new variable "v3" that is the sum of v1 and v2 capped at 100 (values larger than 100 are set to 100). 
# Show how you can do that in three ways (!)

# 1a. with vector algebra
v3 <- v1 + v2
v3[v3>100]<-100
v3

# 1b. with a loop and and a branch
v3 <- rep(NA, length(v1))
for (i in length(v1)) {
  v3[i] <- v1[i] + v2[i]
  if (v3[i]>100) {
     v3[i]<-100
  }
}
v3

# 1c. With apply
# first write a function
fun <- function(x) {
	z<- sum(x)
	z[z>100]<-100
	return(z)
}

# then use apply
v3 <- apply(m, 1, fun)
v3

install.packages(raster)

### More data processing

# Read FAO data on crop area, yield and production for all years, countries, crops
d <- read.csv("C:/arusha/Production_Crops_E_All_Data.csv", stringsAsFactors=FALSE)

# what are the variable names?

# Which variable names end on an F (for flag)? Use the "grep" function
f <- ....

# remove these flag variables from the data.frame
d <- ....

# select the values for Maize and Yield
y <- ....
y <- 


# Remove all "Countries" that are not really countries (see the unique combinations of 
# Country and Country.Code to figure out how to easily select these)



# Which fields have the values of interest (start with "Y")
# Either the names or the indices



# Change the units to Mg (ton) / ha


# Use "apply" to compute the mean maize yields for all countries for the years 2010 to 2014. 


# Make a new data.frame with the countries and the average yield computed above.



# Sort this data.frame by yield


# Remove missing values


# Plot the data and comment on data quality


 
# Extract the time series for Tanzania 


# plot time against yield

  
  ?terra
