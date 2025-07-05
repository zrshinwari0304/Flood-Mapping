###The following codes were used for the downscaling of low resolution data in paper
##this is example code

#data downloading using XGboost model
# Set working directory (adjust path as needed)
setwd("G:/Rstudio")
getwd() # for checking

#load the path to library
#set the library path
.libPaths("G:/Rstudio/Library")
#installed the required pacakges
install.packages("sf")
install.packages("sp")
install.packages("terra")
install.packages("carnet")
install.packages("xgboost")
install.packages("parallel")


# Loaded required packages
library(raster)
library(sf)
library(sp)
library(terra)
library(caret)
library(xgboost)
library(parallel)


# 1. Load the coarseresolution raster files
list.files("G:/For example population data")
coarse_raster <- raster("G:/Loaded the coarse resolution file here/for example/population.tif")

# 2. Create target grid at 100m resolution
fine_grid <- disaggregate(coarse_raster, fact=33)  # 1000m/30m = 33

# 3. Load predictor variables (covariates) at 100m resolution
# Example covariates: elevation, slope, land cover (you'll need these files)
list.files("G:/Rstudio/covariates")
#example of the data used as covariates

elevation <- raster("G:/Rstudio/covariates/ELEVATION.tif")
slope <- raster("G:/Rstudio/covariates/SLOPE.tif")
landcover <- raster("G:/Rstudio/covariates/landcover.tif")


# Stack covariates
covariates <- stack(elevation, slopem, landcover)


# 4. Extract training data
# Aggregate covariates to 1000m for training
covariates_coarse <- aggregate(covariates, fact=33, fun=mean)

# Extract values
train_data <- as.data.frame(getValues(stack(coarse_raster, covariates_coarse)))
colnames(train_data) <- c("target", "elevation", "slope", "landcover")

# Remove NA values
train_data <- na.omit(train_data)

# 5. Split data into training and validation sets
set.seed(123)
train_idx <- createDataPartition(train_data$target, p=0.7, list=FALSE) # P is proportion, 0.7 is 70%
train_set <- train_data[train_idx,]
valid_set <- train_data[-train_idx,]

# 6. Train XGBoost model
xgb_train <- xgb.DMatrix(data = as.matrix(train_set[,2:3]), #the 2:3 mean that from varible 2 to 3, its depend on the number of variables
                         label = train_set$target)
xgb_valid <- xgb.DMatrix(data = as.matrix(valid_set[,2:3]), 
                         label = valid_set$target)

xgb_params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100,
  watchlist = list(train = xgb_train, valid = xgb_valid),
  early_stopping_rounds = 10,
  verbose = 1
)

# 8. Prepare prediction data at 30m resolution
pred_data <- as.data.frame(getValues(covariates))
colnames(pred_data) <- c("elevation", "slope", "landcover")
pred_data <- na.omit(pred_data)

#Predict using both models
# XGBoost prediction
xgb_pred_matrix <- xgb.DMatrix(data = as.matrix(pred_data))
xgb_pred <- predict(xgb_model, xgb_pred_matrix)



#Create output rasters
xgb_raster <- fine_grid
values(xgb_raster) <- xgb_pred


# Save the results
writeRaster(xgb_raster, "G:/downscaled layer/population.tif", 
            format="GTiff", overwrite=TRUE)

# Visualize results
par(mfrow=c(1,3))
plot(coarse_raster, main="Original 1000m")
plot(xgb_raster, main="XGBoost 30m")

#Calculate basic statistics
print("XGBoost Model Performance:")
xgb_rmse <- sqrt(mean((valid_set$target - predict(xgb_model, as.matrix(valid_set[,2:3])))^2))
print(paste("RMSE:", round(xgb_rmse, 3)))

