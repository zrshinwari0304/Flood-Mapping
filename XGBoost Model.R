#This the example code used for the study of flood prediction
## Paper Title: Spatiotemporal dynamics of flood susceptibility under future precipitation variability, population growth, and land cover change


#set working directory
setwd("G:/Rstudio")
#create and set a library
.libPaths("G:/Rstudio/Library")

#load the required packages packages
library(xgboost)
library(sf)        
library(raster)
library(plyr)         
library(dplyr)         
library(RStoolbox)     
library(RColorBrewer) 
library(ggplot2)      
library(sp)           
library(caret)        
library(doParallel)   
library(doSNOW)
library(e1071)
library(raster)
library(sp)
# Import training and testing data ----
data_train <-  read.csv("G:/CSV/Training.csv", header = T)
data_train <-(na.omit(data_train))
data_train <-data.frame(data_train)  # to remove the unwelcome attributes
as.data.frame(table(data_train$Training))
#scale the data
maxs <- apply(data_train, 2, max) 
mins <- apply(data_train, 2, min)
scaled_train <- as.data.frame(scale(data_train, center = mins, scale = maxs - mins))
scaled_t <-scaled_train
scaled_t$Training <- ifelse(scaled_t$Training == 1, "yes","no")

# 2 Testing Data --------------------------------------------------------
data_test <-  read.csv("G:/CSV/Testing.csv", header = T)
data_test <-na.omit(data_test)
data_test <-data.frame(data_test)
as.data.frame(table(data_test$Testing))

# Scale the data
maxs <- apply(data_test, 2, max) 
mins <- apply(data_test, 2, min)
scaled_test <- as.data.frame(scale(data_test, center = mins, scale = maxs - mins))
scaled_tst <-scaled_test
scaled_tst$Testing <- ifelse(scaled_tst$Testing == 1, "yes","no")

# Create one file contain all data
names(scaled_tst)
scaled_tst$Slides=scaled_tst$Testing
scaled_t$Slides=scaled_t$Training
All_incidents <- merge(scaled_tst[,-1], scaled_t[,-1], all=TRUE) 
names(All_incidents)
All_incidents <- All_incidents[,c(13,1:12)]
scaled_tst$Slides= NULL  
scaled_t$Slides=NULL  
#######XGBoost##############################################################################################################################################
########  Run XGBoost function ------------------------------------------------
#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)
#Parameter for Tree Booster

tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance

#Train xgbTree model USING aLL dependent data
set.seed(849)
fit.xgbAll<- train(Slides~., 
                    data=All_incidents,
                    method = "xgbTree",
                    metric= "Accuracy",
                    preProc = c("center", "scale"), 
                    trControl = myControl,
                    tuneGrid = tune_grid,
                    tuneLength = 10,
                    importance = TRUE)


#Produce flood prediction map using Training model results and Raster layers data

# Load the all raster layer Raster data
list.files("G:/Ph.D Research Work/3rd Paper database/Raster dataset/resampled")
#example of flood predictors in raster to be loaded
EL=raster("G:/Ph.D Research Work/3rd Paper database/Raster dataset/resampled2020/EL.tif")

##Convert rasters to dataframe with Long-Lat
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df)
# remove x, y
Rasters.df_N <- Rasters.df[,c(-13,-14)] # remove x, y
# Now:Prediction using imported Rasters
head(Rasters.df_N)
##Scale the numeric variables --------------------------------------
# Check the relationship between the numeric varaibles, Scale the numeric var first!
maxss <- apply(Rasters.df_N, 2, max) 
minss <- apply(Rasters.df_N, 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N, center = minss, scale = maxss - minss)) 
head(Rasters.df_N_scaled)
##different step for every model
# PRODUCE PROBABILITY MAP #
p3<-as.data.frame(predict(fit.xgbAll, Rasters.df_N_scaled, type = "prob"))
summary(p3)
Rasters.df$Levels_yes<-p3$yes

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(EL))

# Plot Maps
spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="G:/output flood.tif", format="GTiff", overwrite=TRUE) 
X.xgb = varImp(fit.xgbAll)
print(X.xgb)
