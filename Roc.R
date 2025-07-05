#the following code were used for the checking of Roc



# set the path to library
.libPaths("G:/Rstudio/Library")
#install the required packages
install.packages("raster")
install.packages("sf")
install.packages("pROC")

#loaded the required packages

library(raster)
library(sf)
library(pROC)

# Loaded the file for each year the raster file
#for example first for 2020
Flood2020 <- raster("G:/output flood/flood2020.tif")


# Loaded training and testing datasets
Training <- st_read("G:/Training.shp")
Testing <- st_read("G:/testing.shp")


# Extracted the raster values
train_pred <- extract(Flood2020, as(Training, "Spatial"))
test_pred <- extract(Flood2020, as(Testing, "Spatial"))

# Combined extracted values with observed class labels
train_data <- data.frame(
  observed = Training$Training,  
  predicted = train_pred
)

test_data <- data.frame(
  observed = Testing$Testing,  
  predicted = test_pred
)

# Removed NAs
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# Computed ROC curves
roc_train <- roc(train_data$observed, train_data$predicted)
roc_test <- roc(test_data$observed, test_data$predicted)

# AUC values
auc_train <- auc(roc_train)
auc_test <- auc(roc_test)

cat("Training AUC:", auc_train, "\n")
cat("Testing AUC:", auc_test, "\n")

# Set bold axis text and border
par(font.axis = 2,        
    font.lab = 2,         
    lwd = 2,              
    cex.axis = 1.1,       
    cex.lab = 1.2)        

# Plotted ROC curve 
plot(roc_train, col = "blue", lwd = 3,
     main = "ROC Curves for Training and Testing Data", 
     cex.main = 1.2, 
     xlab = "False Positive Rate", 
     ylab = "True Positive Rate")

# Added test ROC curve
lines(roc_test, col = "red", lwd = 3)

# Added legend
legend(x = 0.45, y = 0.25, 
       legend = c(paste("Training AUC =", round(auc_train, 4)),
                  paste("Testing AUC =", round(auc_test, 4))),
       col = c("blue", "red"), 
       lwd = 3, 
       text.font = 2, 
       bty = "n", 
       cex = 1.1)
#here we done with the ROC Curve
