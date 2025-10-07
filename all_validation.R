# Set the path to library
.libPaths("G:/Ph.D Research Work/Codes/Rstudio/Library")

# Install the required packages (only first time)
install.packages("raster")
install.packages("sf")
install.packages("pROC")
install.packages("caret")   # for confusionMatrix
install.packages("RColorBrewer") # for attractive bar plot colors

# Load libraries
library(raster)
library(sf)
library(pROC)
library(caret)
library(RColorBrewer)

# Load raster and shapefiles
Flood2020 <- raster("G:/Ph.D Research Work/3rd Paper database/Raster dataset/output flood/flood2020.tif")
Training  <- st_read("G:/Ph.D Research Work/3rd Paper database/Shapefile/Training.shp")
Testing   <- st_read("G:/Ph.D Research Work/3rd Paper database/Shapefile/testing.shp")

# Extract raster values at training/testing points
train_pred <- extract(Flood2020, as(Training, "Spatial"))
test_pred  <- extract(Flood2020, as(Testing, "Spatial"))

# Create data frames
train_data <- data.frame(
  observed  = Training$Training,
  predicted = train_pred
)
test_data <- data.frame(
  observed  = Testing$Testing,
  predicted = test_pred
)

# Remove NA values
train_data <- na.omit(train_data)
test_data  <- na.omit(test_data)

# ----------------- ROC / AUC -----------------
roc_train <- roc(train_data$observed, train_data$predicted)
roc_test  <- roc(test_data$observed,  test_data$predicted)

auc_train <- auc(roc_train)
auc_test  <- auc(roc_test)
cat("Training AUC:", auc_train, "\n")
cat("Testing AUC:", auc_test, "\n")

# ----------------- Other Metrics -----------------
threshold <- 0.5  # or coords(roc_test, "best")$threshold

train_class <- ifelse(train_data$predicted >= threshold, 1, 0)
test_class  <- ifelse(test_data$predicted  >= threshold, 1, 0)

train_obs <- factor(train_data$observed, levels = c(0,1))
test_obs  <- factor(test_data$observed,  levels = c(0,1))
train_cls <- factor(train_class,         levels = c(0,1))
test_cls  <- factor(test_class,          levels = c(0,1))

cm_train <- confusionMatrix(train_cls, train_obs, positive = "1")
cm_test  <- confusionMatrix(test_cls,  test_obs,  positive = "1")

# -------- Function to calculate Matthews Correlation Coefficient (MCC) --------
mcc <- function(cm) {
  tp <- cm$table[2,2]
  tn <- cm$table[1,1]
  fp <- cm$table[1,2]
  fn <- cm$table[2,1]
  numerator   <- (tp*tn) - (fp*fn)
  denominator <- sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
  return(numerator/denominator)
}

# -------- Create Metrics Table --------
metrics <- data.frame(
  Dataset       = c("Training","Testing"),
  AUC           = c(auc_train, auc_test),
  Accuracy      = c(cm_train$overall["Accuracy"], cm_test$overall["Accuracy"]),
  Recall        = c(cm_train$byClass["Sensitivity"], cm_test$byClass["Sensitivity"]),
  Precision     = c(cm_train$byClass["Pos Pred Value"], cm_test$byClass["Pos Pred Value"]),
  Specificity   = c(cm_train$byClass["Specificity"], cm_test$byClass["Specificity"]),
  F1            = c(cm_train$byClass["F1"], cm_test$byClass["F1"]),
  Kappa         = c(cm_train$overall["Kappa"], cm_test$overall["Kappa"])
)

# -------- Table Output --------
print(metrics, row.names = FALSE)
#save to directory in csv
write.csv(metrics, "G:/Ph.D Research Work/3rd Paper database/CSV/validation with all metrics.csv", row.names = FALSE)
