## the following codes were used for checking the multi collienarity of the flood predictors
##this is example codes


## set the working directory
setwd("G:/Rstudio")

##library path
.libPaths("G:/Rstudio/Library")
#installed the required packages
install.packages("car")
# Loaded the required packages from library
library(car) # For calculating VIF

# Loadded the data in CSV file from the directory
data <- read.csv("flood/predictors with flooded data.csv")
list(data)

# Adjust the column names as per the dataset

#example of predictors used in the study
flood_factors <- data[, c("Training", "Pop2020", "Per_2020", "Elevation", "Slope", "drain_dens","TD","TPI","twi","NDVI","ndwi","LC_2020","Curvature")]
# Compute the correlation matrix
cor_matrix <- cor(flood_factors)
print(cor_matrix)


# Fit a linear model using the flood factors
model <- lm(Training ~ ., data = flood_factors)


# Calculate Variance Inflation Factor (VIF)
vif_values <- vif(model)

# Calculate Tolerance (TO)
tolerance <- 1 / vif_values

# Combine results into a data frame
vif_to_results <- data.frame(Factor = names(vif_values), VIF = vif_values, Tolerance = tolerance)

# Display results
print(vif_to_results)
# Save the printed data to a CSV file
write.csv(vif_to_results, "G:/Ph.D Research Work/3rd Paper database/CSV/vif_tol_results.csv", row.names = FALSE)

##__________________________NOW CREATE A GRAPH__________________________________________


library(ggplot2)

# Read the data from a CSV file
# Replace "your_data.csv" with the path to your CSV file
vif_tol_data <- read.csv("Loaded our data from our directory/vif_tol_results.csv")

# Clean the data by removing rows with missing values or invalid values in VIF and Tolerance
vif_tol_data_clean <- vif_tol_data[!is.na(vif_tol_data$VIF) & !is.na(vif_tol_data$Tolerance), ]

# Check the maximum values of VIF and Tolerance
max_VIF <- max(vif_tol_data_clean$VIF, na.rm = TRUE)
max_Tolerance <- max(vif_tol_data_clean$Tolerance, na.rm = TRUE)


# Adjust the plot
dual_axis_plot <- ggplot(vif_tol_data_clean, aes(x = reorder(Factor, -VIF))) +
  geom_line(aes(y = VIF, group = 1, color = "VIF"), size = 1.2) +
  geom_point(aes(y = VIF, color = "VIF"), size = 3) +
  geom_line(aes(y = Tolerance * max_VIF, group = 1, color = "Tolerance"), linetype = "dashed", size = 1.2) + # Scaling tolerance with max_VIF for clarity
  geom_point(aes(y = Tolerance * max_VIF, color = "Tolerance"), size = 3) +
  scale_y_continuous(
    name = "VIF", 
    limits = c(0, max_VIF + 2),  # Extend the VIF limits a little more
    breaks = seq(0, max_VIF + 2, 1),
    sec.axis = sec_axis(~ . / max_VIF, name = "Tolerance", breaks = seq(0, 1, 0.1))  # Adjusting tolerance axis breaks
  ) +
  scale_color_manual(
    name = "Metrics",
    values = c("VIF" = "red", "Tolerance" = "green"),
    labels = c("VIF", "Tolerance")
  ) +
  labs(
    title = "Multi-Collinearity Analysis", 
    x = "Predictors",
    y = "VIF"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # X-axis text customization
    axis.text.x = element_text(angle = 45, face = "bold", color = "black", size = 12, hjust = 1),
    # Y-axis text customization
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    # X-axis title customization
    axis.title.x = element_text(face = "bold", color = "black", size = 14), # Bold, larger, and black
    # Y-axis title customization
    axis.title.y = element_text(face = "bold", color = "black", size = 14), # Bold, larger, and black
    # Secondary Y-axis title customization
    axis.title.y.right = element_text(face = "bold", color = "black", size = 14), # For "Tolerance" axis
    # Title customization
    plot.title = element_text(face = "bold", hjust = 0.5),
    # Legend position
    legend.position = "top"
  )

# Display the plot
print(dual_axis_plot)

# Save the plot as a high-resolution image
ggsave("Multicollinearity.jpg", plot = dual_axis_plot, width = 10, height = 8, dpi = 600)
