# Load necessary libraries
library(dplyr)   # for data manipulation
library(ggplot2) # for data visualization
library(caret)   # for machine learning workflows

# Load the dataset
house_data <- read.csv("house_prices.csv")

# Explore the structure and summary of the dataset
str(house_data)
summary(house_data)

# Data cleaning and preprocessing
# Example steps: handling missing values, removing outliers, normalization
# Replace with your actual data cleaning steps
# For example:
# house_data <- na.omit(house_data)  # Remove rows with missing values
# house_data <- house_data[!apply(house_data, 1, function(x) any(is.na(x))), ]  # Alternative way to remove rows with NA values
# house_data <- house_data[house_data$price <= 1000000, ]  # Example of removing outliers based on price

# Split data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(house_data$price, p = 0.8, list = FALSE)
train_data <- house_data[train_index, ]
test_data <- house_data[-train_index, ]

# Model building - Linear Regression
lm_model <- lm(price ~ size + location + rooms + amenities, data = train_data)

# Model summary
summary(lm_model)

# Predictions on test set
predictions <- predict(lm_model, newdata = test_data)

# Evaluation metrics
rmse <- sqrt(mean((predictions - test_data$price)^2))
r_squared <- cor(predictions, test_data$price)^2

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Plotting actual vs. predicted prices
ggplot() +
  geom_point(data = test_data, aes(x = price, y = predictions), color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Price", y = "Predicted Price", title = "Actual vs. Predicted House Prices")

# Further analysis: Feature importance, cross-validation, etc.
