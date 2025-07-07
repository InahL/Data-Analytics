library(ggplot2)

# Load dataset
epi_data <- read.csv("C:/Users/leei6/Data-Analytics/Project/Environmnetal Indicator Modeling and Regional Classification/epi_results_2024_pop_gdp.csv")

# Subset data for the two regions
west_data <- subset(epi_data, region == "Global West")
latam_data <- subset(epi_data, region == "Latin America & Caribbean")

# Create histogram plots with density lines
ggplot() +
  geom_histogram(data = west_data, aes(x = EPI.new, y = ..density..), 
                 bins = 20, fill = "blue", alpha = 0.5) +
  geom_density(data = west_data, aes(x = EPI.new), color = "blue", size = 1) +
  geom_histogram(data = latam_data, aes(x = EPI.new, y = ..density..), 
                 bins = 20, fill = "red", alpha = 0.5) +
  geom_density(data = latam_data, aes(x = EPI.new), color = "red", size = 1) +
  labs(title = "EPI.new Distribution: Global West vs Latin America & Caribbean", 
       x = "EPI.new", 
       y = "Density") +
  theme_minimal()


# QQ plots for both regions compared to a normal distribution
par(mfrow = c(1, 2))  # Set up plotting area for two plots
qqnorm(west_data$EPI.new, main = "QQ Plot: Global West", pch = 16, col = "blue")
qqline(west_data$EPI.new, col = "blue")
qqnorm(latam_data$EPI.new, main = "QQ Plot: Latin America & Caribbean", pch = 16, col = "red")
qqline(latam_data$EPI.new, col = "red")

#---------------------------------------------------------------
# Fit linear models using only GDP
epi_model <- lm(EPI.new ~ gdp, data = epi_data)
eco_model <- lm(ECO.new ~ gdp, data = epi_data)

epi_model_pop <- lm(EPI.new ~ population, data = epi_data)
eco_model_pop <- lm(ECO.new ~ population, data = epi_data)

# Print model summaries
summary(epi_model_pop)
summary(eco_model_pop)

# Print model summaries
summary(epi_model)
summary(eco_model)

# Plot response vs. GDP
ggplot(epi_data, aes(x = gdp, y = EPI.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "EPI.new vs GDP", x = "GDP", y = "EPI.new")

# Residuals plot
par(mfrow = c(1, 2))
plot(epi_model$fitted.values, epi_model$residuals, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "blue")
abline(h = 0, col = "red")

# Fit model for a specific region (Global West)
west_model <- lm(EPI.new ~ gdp, data = west_data)
summary(west_model)

# Fit linear models using GDP
epi_model_gdp <- lm(EPI.new ~ log(gdp), data = epi_data)

# Print model summary
summary(epi_model_gdp)

# Plot response vs. log-transformed GDP
ggplot(epi_data, aes(x = log(gdp), y = EPI.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "EPI.new vs Log(GDP)", x = "Log(GDP)", y = "EPI.new")

# Residuals plot
par(mfrow = c(1, 1))
plot(epi_model_gdp$fitted.values, epi_model_gdp$residuals, main = "Residuals", xlab = "Fitted Value", ylab = "Residual", pch = 16, col = "blue")
abline(h = 0, col = "red")


# Repeat for Global West
# Fit model for a specific region (Global West)
west_model_gdp <- lm(EPI.new ~ log(gdp), data = west_data)
summary(west_model_gdp)

# Plot response vs. GDP for Global West
ggplot(west_data, aes(x = log(gdp), y = EPI.new)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "EPI.new vs GDP (Global West)", x = "GDP", y = "EPI.new")

# Residuals plot for Global West model
par(mfrow = c(1, 1))
plot(west_model_gdp$fitted.values, west_model_gdp$residuals, main = "Residuals", xlab = "Fitted Value", ylab = "Residual", pch = 16, col = "blue")
abline(h = 0, col = "red")

#------------------------------------------------------
library(caret)
library(class)

# Select two regions for classification
selected_regions <- c("Global West", "Latin America & Caribbean")
kNN_data <- subset(epi_data, region %in% selected_regions)

# Choose three predictor variables (excluding GDP and population)
selected_vars <- c("EPI.new", "ECO.new", "GHN.new")
kNN_data <- kNN_data[, c(selected_vars, "region")]

# Normalize the predictor variables
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
kNN_data[, selected_vars] <- as.data.frame(lapply(kNN_data[, selected_vars], normalize))

# Convert region to factor for classification
kNN_data$region <- as.factor(kNN_data$region)

# Split into training and testing sets
set.seed(123)
train_index <- createDataPartition(kNN_data$region, p = 0.7, list = FALSE)
train_data <- kNN_data[train_index, ]
test_data <- kNN_data[-train_index, ]

# Train kNN model with k = 5
k_value <- 20
knn_pred <- knn(train = train_data[, selected_vars], test = test_data[, selected_vars], 
                cl = train_data$region, k = k_value)

# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(knn_pred), as.factor(test_data$region))
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
print(paste("Accuracy:", accuracy))


#--------------------------try other k
library(ggplot2)
library(class)
library(caret)

# Load dataset
epi_data <- read.csv("C:/Users/leei6/Data-Analytics/Project/Environmnetal Indicator Modeling and Regional Classification/epi_results_2024_pop_gdp.csv")
# Select two regions for classification
selected_regions <- c("Global West", "Latin America & Caribbean")
kNN_data <- subset(epi_data, region %in% selected_regions)

# Choose three predictor variables (excluding GDP and population)
selected_vars <- c("EPI.new", "ECO.new", "GHN.new")
kNN_data <- kNN_data[, c(selected_vars, "region")]

# Normalize the predictor variables
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
kNN_data[, selected_vars] <- as.data.frame(lapply(kNN_data[, selected_vars], normalize))

# Convert region to factor for classification
kNN_data$region <- as.factor(kNN_data$region)

# Split into training and testing sets
set.seed(123)
train_index <- createDataPartition(kNN_data$region, p = 0.7, list = FALSE)
train_data <- kNN_data[train_index, ]
test_data <- kNN_data[-train_index, ]

# Test multiple values of k
k_values <- c(1, 5, 10, 20, 30, 50)
k_results <- data.frame(k = integer(), Accuracy = numeric())

for (k in k_values) {
  knn_pred <- knn(train = train_data[, selected_vars], test = test_data[, selected_vars], 
                  cl = train_data$region, k = k)
  
  # Evaluate the model
  conf_matrix <- confusionMatrix(as.factor(knn_pred), as.factor(test_data$region))
  accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
  
  # Store results
  k_results <- rbind(k_results, data.frame(k = k, Accuracy = accuracy))
}

# Print accuracy results for different k values
print(k_results)

#---------------------------------

library(ggplot2)
library(class)
library(caret)

# Load dataset
epi_data <- read.csv("C:/Users/leei6/Data-Analytics/Project/Environmnetal Indicator Modeling and Regional Classification/epi_results_2024_pop_gdp.csv")

# Select two regions for classification
selected_regions <- c("Global West", "Latin America & Caribbean")
kNN_data <- subset(epi_data, region %in% selected_regions)

# Choose three new predictor variables (excluding GDP and population)
selected_vars <- c("GTI.new", "LUF.new", "GTP.new")
kNN_data <- kNN_data[, c(selected_vars, "region")]

# Handle missing values by replacing with mean for each column
for (var in selected_vars) {
  kNN_data[[var]][is.na(kNN_data[[var]])] <- mean(kNN_data[[var]], na.rm = TRUE)
}

# Normalize the predictor variables
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
kNN_data[, selected_vars] <- as.data.frame(lapply(kNN_data[, selected_vars], normalize))

# Convert region to factor for classification
kNN_data$region <- as.factor(kNN_data$region)

# Split into training and testing sets
set.seed(123)
train_index <- createDataPartition(kNN_data$region, p = 0.7, list = FALSE)
train_data <- kNN_data[train_index, ]
test_data <- kNN_data[-train_index, ]

# Train kNN model with k = 20
k_value <- 20
knn_pred <- knn(train = train_data[, selected_vars], test = test_data[, selected_vars], 
                cl = train_data$region, k = k_value)

# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(knn_pred), as.factor(test_data$region))
accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)

# Print confusion matrix and statistics
print(paste("Confusion Matrix for k =", k_value))
print(conf_matrix)

# Print accuracy
print(paste("Accuracy for k =", k_value, "is", accuracy))


#-----------------------
library(ggplot2)
library(class)
library(caret)

# Load dataset
epi_data <- read.csv("C:/Users/leei6/Data-Analytics/Project/Environmnetal Indicator Modeling and Regional Classification/epi_results_2024_pop_gdp.csv")

# Select two regions for classification
selected_regions <- c("Global West", "Latin America & Caribbean")
kNN_data <- subset(epi_data, region %in% selected_regions)

# Choose three new predictor variables (excluding GDP and population)
selected_vars <- c("GTI.new", "LUF.new", "GTP.new")
kNN_data <- kNN_data[, c(selected_vars, "region")]

# Handle missing values by replacing with mean for each column
for (var in selected_vars) {
  kNN_data[[var]][is.na(kNN_data[[var]])] <- mean(kNN_data[[var]], na.rm = TRUE)
}

# Normalize the predictor variables
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
kNN_data[, selected_vars] <- as.data.frame(lapply(kNN_data[, selected_vars], normalize))

# Convert region to factor for classification
kNN_data$region <- as.factor(kNN_data$region)

# Split into training and testing sets
set.seed(123)
train_index <- createDataPartition(kNN_data$region, p = 0.7, list = FALSE)
train_data <- kNN_data[train_index, ]
test_data <- kNN_data[-train_index, ]

# Test multiple values of k
k_values <- c(1, 5, 10, 20, 30)
k_results <- data.frame(k = integer(), Accuracy = numeric())

for (k in k_values) {
  knn_pred <- knn(train = train_data[, selected_vars], test = test_data[, selected_vars], 
                  cl = train_data$region, k = k)
  
  # Evaluate the model
  conf_matrix <- confusionMatrix(as.factor(knn_pred), as.factor(test_data$region))
  accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
  
  # Print confusion matrix and statistics
  print(paste("Confusion Matrix for k =", k))
  print(conf_matrix)
  
  # Store results
  k_results <- rbind(k_results, data.frame(k = k, Accuracy = accuracy))
}

# Print accuracy results for different k values
print(k_results)
