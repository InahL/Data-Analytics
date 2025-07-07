library(readr)
library(ggplot2)
library(dplyr)

# Load dataset
df <- read_csv("C:/Users/leei6/Data-Analytics/Project/NYC Real Estate Analysis and Neighborhood Classification/NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

#------------------1b---------------------------

# Brooklyn data
brooklyn <- df %>% filter(BOROUGH == 3)

# Filtering
brooklyn <- brooklyn %>%
  mutate(SALE_PRICE = as.numeric(`SALE PRICE`)) %>%
  filter(!is.na(SALE_PRICE), SALE_PRICE > 0)

# Distribution histogram
ggplot(brooklyn, aes(x = SALE_PRICE)) +
  geom_histogram(bins = 100, fill = "blue", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Sale Prices in Brooklyn",
       x = "Sale Price", y = "Count")

# IQR for Outlier
q1 <- quantile(brooklyn$SALE_PRICE, 0.25)
q3 <- quantile(brooklyn$SALE_PRICE, 0.75)
iqr <- q3 - q1
upper_bound <- q3 + 1.5 * iqr

# Boxplot
ggplot(brooklyn, aes(x = SALE_PRICE)) + geom_boxplot(fill = "red", outlier.color = "red") +
  scale_x_continuous(limits = c(0, upper_bound * 2)) +
  labs(title = "Boxplot of Sale Prices in Brooklyn", x = "Sale Price")

# Get outlier data
outliers <- brooklyn %>% filter(SALE_PRICE > upper_bound)

#----------------------1c---------------------------
# Filtering
brooklyn <- df %>%
  filter(BOROUGH == 3) %>%
  mutate(SALE_PRICE = as.numeric(`SALE PRICE`), GROSS_SQUARE_FEET = as.numeric(`GROSS SQUARE FEET`), YEAR_BUILT = as.numeric(`YEAR BUILT`)) %>%
  filter(SALE_PRICE > 1000, !is.na(GROSS_SQUARE_FEET), GROSS_SQUARE_FEET > 0)

# Log transformation
brooklyn <- brooklyn %>%
  mutate(log_price = log(SALE_PRICE),
         log_area = log(GROSS_SQUARE_FEET))

# Regression model
model <- lm(log_price ~ log_area + YEAR_BUILT, data = brooklyn)
summary(model)

# Test BEDFORD STUYVESANT
test_sample <- brooklyn %>% filter(NEIGHBORHOOD == "BEDFORD STUYVESANT")
test_sample$predicted_log_price <- predict(model, newdata = test_sample)
test_sample$residuals <- test_sample$log_price - test_sample$predicted_log_price

# Predicted and Actural Graph
ggplot(test_sample, aes(x = predicted_log_price, y = log_price)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual Sale Prices (Bedford Stuyvesant)",
       x = "Predicted Price", y = "Actual Price")



#------------------1d---------------------------
library(caret)
library(e1071)
library(class) 
library(randomForest)

# Data preprocessing
brooklyn <- df %>%
  filter(BOROUGH == 3) %>%
  mutate(SALE_PRICE = as.numeric(`SALE PRICE`), GROSS_SQUARE_FEET = as.numeric(`GROSS SQUARE FEET`), NEIGHBORHOOD = as.factor(NEIGHBORHOOD)) %>%
  filter( SALE_PRICE > 1000, GROSS_SQUARE_FEET > 100,
    !is.na(SALE_PRICE), !is.na(GROSS_SQUARE_FEET), !is.na(NEIGHBORHOOD)
  )

# Log transformation
brooklyn$log_price <- log(brooklyn$SALE_PRICE)
brooklyn$log_area <- log(brooklyn$GROSS_SQUARE_FEET)

# Select variables
data <- brooklyn %>% select(log_price, log_area, NEIGHBORHOOD)

# Spliting dataset
set.seed(42)
train_idx <- createDataPartition(data$NEIGHBORHOOD, p = 0.7, list = FALSE)
train <- data[train_idx, ]
test <- data[-train_idx, ]

# Naive Bayes
nb_model <- naiveBayes(NEIGHBORHOOD ~ ., data = train)
nb_pred <- predict(nb_model, test)
nb_cm <- confusionMatrix(nb_pred, test$NEIGHBORHOOD)

# KNN
knn_pred <- knn(train[, 1:2], test[, 1:2], train$NEIGHBORHOOD, k = 5)
knn_cm <- confusionMatrix(knn_pred, test$NEIGHBORHOOD)

train$NEIGHBORHOOD <- droplevels(train$NEIGHBORHOOD)
test$NEIGHBORHOOD <- droplevels(test$NEIGHBORHOOD)

# 8. Random Forest
rf_model <- randomForest(NEIGHBORHOOD ~ ., data = train, ntree = 100)
rf_pred <- predict(rf_model, test)
rf_pred <- factor(rf_pred, levels = levels(test$NEIGHBORHOOD))
rf_cm <- confusionMatrix(rf_pred, test$NEIGHBORHOOD)

# Print results
nb_cm
knn_cm
rf_cm

data.frame( Model = c("Naive Bayes", "k-NN", "Random Forest"),
  Accuracy = c(nb_cm$overall["Accuracy"], knn_cm$overall["Accuracy"], rf_cm$overall["Accuracy"]))


#------------------2a---------------------------
# Queens data
queens <- df %>%
  filter(BOROUGH == 4) %>%
  mutate(SALE_PRICE = as.numeric(`SALE PRICE`), GROSS_SQUARE_FEET = as.numeric(`GROSS SQUARE FEET`),
         YEAR_BUILT = as.numeric(`YEAR BUILT`)) %>%
  filter(SALE_PRICE > 1000, GROSS_SQUARE_FEET > 100, !is.na(YEAR_BUILT)) %>%
  mutate(log_price = log(SALE_PRICE),log_area = log(GROSS_SQUARE_FEET))

# Predict
queens$predicted_log_price <- predict(model, newdata = queens)
queens$residuals <- queens$log_price - queens$predicted_log_price

# Predicted vs Actual Sale Price
ggplot(queens, aes(x = predicted_log_price, y = log_price)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual Sale Prices (Queens)",
       x = "Predicted Price", y = "Actual Price")

# Residual plot
ggplot(queens, aes(x = predicted_log_price, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot (Queens)",
       x = "Predicted Price", y = "Residual")


#------------------2b---------------------------
# Model from 1d
brooklyn <- df %>%
  filter(BOROUGH == 3) %>%
  mutate(SALE_PRICE = as.numeric(`SALE PRICE`), GROSS_SQUARE_FEET = as.numeric(`GROSS SQUARE FEET`), NEIGHBORHOOD = as.factor(NEIGHBORHOOD)) %>%
  filter(SALE_PRICE > 1000, GROSS_SQUARE_FEET > 100,
    !is.na(SALE_PRICE), !is.na(GROSS_SQUARE_FEET), !is.na(NEIGHBORHOOD)
  )

brooklyn$log_price <- log(brooklyn$SALE_PRICE)
brooklyn$log_area <- log(brooklyn$GROSS_SQUARE_FEET)
data <- brooklyn %>% select(log_price, log_area, NEIGHBORHOOD)

# Split the training and testing set
set.seed(42)
train_idx <- createDataPartition(data$NEIGHBORHOOD, p = 0.7, list = FALSE)
train <- data[train_idx, ]
test <- data[-train_idx, ]

train$NEIGHBORHOOD <- droplevels(train$NEIGHBORHOOD)
test$NEIGHBORHOOD <- droplevels(test$NEIGHBORHOOD)

# Train classification model
nb_model <- naiveBayes(NEIGHBORHOOD ~ ., data = train)
knn_pred <- knn(train[, 1:2], test[, 1:2], train$NEIGHBORHOOD, k = 5)
rf_model <- randomForest(NEIGHBORHOOD ~ ., data = train, ntree = 100)

# Preprocessing Queens data
queens <- df %>%
  filter(BOROUGH == 4) %>%
  mutate(SALE_PRICE = as.numeric(`SALE PRICE`), GROSS_SQUARE_FEET = as.numeric(`GROSS SQUARE FEET`),NEIGHBORHOOD = as.factor(NEIGHBORHOOD)) %>%
  filter(SALE_PRICE > 1000,GROSS_SQUARE_FEET > 100,
    !is.na(SALE_PRICE), !is.na(GROSS_SQUARE_FEET), !is.na(NEIGHBORHOOD)
  )

queens$log_price <- log(queens$SALE_PRICE)
queens$log_area <- log(queens$GROSS_SQUARE_FEET)
queens_data <- queens %>% select(log_price, log_area, NEIGHBORHOOD)

# Set the level
queens_data$NEIGHBORHOOD <- factor(queens_data$NEIGHBORHOOD, levels = levels(train$NEIGHBORHOOD))

# Predict
nb_pred_q <- predict(nb_model, newdata = queens_data)
knn_pred_q <- knn(train[, 1:2], queens_data[, 1:2], train$NEIGHBORHOOD, k = 5)
rf_pred_q <- predict(rf_model, newdata = queens_data)

nb_pred_q <- factor(nb_pred_q, levels = levels(train$NEIGHBORHOOD))
knn_pred_q <- factor(knn_pred_q, levels = levels(train$NEIGHBORHOOD))
rf_pred_q <- factor(rf_pred_q, levels = levels(train$NEIGHBORHOOD))

# Confusion matrix
nb_cm_q <- confusionMatrix(nb_pred_q, queens_data$NEIGHBORHOOD)
knn_cm_q <- confusionMatrix(knn_pred_q, queens_data$NEIGHBORHOOD)
rf_cm_q <- confusionMatrix(rf_pred_q, queens_data$NEIGHBORHOOD)

# Print accuracy
data.frame( Model = c("Naive Bayes", "k-NN", "Random Forest"),
  Accuracy = c(nb_cm_q$overall["Accuracy"], knn_cm_q$overall["Accuracy"], rf_cm_q$overall["Accuracy"]))