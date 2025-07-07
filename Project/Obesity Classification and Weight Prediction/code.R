library(readr)
library(dplyr)
library(ggplot2)
library(DescTools)
library(ggcorrplot)
library(nnet)
library(caret)
library(dplyr)


# Data
df <- read_csv("C:/Users/leei6/Data-Analytics/Assignment/assignment6/ObesityDataSet_raw_and_data_sinthetic.csv")
str(df)
summary(df)
colSums(is.na(df))


# Check for outlier
par(mfrow=c(1,3), mar=c(4,4,2,1))

boxplot(df$Weight, main="Weight", col="blue")
boxplot(df$FAF, main="Physical Activity (FAF)", col="green")
boxplot(df$TUE, main="Leisure Time (TUE)", col="purple")

par(mfrow=c(1,1))

# Histograms
numeric_vars <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")

par(mfrow=c(3,3))
for (v in numeric_vars) {
  hist(df[[v]], main=paste("Histogram of", v), col="red", xlab=v)
}
par(mfrow=c(1,1))

# Categorical variable
ggplot(df, aes(x = Gender)) +
  geom_bar(fill = "red") +
  ggtitle("Gender Distribution")

ggplot(df, aes(x = NObeyesdad)) +
  geom_bar(fill = "red") +
  coord_flip() +
  ggtitle("Obesity Distribution")

# Visualize correlation matrix
numeric_data <- df %>% select(where(is.numeric))
corr_matrix <- cor(numeric_data)

ggcorrplot(corr_matrix, lab = TRUE, type = "lower", 
           title = "Correlation Matrix")



# Convert target variable to factor
df$NObeyesdad <- as.factor(df$NObeyesdad)

# Select variables
data <- df %>% select(Age, Height, FCVC, NCP, CH2O, FAF, TUE, Weight, NObeyesdad)

# Split dataset
set.seed(123)
train_index <- createDataPartition(data$NObeyesdad, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Logistic Regression
logit_model <- multinom(NObeyesdad ~ Age + Height + FCVC + NCP + CH2O + FAF + TUE, data = train_data)

# Prediction
logit_pred <- predict(logit_model, newdata = test_data)

# Evaluation
confusionMatrix(logit_pred, test_data$NObeyesdad)


# Linear Regression
lm_model <- lm(Weight ~ Age + Height + FCVC + NCP + CH2O + FAF + TUE, data = train_data)
summary(lm_model)

# Prediction
lm_pred <- predict(lm_model, newdata = test_data)

# Calculate for RMSE and R^2
rmse <- sqrt(mean((lm_pred - test_data$Weight)^2))
r_squared <- summary(lm_model)$r.squared

cat("RMSE:", rmse, "\nR-squared:", r_squared, "\n")
