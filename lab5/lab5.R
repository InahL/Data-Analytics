library(e1071)
library(caret)

#--------Part 1---------

# Read Data
wine <- read.csv("C:/Users/leei6/Data-Analytics/lab5/wine.data", header = FALSE)

# Preprocess
colnames(wine)[1] <- "Type"
wine$Type <- as.factor(wine$Type)

# Data Partitioning
set.seed(123)
train.index <- createDataPartition(wine$Type, p = 0.75, list = FALSE)
train <- wine[train.index, ]
test <- wine[-train.index, ]

# SVM with linear
svm.linear.tuned <- tune.svm(Type ~ ., data = train, kernel = "linear",
                             cost = 2^(-1:5))
best.linear <- svm.linear.tuned$best.model

# SVM with radial
svm.radial.tuned <- tune.svm(Type ~ ., data = train, kernel = "radial",
                             cost = 2^(-1:5), gamma = 2^(-5:1))
best.radial <- svm.radial.tuned$best.model

# Naive Bayes classifier
nb.model <- naiveBayes(Type ~ ., data = train)

# Predictions
pred.linear <- predict(best.linear, test)
pred.radial <- predict(best.radial, test)
pred.nb <- predict(nb.model, test)

# Comparison
conf.linear <- confusionMatrix(pred.linear, test$Type)
conf.radial <- confusionMatrix(pred.radial, test$Type)
conf.nb <- confusionMatrix(pred.nb, test$Type)

# Print results
cat("\nSVM Linear\n")
print(conf.linear$byClass)

cat("\nSVM Radial\n")
print(conf.radial$byClass)

cat("\nNaive Bayes\n")
print(conf.nb$byClass)

#--------Part 2--------------
library(readr)
library(ggplot2)
library(e1071)
library(caret)

# Read Data
NY_House_Dataset <- read_csv("C:/Users/leei6/Data-Analytics/lab5/NY-House-Dataset.csv")
dataset <- NY_House_Dataset

# Split Train and Test Set
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))
train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

# SVM
svr.mod0 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset, kernel="radial")
summary(svr.mod0)
svr.pred <- predict(svr.mod0, dataset)

# Predicted vs Actual Plot
svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")
  ggtitle("SVM Regression Predicted vs Actual")

# Residual Plot
svr.outs$residual <- svr.outs$real - svr.outs$pred
ggplot(svr.outs, aes(x = residual)) +
  geom_histogram(bins = 30) +
  ggtitle("SVM Regression Residuals")


svr.mod0 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset)
summary(svr.mod0)
svr.pred <- predict(svr.mod0, dataset)
svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

# Predicted vs Actual Plot
ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm") +
  ggtitle("Linear Regression Predicted vs Actual")

# Redisual Plot
svr.outs$residual <- svr.outs$real - svr.outs$pred
ggplot(svr.outs, aes(x = residual)) +
  geom_histogram(bins = 30) +
  ggtitle("Linear Regression Residuals")

