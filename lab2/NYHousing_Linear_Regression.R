library("ggplot2")
library("readr")

# load dataset
NY_House_Dataset <- read_csv("C:/Users/leei6/Data-Analytics/lab2/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

# filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

# column names
names(dataset)

# fit three linear models
lmod1 <- lm(PRICE ~ PROPERTYSQFT, data = dataset) 
lmod2 <- lm(PRICE ~ BEDS + BATH, data = dataset) 
lmod3 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = dataset) 

# print model summaries
summary(lmod1)
summary(lmod2)
summary(lmod3)

# model 1
plot(dataset$PROPERTYSQFT, dataset$PRICE, main = "PRICE vs PROPERTYSQFT",
     xlab = "property Sq Ft", ylab = "price", pch = 16, col = "blue")
abline(lmod1, col = "red")

# residual plot for model 1
plot(lmod1$fitted.values, residuals(lmod1), main = "Residual plot",
     xlab = "fitted val", ylab = "residual", pch = 16, col = "blue")
abline(h=0, col="red")

# model 2
plot(dataset$BEDS + dataset$BATH, dataset$PRICE, main = "PRICE vs BEDS + BATH",
     xlab = "Beds + Baths", ylab = "Price", pch = 16, col = "blue")
abline(lmod2, col = "red")

# residual plot for model 2
plot(lmod2$fitted.values, residuals(lmod2), main = "Residual plot",
     xlab = "fitted val", ylab = "residual", pch = 16, col = "blue")
abline(h=0, col="red")

# model 3
plot(dataset$PROPERTYSQFT + dataset$BEDS + dataset$BATH, dataset$PRICE, main = "PRICE vs PROPERTYSQFT + BEDS + BATH",
     xlab = "PROPERTYSQFT + BEDS + BATH", ylab = "Price", pch = 16, col = "blue")
abline(lmod3, col = "red")

# residual plot for model 3
plot(lmod3$fitted.values, residuals(lmod3), main = "Residual plot",
     xlab = "fitted Val", ylab = "Residuals", pch = 16, col = "blue")
abline(h=0, col="red")

# compare the 3 models
cat("Model 1 R^2:", summary(lmod1)$r.squared, "\n")
cat("Model 2 R^2:", summary(lmod2)$r.squared, "\n")
cat("Model 3 R^2:", summary(lmod3)$r.squared, "\n")
