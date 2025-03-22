library(ggplot2)
library(ggfortify)
library(dplyr)
library(class)
library(caret)
library(e1071)

# Load the wine dataset
column_names <- c("target", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash",
                  "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols",
                  "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline")

wine <- read.csv("C:/Users/leei6/Data-Analytics/lab4/wine.data", header = FALSE)
colnames(wine) <- column_names
wine$target <- as.factor(wine$target)

# Perform PCA
X <- wine[, -1]
Y <- wine$target
pca_result <- princomp(X, cor = TRUE, scores = TRUE)

# Plot PC1 and PC2
autoplot(pca_result, data = wine, colour = 'target',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("Wine Dataset PCA")

# Identify variables that contributed the most to PC1
abs_pc1 <- abs(pca_result$loadings[, 1])
abs_sorted <- sort(abs_pc1, decreasing = TRUE)
print(abs_sorted)

# Remove the two least contributing variables and rerun PCA
least_contribute <- names(tail(abs_sorted, 2))
X_dropped <- X[, !(names(X) %in% least_contribute)]

pca_dropped <- princomp(X_dropped, cor = TRUE, scores = TRUE)
autoplot(pca_dropped, data = wine, colour = 'target',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA After Dropping Least Contributing Variables")

# Train KNN classifier using original features
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
normalized_features <- as.data.frame(lapply(X, normalize))
normalized_features$target <- Y

set.seed(100)
train_idx <- createDataPartition(normalized_features$target, p = 0.7, list = FALSE)
train_original <- normalized_features[train_idx, ]
test_orignal <- normalized_features[-train_idx, ]

knn_pred_orig <- knn(train = train_original[, -ncol(train_original)],
                     test = test_orignal[, -ncol(test_orignal)],
                     cl = train_original$target, k = 65)

# Confusion matrix for original data
cm_original <- confusionMatrix(knn_pred_orig, test_orignal$target)
print(cm_original)

# Train KNN classifier using PC1, PC2, and PC3
pca_scores <- data.frame(pca_result$scores[, 1:3])
pca_scores$target <- Y
train_pca <- pca_scores[train_idx, ]
test_pca <- pca_scores[-train_idx, ]

knn_pred_pca <- knn(train = train_pca[, 1:3],test = test_pca[, 1:3],
                    cl = train_pca$target, k = 65)

# Confusion matrix
cm_pca <- confusionMatrix(knn_pred_pca, test_pca$target)
print(cm_pca)

# Compare the performance
get_metrics <- function(cm) {
  acc <- cm$overall["Accuracy"]
  precision <- cm$byClass[, "Precision"]
  recall <- cm$byClass[, "Recall"]
  f1 <- cm$byClass[, "F1"]
  return(data.frame(Accuracy = acc,Precision = mean(precision, na.rm = TRUE),
                    Recall = mean(recall, na.rm = TRUE), F1 = mean(f1, na.rm = TRUE)))
}

metrics_original <- get_metrics(cm_original)
metrics_pca <- get_metrics(cm_pca)
comparison <- rbind(Original = metrics_original,PCA_PC1_3 = metrics_pca)

# Final comparison
print(comparison)

