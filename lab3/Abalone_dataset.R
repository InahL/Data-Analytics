###################
##### Abalone #####
###################

# read dataset
abalone <- read.csv("C:/Users/leei6/Data-Analytics/lab3/abalone_dataset.csv")
dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

knn.predicted1 <- knn(train = dataset[,3:5], test = dataset[,3:5], cl = dataset$age.group, k = 65)
knn.predicted2 <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k = 65)

# create contingency table and confusion matrix 
contingency1.table <- table(knn.predicted1, dataset$age.group, dnn=list('predicted','actual'))
contingency2.table <- table(knn.predicted2, dataset$age.group, dnn=list('predicted','actual'))

contingency1.table
contingency2.table

# calculate accuracies
sum(diag(contingency1.table))/length(dataset$age.group)
sum(diag(contingency2.table))/length(dataset$age.group)

# train knn models

k.list <- c(59,61,63,65,67,69,71)

# empty list for accuracy
accuracy.list <- c()

# train and predict a model for each k, and compute accuracy
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k = k)
  
  contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$age.group)
  
  accuracy.list <- c(accuracy.list,accuracy)
  
}

# plot the acccuracy with k
plot(k.list,accuracy.list,type = "b")

#--------------------------------#

# run tests with multiple k values and plot WCSS
k.list <- c(2,3,4,5)
wcss.list <- c()

for (k in k.list) {
  abalone.km <- kmeans(dataset[,2:4], centers = k)
  wcss <- abalone.km$tot.withinss
  wcss.list <- c(wcss.list,wcss)
  
  # get and plot clustering output 
  assigned.clusters <- as.factor(abalone.km$cluster)
}
plot(k.list,wcss.list,type = "b")
