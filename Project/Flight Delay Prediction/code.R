library(readr)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(tidyr)
library(DescTools)


# Data
flights <- read_csv("C:/Users/leei6/Data-Analytics/Assignment/assginment4/flights_sample_3m.csv")

# Preprocessing
flights <- flights %>%
  filter(CANCELLED == 0, DIVERTED == 0) %>%
  mutate(
    is_delayed = ifelse(ARR_DELAY > 15, 1, 0),
    Airline = as.factor(AIRLINE)
  ) %>%
  drop_na(ARR_DELAY, CRS_DEP_TIME, DISTANCE)


# EDA 1 by airline
ggplot(flights, aes(x = Airline, fill = as.factor(is_delayed))) +
  geom_bar(position = "fill") +
  labs(title = "Airline vs Delay Rate", y = "Proportion", fill = "Delayed")

# Calculate average delay rate
top_airlines <- flights %>%
  group_by(AIRLINE) %>%
  summarise(delay_rate = mean(is_delayed)) %>%
  arrange(desc(delay_rate)) %>%
  slice(1:5) %>%
  pull(AIRLINE)

# Visulaize top 5
flights %>%
  filter(AIRLINE %in% top_airlines) %>%
  ggplot(aes(x = Airline, fill = as.factor(is_delayed))) +
  geom_bar(position = "fill") +
  labs(title = "Top 5 Airlines by Delay Rate", y = "Proportion(0-1 scale)", fill = "Delayed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# EDA 2 by departure time
flights <- flights %>%
  mutate(dep_hour = floor(CRS_DEP_TIME / 100))

ggplot(flights, aes(x = dep_hour, fill = as.factor(is_delayed))) +
  geom_bar(position = "fill") +
  labs(title = "Departure Hour vs Delay Rate", x = "Scheduled Hour(hr)", y = "Proportion(0-1 scale)", fill = "Delayed")


# Correlations
cor_data <- flights %>%
  select(CRS_DEP_TIME, DISTANCE, ARR_DELAY)

cor_matrix <- Cor(cor_data, method = "pearson", use = "complete.obs")
print(cor_matrix)


# Undersampling
delayed <- flights %>% filter(is_delayed == 1)
not_delayed <- flights %>% filter(is_delayed == 0) %>% sample_n(nrow(delayed))
balanced_flights <- bind_rows(delayed, not_delayed)

# Data
model_data <- balanced_flights %>%
  select(is_delayed, AIRLINE, CRS_DEP_TIME, DISTANCE) %>%
  mutate(
    CRS_DEP_TIME = as.numeric(CRS_DEP_TIME),
    is_delayed = as.factor(is_delayed)
  )


# Train and test set
set.seed(123)
train_index <- createDataPartition(model_data$is_delayed, p = 0.8, list = FALSE)
train <- model_data[train_index, ]
test <- model_data[-train_index, ]


# Model 1 RandomForest
rf_model <- randomForest(is_delayed ~ ., data = train, ntree = 100)

# Prediction and Evaluation
rf_pred <- predict(rf_model, newdata = test)
rf_result <- confusionMatrix(rf_pred, test$is_delayed)
print(rf_result)

# Visualize variable importance
varImpPlot(rf_model)


# Model 2 logistic Regression
glm_model <- glm(is_delayed ~ ., data = train, family = "binomial")

# Prediction and Evaluation
glm_probs <- predict(glm_model, newdata = test, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
glm_pred <- as.factor(glm_pred)
glm_result <- confusionMatrix(glm_pred, test$is_delayed)
print(glm_result)

