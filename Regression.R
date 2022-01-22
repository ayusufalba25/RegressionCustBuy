# Import libraries
library(tidyverse)
library(readxl)
library(skimr)
library(lmtest)
library(psych)

# Import data
df <- read_xlsx("Dapur Putih Cafe.xlsx")
View(df)

# Skim the data
skim(df)

# Check if there is any clear of relationship pattern
pairs.panels(df, method = "spearman")

# Check if there is any outliers in the data using boxplot
par(mfrow = c(1, 3))
boxplot(df$consider_buying)
boxplot(df$price)
boxplot(df$product_quality)

# Split the data into train and test set
set.seed(123)
train_set = sample(1:nrow(df), round(0.8 * nrow(df), 0))
df_train <- df[train_set,]
df_test <- df[-train_set,]

# Fitting linear regression model
model <- lm(consider_buying ~ ., data = df_train)
summary(model)
plot(model)

# Fitting linear regression model but without product_quality
model <- lm(consider_buying ~ price, data = df_train)
summary(model)
plot(model)

# Normality test for residual
shapiro.test(model$residuals)

# Heteroscedasticity test
bptest(model)

# Predict test data
y_test_pred <- predict(model, df_test[-1])

# RMSE
rmse <- function(y_true, y_pred){
  mse <- mean((y_true - y_pred) ^ 2)
  return(sqrt(mse))
}
rmse(df_test$consider_buying, y_test_pred)

# MAPE
mape <- function(y_true, y_pred){
  return((mean(abs(y_true - y_pred) / y_true) * 100))
}
mape(df_test$consider_buying, y_test_pred)

# Check the difference
cbind(df_test$consider_buying, round(y_test_pred, 0))

# Clustering customers
cluster_customer <- kmeans(df, 2)
cluster_customer
