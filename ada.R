library(MASS)
library(tidymodels)
library(tidyr)
library(rpart)
library(adabag)
library(foreach)
library(ggplot2)


data('biopsy')
summary(biopsy) 
df <- biopsy[-c(1, 7)]


# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

model_adaboost <- boosting(class~., data=train_data, boos=TRUE, mfinal=50)

#use model to make predictions on test data
predict(model_adaboost, test_data)$error


