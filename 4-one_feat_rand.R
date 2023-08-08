library(MASS)
library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)

#open dataset
data("biopsy")
df <- biopsy[-c(1, 7)]


# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

#parameter
nt <- 100 # number of trees in each round


get.error <- function(i){
  
  feat <- sample(colnames(subset(train_data, select = -c(class))), 1)
  formula = as.formula(paste("class ~", feat))
  
  bio.rf <- randomForest(formula, type = 'classification', data = train_data, 
                         importance=TRUE, ntree = nt)
  y.hat <- predict(bio.rf, newdata = test_data[-10])
  
  cm <- table(test_data$class, y.hat)
  as.numeric((cm[2,1] + cm[1,2])/sum(cm))
  
}

result <- t(sapply(1:100, get.error))
mean(result)

ggplot() + geom_point(aes(x = 1:length(result), y = result))
