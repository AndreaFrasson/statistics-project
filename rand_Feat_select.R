library(MASS)
library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)

#open dataset
data("biopsy")
df <- biopsy[-c(1, 7)]


# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

#parameter
nt <- 100 # number of trees in each round
nf <- c(1:9) # number of features extracted in each tree


errors_feat <- c()

for(features in nf){
  test_error <- c()
  for(i in 1:100){
    bio.rf <- randomForest(class ~ ., type = 'classification', data = train_data, 
                           importance=TRUE, ntree = nt, mtry = features)
    y.hat <- predict(bio.rf, newdata = test_data[-10])
    
    cm <- table(test_data$class, y.hat)
    test_error[i] <- as.numeric((cm[2,1] + cm[1,2])/sum(cm))
  }
  errors_feat[features] <- mean(test_error)
}

errors_feat

library(ggplot2)

ggplot() + geom_point(aes(x = nf, y = errors_feat))

