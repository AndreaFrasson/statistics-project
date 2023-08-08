library(MASS)
library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)

data('biopsy')

df <-biopsy[-c(1,7)]

#parameter
nt <- 100 # number of trees in each round
nf <- 3
L <- 3

lc <- data <- data.frame(matrix(NA,    # Create empty data frame
                                nrow = dim(df)[1],
                                ncol = L))

for(i in 1:L){

  theta_k <- sample(colnames(subset(df, select = -c(class))), L)
  coef <- c(runif(L, -1,1))
  
  lc[, i] <- df[theta_k] %>%
    apply(., 1, function(x) dot(x, coef, d = T))
  
  colnames(lc)[i] <- paste("LC",i, sep = '')
}

lc$class <- df$class

# Split the data into training and testing sets
data_split <- initial_split(lc, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


get.error <- function(i){
  bio.rf <- randomForest(class ~ ., type = 'classification', data = train_data, 
                         importance=TRUE, ntree = nt)
  y.hat <- predict(bio.rf, newdata = test_data[-(ncol(test_data))])
  
  cm <- table(test_data$class, y.hat)
  as.numeric((cm[2,1] + cm[1,2])/sum(cm))
}

result <- t(sapply(1:100, get.error))
mean(result)
