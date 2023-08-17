library(MASS)
library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)

#open dataset
data("biopsy")
df <- biopsy[-c(1)]
df <- na.omit(df)


# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


nf <- log2(ncol(df) - 1) + 1
nt <- 50

bio.rf <- randomForest(class ~ ., type = 'classification', data = train_data, 
                        ntree = nt, mtry = nf, keep.forest = T, replace = T)

votes <- as.data.frame(bio.rf$votes / bio.rf$oob.times) #proportion OOB votes cast at x for each class
mr <- c(votes['benign'] - votes['malignant'])[[1]] # margin for the random forest

s1 <- 0
s2 <- 0

#correction on the sign and quantities for the correlation
for(i in 1:nrow(train_data)){
  if(train_data[i, 'class'] == 'malignant'){
    mr[i] <- mr[i] * - 1
    s1 <- s1 + votes[i, 'malignant']
    s2 <- s2 + votes[i, 'benign']
  }
  else {
    s1 <- s1 + votes[i, 'benign']
    s2 <- s2 + votes[i, 'malignant']
  }
}

#strength
strength <- mean(mr)
strength


#E[quadro]
squared.mr <- c((votes['benign'] - votes['malignant'])**2)[[1]]
var.mr <- mean(squared.mr) - strength**2


#proportion of right guesses
p1 <- s1/nrow(train_data)
#proportion of wrong guesses
p2 <- s2/nrow(train_data)

sd <- sqrt(p1 + p2 + (p1-p2)**2)

#correlation
corr <- var.mr/(sd)**2
corr
