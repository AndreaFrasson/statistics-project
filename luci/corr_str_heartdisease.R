library(MLDataR)
library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)
library(zeallot)

#open dataset
data("stroke_classification")
df <- stroke_classification[-c(1)]
df <- na.omit(df)

# Convert 1s to a positive label and 0s to a negative label
df$stroke <- factor(ifelse(df$stroke == 1, "0", "1"))


# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

nf <- 1:(ncol(train_data)-1)
nt <- 100

set.seed(1234)

get.stcor <- function(i){
  bio.rf <- randomForest(stroke ~ ., type = 'classification', data = train_data, 
                         importance=TRUE, ntree = nt, mtry = f, replace = T, norm.votes = F)
  
  
  
  Qs <- as.data.frame(bio.rf$votes/bio.rf$oob.times) #proportion OOB votes cast at x for each class
  mr <- c(Qs['0'] - Qs['1'])[[1]] # margin for the random forest
  
  
  #correction on the sign and quantities for the correlation
  for(i in 1:nrow(train_data)){
    if(train_data[i, 'stroke'] == '1'){
      mr[i] <- mr[i] * - 1
    }
  }
  
  #strength
  strength <- mean(mr)
  
  #E[quadro]
  squared.mr <- c((Qs['0'] - Qs['1'])**2)[[1]]
  var.mr <- mean(squared.mr) - strength**2
  
  sd_k <- c()
  for(i in 1:nrow(bio.rf$votes)){
    p1 <- bio.rf$votes[i,'1']/bio.rf$oob.times
    p2 <- bio.rf$votes[i, '0']/bio.rf$oob.times
    sd_k[i] <- sqrt(p1 + p2  + (p1 - p2)**2)
  }
  
  #correlation
  corr <- var.mr/(mean(sd_k)**2)
  return(c(strength, corr))
}


for(f in nf){
  result <- t(sapply(1:100, get.stcor))
  strength_by_nfeat[f] <- mean(result[,1])
  corr_by_nfeat[f] <- mean(result[,2])
}

