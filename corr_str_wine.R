library(tidymodels)
library(tidyr)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)

df <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
df$quality <- factor(as.character(df$quality - rep(2, length(df$quality))))

# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

nf <- 1:(ncol(train_data)-1)
nt <- 100

set.seed(1234)

get.stcor <- function(i){
  bio.rf <- randomForest(quality ~ ., type = 'classification', data = train_data, 
                         importance=TRUE, ntree = nt, mtry = f, replace = T, norm.votes = F)
  
  
  
  Qs <- as.data.frame(bio.rf$votes/bio.rf$oob.times) #proportion OOB votes cast at x for each class
  mr <- c() # margin for the random forest
  squared.mr <- c()
  sd_k <- c()
  
  #correction on the sign and quantities for the computation of the strength and correlation
  for(i in 1:nrow(train_data)){
    y <- train_data[i,'quality']
    mr[i] <- Qs[i, y] - max(Qs[i, !(names(Qs) %in% y)])
    squared.mr[i] <- (Qs[i, y] - max(Qs[i, !(names(Qs) %in% y)]))**2
    p1 <- Qs[i, y]
    p2 <- max(Qs[i, !(names(Qs) %in% y)])
    sd_k[i] <- sqrt(p1 + p2  + (p1 - p2)**2)
  }

  #strength
  strength <- mean(mr)
  
  #E[quadro]
  var.mr <- mean(squared.mr) - strength**2
  
  #sd_k <- c()
  #for(i in 1:nrow(bio.rf$votes)){
  #  p1 <- bio.rf$votes[i,'1']/bio.rf$oob.times
  #  p2 <- bio.rf$votes[i, '0']/bio.rf$oob.times
  #  sd_k[i] <- sqrt(p1 + p2  + (p1 - p2)**2)
  #}
  
  #correlation
  corr <- var.mr/(mean(sd_k)**2)
  return(c(strength, corr))
}


for(f in nf){
  result <- t(sapply(1:100, get.stcor))
  strength_by_nfeat[f] <- mean(result[,1])
  corr_by_nfeat[f] <- mean(result[,2])
}

makePlot<-function(){
  plot(nf, strength_by_nfeat, ylim = c(0,1), type="b", pch=19, col="red", xlab="x", ylab="y")
  lines(nf, corr_by_nfeat, pch=18, col="blue", type="b", lty=2)
}

makePlot()
axis(1, at=1:9)
legend('center', legend=c("Strength", "Correlation"),
       col=c("red", "blue"), lty=1:2, cex=0.7,
       text.font=4, bg='lightblue')

