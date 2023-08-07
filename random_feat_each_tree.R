library(MASS)
library(tidymodels)
library(tidyr)
library(rpart)

mode<-function(x){which.max(tabulate(x))}

grow <- function(dati, ntree){
  
  forest <- list()
  for(i in 1:ntree){
    
    #from training set, extract a new training with replacement
    new_training <- dati[sample(nrow(dati), replace = T), ]  
    
    # Create the formula using the selected features as predictors
    theta_k <- sample(colnames(subset(dati, select = -c(class))), 1)
    formula <- as.formula(paste("class ~", theta_k))
    
    #Decision tree with random features
    classifier <- rpart(formula,
                        data = dati)
    #save in the list
    forest[[i]] <- classifier
    
  }
  return(forest)
}
  
  
RF_predict <- function(new_data, forest) {
  # Initialize a matrix to store predictions from all trees
  predictions <- matrix(0, nrow = nrow(new_data), ncol = length(forest))
  
  # Iterate through each tree and get predictions
  for (i in 1:length(forest)) {
    predictions[, i] <- factor(predict(forest[[i]], new_data, type = 'class'))
  }
  
  # Take a majority vote (if classification problem)
  final_predictions <- apply(predictions, 1, mode)
  
  return(final_predictions)
}


data("biopsy")
df <- biopsy[-c(1, 7)]


# Convert gender_vector to a factor, then label
df$class <- ifelse(factor(df$class) == 'benign', '0', '1')

# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

forest <- grow(train_data, 20)

#CON LA FUNZIONE AL MOMENTO NON VA
# Make predictions on new data (replace "new_data" with your actual test data)
#predictions <- predict(test_data[-10], forest)

new_data <- test_data[-10]

# Initialize a matrix to store predictions from all trees
predictions <- matrix(0, nrow = nrow(new_data), ncol = length(forest))

# Iterate through each tree and get predictions
for (i in 1:length(forest)) {
  predictions[, i] <- factor(predict(forest[[i]], new_data, type = 'class'))
}

# Take a majority vote (if classification problem)
final_predictions <- apply(predictions, 1, mode)

predictions <- final_predictions

# Making the Confusion Matrix
cm <- table(test_data[, 9], predictions)

test_error <- as.numeric((cm[2,1] + cm[1,2])/sum(cm))
