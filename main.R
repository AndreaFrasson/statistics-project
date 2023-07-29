library(MASS)
data("biopsy")

df <- biopsy[-1]
set.seed(123)

library(tidymodels)
library(tidyr)

# Split the data into training and testing sets

data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


library(rpart)

grow <- function(data, ntree){
  
  forest <- list()
  
  for(i in 1:ntree){
    
    #from training set, extract a new training with replacement
    new_training <- train_data[sample(nrow(data), replace = T), ]  
    
    # Create the formula using the selected features as predictors
    theta_k <- sample(colnames(new_training[,-10]), 3)
    formula <- as.formula(paste("class ~", paste(theta_k, collapse = "+")))
    
    classifier = rpart(formula,
                       data = new_training)
    
    decision_trees[[i]] <- tree_model
    
  }

}


predict <- function(new_data, forest) {
  # Initialize a matrix to store predictions from all trees
  predictions <- matrix(0, nrow = nrow(new_data), ncol = num_trees)
  
  # Iterate through each tree and get predictions
  for (i in 1:num_trees) {
    predictions[, i] <- predict(decision_trees[[i]], new_data)
  }
  
  # Take a majority vote (if classification problem) or average (if regression problem) for the final prediction
  final_predictions <- apply(predictions, 1, mean)  # Use mean for regression, mode for classification
  
  return(final_predictions)
}

# Make predictions on new data (replace "new_data" with your actual test data)
predictions <- get_random_forest_predictions(new_data)





# Predicting the Test set results
y_pred <- predict(classifier,
                  newdata = test_data[, -10],
                  type = 'class')

# Making the Confusion Matrix
cm <- table(test_data[, 10], y_pred)


# Plotting the tree
plot(classifier)
text(classifier)
