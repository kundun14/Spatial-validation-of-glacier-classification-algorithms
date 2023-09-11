

library(sf)

predictions <- c(1, 0, 1, 1, 1, 0, 1, 0, 1, 0)
test <- c(1, 1, 1, 1, 0, 1, 0, 1, 0, 1)

x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(10, 20, 30, 40, 50, 60, 70, 80)


calculate_accuracy <- function(predictions, test) {

  confusion <- table(predictions, test)

  true_positives <- confusion[2, 2]
  true_negatives <- confusion[1, 1]
  false_positives <- confusion[2, 1]
  false_negatives <- confusion[1, 2]
  

  accuracy <- (true_positives + true_negatives) / sum(confusion)
  
  result <- list(confusion = confusion, accuracy = accuracy)
  return(result)
}

result <- calculate_accuracy(predictions, test)

#################
#################
#################

predictions <- c(1, 0, 1, 1, 1, 0, 1, 0, 1, 0)
test <- c(1, 1, 1, 1, 0, 1, 0, 1, 0, 1)


calculate_moran_i <- function(predicted_classes, test_classes, coordinates) {
  
  "coordinates is a sp object?
  
  "
  # error_indicator <- ifelse(predicted_classes == test_classes, 1, 0) # 1 si el modelo clasifico mal (error), 0 si clasifico correctamente la clase 
  
  sp_points <- SpatialPoints(coordinates)
  sp_points$predicted_classes <- predicted_classes
  sp_points$test_classes <- test_classes
  
  nn5 = knn2nb(knearneigh(sp_points,5)) # funciona con sp objects 
  w = nb2listw(nn5, style="B")
  
  spatial_predictions <- as.integer(w %*% as.integer(factor(predicted_classes))) >= 0
  spatial_accuracy <- sum(spatial_predictions == test_classes) / length(test_classes)
  
  
  
  
  # moran_result <- moran.test(sp_points$error_indicator, listw = w)
  # moran_result <- moran.mc(sp_points$error_indicator, listw = w, nsim =99)
  
  # Moran_I <- moran_result$estimate[1][[1]]
  # Moran_I <-moran_result$statistic[[1]]
  # p_value <- moran_result$p.value
  
  return()
}

"To modify the original accuracy equation to account for the similarity between predicted classes and nearby test classes, you can introduce a spatial weighting factor that adjusts the contribution of nearby test classes based on their similarity to the predicted classes. Here are a few ideas on how you can incorporate this into the accuracy calculation:

Weighted Accuracy:

Calculate the similarity between predicted classes and nearby test classes. This can be done using a similarity measure such as the Jaccard similarity or the cosine similarity.
Multiply the similarity measure by the original accuracy for each point to obtain a weighted accuracy value.
Sum the weighted accuracies for all points and divide by the total number of points to get the overall weighted accuracy.
Threshold-based Accuracy:

Define a threshold for the similarity measure. For example, you can consider nearby test classes to be similar if the Jaccard similarity is above a certain threshold.
Calculate the proportion of nearby test classes that meet the similarity threshold for each point.
Multiply the original accuracy by the proportion of similar nearby test classes for each point to obtain a weighted accuracy value.
Sum the weighted accuracies for all points and divide by the total number of points to get the overall weighted accuracy.
Neighborhood Consensus Accuracy:

Calculate the majority class among the nearby test classes for each point.
Compare the majority class with the predicted class for each point and determine if they match.
Calculate the proportion of matching majority classes and use it as a measure of spatial accuracy.
These are just a few ideas to get you started. The choice of approach depends on the specific context and requirements of your problem. You can experiment with different weighting schemes and similarity measures to find the most appropriate approach for your situation."
