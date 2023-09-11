# data <- data.frame(ndsi = c(0.7,0.4,0.2,0.4,0.4,0.4,0.1,0.4))

ndsi_to_binary <- function(data) {
  
  result <- numeric(length(data$ndsi))
  
  for (i in seq(data$ndsi)) {
    
    if (data$ndsi[i] >= 0.4 & data$ndsi[i] <= 1) {
      result[i] <- 1
    } else {
      result[i] <- 0
    }
  }
  
  return(result)
}

# ndsi_to_binary(data)

