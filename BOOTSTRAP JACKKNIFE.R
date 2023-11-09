set.seed(123)
mean_x <- 0
mean_y <- 0
var_x <- 1
var_y <- 1
correlation <- 0.3
# Generate bivariate normal data
n <- 10
data <- MASS::mvrnorm(n, mu = c(mean_x, mean_y), Sigma = matrix(c(var_x, correlation, correlation, var_y), ncol = 2))
# Function to calculate the sample statistic of interest
calculate_statistic <- function(data) {
  return(var(data[, 1]))
}
# Bootstrap function
bootstrap <- function(data, B) {
  statistics <- numeric(B)
  for (i in 1:B) {
    
    resampled_data <- data[sample(1:n, replace = TRUE), ]
    
    statistics[i] <- calculate_statistic(resampled_data)
  }
  return(statistics)
}
# Number of bootstrap samples
B <- 1000
bootstrap_results <- bootstrap(data, B)
original_statistic <- calculate_statistic(data)
bias <- mean(bootstrap_results) - original_statistic
variance <- var(bootstrap_results)
median_estimate <- median(bootstrap_results)
#######################
# Function to calculate the sample statistic of interest
calculate_statistic <- function(data) {
  return(var(data[, 1]))
}
# Jackknife function
jackknife <- function(data) {
  n <- nrow(data)
  statistics <- numeric(n)
  for (i in 1:n) {
        subset_data <- data[-i, ]
    statistics[i] <- calculate_statistic(subset_data)
  }
  return(statistics)
}
jackknife_results <- jackknife(data)
original_statistic <- calculate_statistic(data)
bias_jackknife <- (n - 1) * (mean(jackknife_results) - original_statistic)
variance_jackknife <- ((n - 1) / n) * sum((jackknife_results - mean(jackknife_results))^2)


