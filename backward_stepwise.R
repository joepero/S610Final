## Note:
# Remove all rows containing missing data in your data set


# Separate functions
make_matrix_X <- function(...) cbind(1,...) # X is a matrix of predictors with the first column is a vector of ones
get_beta <- function(X, y) solve(t(X)%*%X) %*% t(X) %*% y
get_yhat <- function(X, beta) X %*% beta
get_sigma <- function(yhat, y) sqrt(sum((y-yhat)^2)/(nrow(X)-ncol(X)))
get_se <- function(X, sigma) sqrt(diag(sigma^2*solve(t(X)%*%X)))
t_test <- function(beta, se, X) {
  t_stat <- beta/se
  p_value <- 2*pt(t_stat, df = nrow(X)-ncol(X), lower.tail=FALSE)
  result <- cbind(t_stat, p_value)
  colnames(result) <- c("t-stat", "p-value")
  return(result)
}

get_AIC <- function(y, yhat, n, num_p) {
  k <- 1 + num_p # including x-coefficient, the intercept, and sigma
  logLik_2 <- n*(log(2*pi)+1+log((sum((y-yhat)^2)/n))) # -2log(L)
  if (n/k >= 40) {
    AIC <- 2*k + logLik_2
  } else {
    AIC <- 2*k + logLik_2 + (2*k^2+2*k)/(n-k-1)
  }
  return(AIC)
}

# test function
# inputs of this function are: y (response vector), x_i,...,x_p respectively.
# Note that if you wanna check the result with lm function, you have to place x_i,...,x_p in the same order as a model in lm
test_func <- function(y, ...) {
  X <- make_matrix_X(...)
  n <- nrow(X)
  p <- ncol(X)
  beta <- get_beta(X,y)
  yhat <- get_yhat(X,beta)
  AIC <- get_AIC(y, yhat, n, p)
  return(AIC)
}

# test function
test <- function () {
  
}

# main function
bws_selection <- function() {
  
}