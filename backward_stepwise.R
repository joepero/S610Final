## Note:
# Remove all rows containing missing data in your data set
# Remember to rename response and predictor variables with a simple name

# Separate functions
# X is a matrix of predictors with the first column is a vector of ones
# arguments must be specified names and values
make_matrix_X <- function(...) {
  names <- as.list(match.call())[-1] %>% names() # extract arguments' name
  X <- cbind(1,...)
  colnames(X) <- c("(Intercept)", names)
  return(X)
}  
get_beta <- function(X, y) solve(t(X)%*%X) %*% t(X) %*% y
get_yhat <- function(X, beta) X %*% beta
get_sigma <- function(yhat, y, n, p) sqrt(sum((y-yhat)^2)/(n-p))
get_se <- function(X, sigma) sqrt(diag(sigma^2*solve(t(X)%*%X)))
t_test <- function(beta, se, n, p) {
  t_stat <- beta/se
  p_value <- 2*pt(abs(t_stat), df = n-p, lower.tail = FALSE)
  result <- cbind(t_stat, p_value)
  colnames(result) <- c("t-stat", "p-value")
  return(result)
}

get_AIC <- function(y, yhat, n, p) {
  k <- 1 + p # including x-coefficient, the intercept, and sigma
  logLik_2 <- n*(log(2*pi)+1+log((sum((y-yhat)^2)/n))) # -2log(L)
  if (n/k >= 40) {
    AIC <- 2*k + logLik_2
  } else {
    AIC <- 2*k + logLik_2 + (2*k^2+2*k)/(n-k-1) # AICc
  }
  return(AIC)
}

# step function
# inputs of this function are: y (response vector), x_i,...,x_p respectively.
# Note that if you wanna check the result with lm function, you have to place x_i,...,x_p in the same order as a model in lm
# ... are predictor inputs
# ... must have names and values
# for better display, recommend to use this format step_func((named_response_vector), (desire_predictor_name_i)=(predictor_value_i))
step_func <- function(y, ...) {
  name_y <- deparse(substitute(y))
  X <- make_matrix_X(...)
  n <- nrow(X)
  p <- ncol(X)
  
  # coefficient estimate summary
  beta <- get_beta(X,y)
  yhat <- get_yhat(X,beta)
  sigma <- get_sigma(yhat, y, n, p)
  se <- get_se(X, sigma)
  t_stat <- t_test(beta, se, n, p)[,1]
  p_value <- t_test(beta, se, n, p)[,2]
  
  # calculate AIC of the current and AIC of "if-drop" models
  AIC <- get_AIC(y, yhat, n, p)
  list_AIC <- vector(mode = "list", length = p)
  for (i in 1:p) {
    list_AIC[[i]] <- X[,-i]
  }
  next_AIC <- vector("numeric", p)
  for (j in 1:p) {
    X_j <- list_AIC[[j]]
    beta_j <- get_beta(X_j, y)
    yhat_j <- get_yhat(X_j, beta_j)
    next_AIC[j] <- get_AIC(y, yhat_j, n, p-1)
  }

  
  # create a result table and display it
  result <- cbind(beta, se, t_stat, p_value, next_AIC)
  colnames(result) <- c("Est", "se", "t-stat", "p-value", "AIC")
  
  cat("Formula:", name_y, "=", paste(colnames(X)[-1], collapse=" + "), "\nAIC =", AIC, "\n")
  print(result)
}


# test function
test <- function () {
  
}

# main function
bws_selection <- function() {
  
}