library(tidyverse)
options(scipen = 1) # display numbers

# Separate functions
# X is a matrix of predictors with the first column is a vector of ones
# subset is a dataframe containing all predictors under consideration
make_matrix_X <- function(subset) {
  X <- cbind(Intercept = rep(1, nrow(subset)), subset)
  names <- colnames(X)
  X <- matrix(unlist(X), nrow=nrow(subset)) # convert to matrix to perform calculation later
  colnames(X) <- names
  return(X)
}  
get_beta <- function(X, y) solve(t(X)%*%X, tol=1e-17) %*% t(X) %*% y
get_yhat <- function(X, beta) X %*% beta
get_sigma <- function(yhat, y, n, p) sqrt(sum((y-yhat)^2)/(n-p))
get_se <- function(X, sigma) sqrt(diag(sigma^2*solve(t(X)%*%X, tol=1e-17)))
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
# inputs of this function are: y (response vector), X (matrix of predictors)
step_func <- function(y,X) {
  n <- nrow(X) # number of observations
  p <- ncol(X) # number of parameters
  
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
  next_AIC[1] <- NA
  
  # create a result table and display it
  result <- cbind(beta, se, t_stat, p_value, next_AIC)
  colnames(result) <- c("Est", "se", "t-stat", "p-value", "AIC")
  table <- list(AIC = AIC, summary = result)
  return(table)
}

# main function
# Notes:
# Remove all missing values from your data
# Input data is a formula in format "response ~ predictor1 + predictor2 + ..." and a dataset
# The formula has to be a string
# Suggest to name columns of the data for better display
bws_selection <- function(formula, data) {
  # get response vector and matrix of predictors
  var <- strsplit(formula, "\\~|\\+")[[1]] # extract variable names
  var <- gsub(" ", "", var) # remove white spaces
  y <- data[,var[1]]
  X <- make_matrix_X(data[var[2:length(var)]])
  
  t <- 1 # step count
  checkCondition <- TRUE
  stepResult <- step_func(y,X) # step 1 result
  
  # loop through each step
  while (checkCondition) {
    # print out the formula and AIC of current model
    cat("Step", t, ":", "\nFormula:", var[1], "~", paste(colnames(X)[!colnames(X) %in% "Intercept"], collapse=" + "), "\nAIC =", stepResult$AIC, "\n")
    # summary statistic of the current model
    print(stepResult$summary)
    cat("\n")
    cat(strrep("-", 25))
    cat("\n")
    # get an index at which "if-drop" AIC is smallest
    minAIC <- which(stepResult$summary[-1,5] == min(stepResult$summary[-1,5]))
    # compare AIC
    if (stepResult$summary[minAIC+1,5] < stepResult$AIC) {
      # remove the variable whose "if-drop" AIC is smallest and smaller than the current AIC
      X <- X[,-(minAIC+1)]
      # obtain the new summary statistic
      stepResult <- step_func(y,X)
      t <- t + 1 # next step count
    } else {
      # if all "if-drop" AIC is larger than the current AIC, stop looping
      checkCondition <- FALSE
    }
  }
  # print out the final model containing coefficient estimates
  cat("Final model:", "\nFormula:", var[1], "~", paste(colnames(X)[!colnames(X) %in% "Intercept"], collapse=" + "), "\n")
  print(stepResult$summary[,1])
}

