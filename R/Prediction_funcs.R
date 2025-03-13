# Packages ----------------------------------------------------------------
require(randomForest)
require(glmnet)
require(rpart)
require(FNN)

#' @title Prediction Functions for Various Models
#' @description A collection of wrapper functions for different prediction models
#' @importFrom randomForest randomForest
#' @importFrom glmnet glmnet
#' @importFrom rpart rpart
#' @importFrom FNN knn.reg

#' @title Linear Regression Prediction
#' @description Fits and predicts using a linear regression model
#' @param x data.frame containing predictor variables
#' @param y numeric vector of response values
#' @return numeric vector of predicted values
lm_pred <- function(x, y) {
  data <- data.frame(y=y, x)
  model <- lm(y ~ ., data=data)
  y_hat <- predict(model, newdata=data)
  return(y_hat)
}

#' @title Robust Linear Regression Prediction
#' @description Fits and predicts using a robust linear regression model
#' @param x data.frame containing predictor variables
#' @param y numeric vector of response values
#' @return numeric vector of predicted values
rlm_pred <- function(x, y) {
  data <- data.frame(y=y, x)
  model <- rlm(y ~ ., data=data)
  y_hat <- predict(model, newdata=data)
  return(y_hat)
}

#' @title Random Forest Prediction
#' @description Fits and predicts using a random forest model
#' @param x data.frame containing predictor variables
#' @param y numeric vector of response values
#' @param ntree number of trees to grow (default: 200)
#' @param ... additional parameters passed to randomForest
#' @return numeric vector of predicted values
rf_pred <- function(x, y, ntree=200, ...) {
  model <- randomForest(x=x, y=y, ntree=ntree, ...)
  y_hat <- predict(model, newdata=x)
  return(y_hat)
}

#' @title Elastic Net Prediction
#' @description Fits and predicts using an elastic net model
#' @param x data.frame containing predictor variables
#' @param y numeric vector of response values
#' @param alpha mixing parameter between ridge and lasso (default: 0.5)
#' @param lambda regularization parameter (default: 0.01)
#' @return numeric vector of predicted values
glmnet_pred <- function(x, y, alpha=.5, lambda=.01) {
  x_mat <- as.matrix(x)
  model <- glmnet(x=x_mat, y=y, alpha=alpha, lambda=lambda)
  y_hat <- predict(model, newx=x_mat)[, 1]
  return(y_hat)
}

#' @title Decision Tree Prediction
#' @description Fits and predicts using a decision tree model
#' @param x data.frame containing predictor variables
#' @param y numeric vector of response values
#' @return numeric vector of predicted values
rpart_pred <- function(x, y) {
  data <- data.frame(y=y, x)
  model <- rpart(y ~ ., data=data)
  y_hat <- predict(model, newdata=data)
  return(y_hat)
}

#' @title K-Nearest Neighbors Prediction
#' @description Fits and predicts using a k-nearest neighbors model (k=5)
#' @param x data.frame containing predictor variables
#' @param y numeric vector of response values
#' @return numeric vector of predicted values
knn_pred <- function(x, y) {
  model <- knn.reg(train=x, y=y, k=5)
  y_hat <- model$pred
  return(y_hat)
}
