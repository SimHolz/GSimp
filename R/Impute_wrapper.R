#' @import missForest
#' @import impute
#' @import magrittr
#' @import imputeLCMD
require(missForest)
require(impute)
require(magrittr)
require(imputeLCMD)
source('R/MVI_global.R')
source('R/GSimp.R')

#' Random Forest Imputation Wrapper
#' 
#' @description Wrapper function for missForest imputation
#' @param data A numeric matrix or data frame with missing values
#' @param ... Additional arguments passed to missForest
#' @return Imputed dataset
#' @export
RF_wrapper <- function(data, ...) {
  result <- missForest(data, ...)[[1]]
  return (result)
}

#' k-Nearest Neighbors Imputation Wrapper
#' 
#' @description Wrapper function for k-NN imputation
#' @param data A numeric matrix or data frame with missing values
#' @param ... Additional arguments passed to impute.knn
#' @return Imputed dataset
#' @export
kNN_wrapper <- function(data, ...) {
  result <- data %>% data.matrix %>% impute.knn(., ...) %>% extract2(1)
  return(result)
}

#' Singular Value Decomposition Imputation Wrapper
#' 
#' @description Wrapper function for SVD-based imputation with scaling
#' @param data A numeric matrix or data frame with missing values
#' @param K Number of components for SVD imputation
#' @return Imputed dataset
#' @export
SVD_wrapper <- function(data, K = 5) {
  data_sc_res <- scale_recover(data, method = 'scale')
  data_sc <- data_sc_res[[1]]
  data_sc_param <- data_sc_res[[2]]
  result <- data_sc %>% impute.wrapper.SVD(., K = K) %>% 
    scale_recover(., method = 'recover', param_df = data_sc_param) %>% extract2(1)
  return(result)
}

#' Mean Imputation Wrapper
#' 
#' @description Imputes missing values with column means
#' @param data A numeric matrix or data frame with missing values
#' @return Imputed dataset with means replacing NA values
#' @export
Mean_wrapper <- function(data) {
  result <- data
  result[] <- lapply(result, function(x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    x
  })
  return(result)
}

#' Median Imputation Wrapper
#' 
#' @description Imputes missing values with column medians
#' @param data A numeric matrix or data frame with missing values
#' @return Imputed dataset with medians replacing NA values
#' @export
Median_wrapper <- function(data) {
  result <- data
  result[] <- lapply(result, function(x) {
    x[is.na(x)] <- median(x, na.rm = T)
    x
  })
  return(result)
}

#' Half-Minimum Imputation Wrapper
#' 
#' @description Imputes missing values with half of the column minimum
#' @param data A numeric matrix or data frame with missing values
#' @return Imputed dataset with half-minimum values replacing NA values
#' @export
HM_wrapper <- function(data) {
  result <- data
  result[] <- lapply(result, function(x) {
    x[is.na(x)] <- min(x, na.rm = T)/2
    x
  })
  return(result)
}

#' Zero Imputation Wrapper
#' 
#' @description Imputes missing values with zeros
#' @param data A numeric matrix or data frame with missing values
#' @return Imputed dataset with zeros replacing NA values
#' @export
Zero_wrapper <- function(data) {
  result <- data
  result[is.na(result)] <- 0
  return(result)
}

#' Quantile Regression Imputation of Left-Censored Data Wrapper
#' 
#' @description Wrapper function for QRILC imputation with log transformation
#' @param data A numeric matrix or data frame with missing values
#' @param ... Additional arguments passed to impute.QRILC
#' @return Imputed dataset
#' @export
QRILC_wrapper <- function(data, ...) {
  result <- data %>% log %>% impute.QRILC(., ...) %>% extract2(1) %>% exp
  return(result)
}

#' GSimp Pre-processing and Imputation Wrapper
#' 
#' @description Comprehensive wrapper for GSimp imputation including pre-processing steps:
#'              log transformation, QRILC initialization, scaling, imputation, and recovery
#' @param data A numeric matrix or data frame with missing values
#' @return Imputed dataset after complete processing pipeline
#' @details The function performs the following steps:
#'   1. Log transformation of input data
#'   2. QRILC initialization of missing values
#'   3. Centralization and scaling
#'   4. GSimp imputation
#'   5. Recovery of original scale
#'   6. Exponential transformation
#' @export
pre_processing_GS_wrapper <- function(data) {
  data_raw <- data
  ## log transformation ##
  data_raw_log <- data_raw %>% log()
  ## Initialization ##
  data_raw_log_qrilc <- impute.QRILC(data_raw_log) %>% extract2(1)
  ## Centralization and scaling ##
  data_raw_log_qrilc_sc <- scale_recover(data_raw_log_qrilc, method = 'scale')
  ## Data after centralization and scaling ##
  data_raw_log_qrilc_sc_df <- data_raw_log_qrilc_sc[[1]]
  ## Parameters for centralization and scaling ##
  ## For scaling recovery ##
  data_raw_log_qrilc_sc_df_param <- data_raw_log_qrilc_sc[[2]]
  ## NA position ##
  NA_pos <- which(is.na(data_raw), arr.ind = T)
  ## bala bala bala ##
  data_raw_log_sc <- data_raw_log_qrilc_sc_df
  data_raw_log_sc[NA_pos] <- NA
  ## GSimp imputation with initialized data and missing data ##
  result <- data_raw_log_sc %>% GS_impute(., iters_each=50, iters_all=10, 
                                          initial = data_raw_log_qrilc_sc_df,
                                          lo=-Inf, hi= 'min', n_cores=2,
                                          imp_model='glmnet_pred')
  data_imp_log_sc <- result$data_imp
  ## Data recovery ##
  data_imp <- data_imp_log_sc %>% 
    scale_recover(., method = 'recover', 
                  param_df = data_raw_log_qrilc_sc_df_param) %>% 
    extract2(1) %>% exp()
  return(data_imp)
}
