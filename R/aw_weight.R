#' @title Calculate Feature Vector Lambda from a Judgment Matrix
#' 
#' @details This function processes a judgment matrix obtained from pairwise factor comparisons to compute the feature vector lambda, which is an indicator of the consistency of the matrix. The function returns the feature vector lambda, detailed calculations of the matrix product with weights, and the division of these products by their corresponding weights.
#' 
#' @param data A `data.frame` after a two-by-two factor comparison.
#' @return A list containing the feature vector lambda, the product of the matrix and weight factors, 
#'         and the normalized values of these products.
#' @export
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' a <- c(1, 3/4, 1/2)
#' b <- c(4/3, 1, 1/3)
#' c <- c(2, 3/2, 1)
#' data <- data.frame(a, b, c)
#' lambda_result <- aw_weight(data)
#' print(lambda_result)
#' }
#' Calculate Feature Vector Lambda from a Judgment Matrix
#'
#' This function processes a judgment matrix obtained from pairwise factor comparisons
#' to compute the feature vector lambda, which is an indicator of the consistency of the matrix.
#' The function returns the feature vector lambda, detailed calculations of the matrix product 
#' with weights, and the division of these products by their corresponding weights.
#'
#' @param data A `data.frame` after a two-by-two factor comparison.
#' @return A list containing the feature vector lambda, the product of the matrix and weight factors, 
#'         and the normalized values of these products.
#' @export
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' a <- c(1, 3/4, 1/2)
#' b <- c(4/3, 1, 1/3)
#' c <- c(2, 3/2, 1)
#' data <- data.frame(a, b, c)
#' lambda_result <- aw_weight(data)
#' print(lambda_result)
#' }
aw_weight <- function(data) {
  data_matrix <- as.matrix(data)
  rownames(data_matrix) <- colnames(data)
  
  # Assuming weight_fun is a predefined function in the package or environment
  weight_factors <- weight_fun(data_matrix)$weight_factor
  AW_Vector <- data_matrix %*% weight_factors
  
  # Normalize and calculate lambda
  Sum_AW_W <- AW_Vector / weight_factors
  lambda <- sum(Sum_AW_W) / length(AW_Vector)
  lambda <- round(lambda, 3)  # Round lambda to three decimal places for precision
  
  # Compile results into a list
  result = list(
    AW_Vector = AW_Vector,
    Sum_AW_W = Sum_AW_W,
    lambda = lambda
  )
  
  return(result)
}

utils::globalVariables(c("weight_fun"))



