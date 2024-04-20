#' @title Calculate Weights from a Judgment Matrix
#' 
#' @description This function takes a judgment matrix derived from pairwise comparisons of factors and outputs normalized weights for all factors, ensuring the sum of all weights equals 1. The judgment matrix should have factors compared in rows and columns, with each cell representing the relative importance of a factor over another.
#' 
#' @param data A `data.frame` representing a judgment matrix from pairwise factor comparisons.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item{decision_matrix}{The decision matrix normalized by column sums.}
#'   \item{weight_factor}{Normalized weights of all factors, summing to 1.}
#' }
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' a <- c(1, 3/4, 1/2)
#' b <- c(4/3, 1, 1/3)
#' c <- c(2, 3/2, 1)
#' data <- data.frame(a, b, c)
#' weights <- weight_fun(data)
#' print(weights)
#' }
weight_fun <- function(data) {
  # Convert data frame to matrix and ensure row names are set for clarity
  data_matrix <- as.matrix(data)
  rownames(data_matrix) <- colnames(data_matrix)
  
  # Sum each column to normalize decision matrix
  sum_vector_col = apply(data_matrix, 2, sum)
  
  # Normalize each row by the sum of its respective column
  decision_matrix = apply(data_matrix, 1, function(x) x / sum_vector_col)
  
  # Calculate factor weights by summing rows of the normalized decision matrix
  weight_vector = colSums(decision_matrix)
  
  # Normalize weights to ensure their sum is 1
  weight_factor = weight_vector / sum(weight_vector)
  
  #Weights to three decimal places
  weight_factor = round(weight_factor,3)
  
  # Return a list containing the decision matrix and normalized weights
  result = list(decision_matrix = t(decision_matrix), weight_factor = weight_factor)
  return(result)
}




