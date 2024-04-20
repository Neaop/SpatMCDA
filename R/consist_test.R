#' @title Perform Consistency Test on Judgment Matrix
#' 
#' @description This function takes the lambda value obtained from a judgment matrix and the number of factors to evaluate the consistency of the matrix. The consistency ratio (CR) is calculated and used to determine if the matrix meets the threshold of consistency.
#'
#' @details The consistency test is essential in the Analytic Hierarchy Process (AHP) to ensure
#' that the judgment matrix is reasonably consistent. A CR less than or equal to 0.10 indicates
#' acceptable consistency.
#' The reference values for the Random Index (RI) used are: c(0,0,0.52,0.89,1.12,1.26,1.36,1.41,1.46,1.49,1.52,1.54,1.56,1.58,1.59).
#'
#' @param lambda Numeric, the feature vector lambda calculated by `aw_weight()`.
#' @param n Integer, the number of factors used in the matrix.
#' @return Numeric, the Consistency Ratio (CR).
#' @export
#' @examples
#' \donttest{
#' lambda <- 1.5
#' n <- 10
#' CR_result <- consist_test(lambda, n)
#' print(CR_result)
#' }
consist_test <- function(lambda, n) {
  if (n > 15 || n < 2) {
    stop("The number of factors must be between 2 and 15.")
  }
  
  # Reference values for Random Index (RI) for 2-15 factors
  RI_refer =  c(0, 0.52, 0.89, 1.12, 1.26, 1.36, 1.41, 1.46, 1.49, 1.52, 1.54, 1.56, 1.58, 1.59)
  
  # Calculate Consistency Index (CI) and Consistency Ratio (CR)
  CI = (lambda - n) / (n - 1)
  CR = CI / RI_refer[n - 1]
  
  # Resulting CR value rounded for clarity
  CR_rounded = round(CR, 4)
  
  # Message based on CR result
  if (CR_rounded <= 0.1) {
    message("Pass the consistency test! CR: ", CR_rounded)
  } else {
    message("Failed the consistency test, please adjust the judgment matrix! CR: ", CR_rounded)
  }
  
  return(CR_rounded)
}