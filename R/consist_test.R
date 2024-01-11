#' @title Output consistency test results
#' @description Input λ and the number of factors to
#'     get the results of the consistency test.
#' @details A maximum of 15 factors can be tested.
#' The reference value of RI is c(0,0,0.52,0.89,1.12,1.26,1.36,1.41,1.46,1.49,1.52,1.54,1.56,1.58,1.59).
#' @param λ Numeric, calculated by aw_weight().
#' @param n Integer, the number of factors.
#' @return Results of the consistency test.
#' @export
#' @examples
#' \donttest{
#' λ <- 1.5
#' n <- 10
#' consist_test(λ,n)
#' }


consist_test <- function(λ, n) {
  # Reference values for Random Index (RI) for 1-15 factors
  RI_refer =  c(0, 0, 0.52, 0.89, 1.12, 1.26, 1.36, 1.41, 1.46, 1.49, 1.52, 1.54, 1.56, 1.58, 1.59)

  # Display the reference values
  RI_values_str = paste(RI_refer, collapse = ", ")
  message("The reference values for RI (1-15 factor reference) are: ", RI_values_str)

  # Calculate Consistency Index (CI) and Consistency Ratio (CR)
  CI = (λ - n) / (n - 1)
  CR = CI / RI_refer[n]

  # Check for consistency
  if (CR <= 0.1) {
    cat("Pass the consistency test!\n")
    cat("CR: ", round(CR, 4), "\n")
  } else {
    cat("Failed the consistency test, please adjust the judgment matrix!\n")
  }

  return(CR)
}

