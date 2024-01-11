#' @title Output the weights of the factors
#' @description Input a judgment matrix after a two-by-two factor comparison
#'     to output the weights of all factors (the sum of all weights is 1).
#' @param data A data.frame after a two-by-two factor comparison.
#'
#' @return A decision matrix and weights of all factors (the sum of all weights is 1).
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' a <- c(1,3/4,1/2)
#' b <- c(4/3,1,1/3)
#' c <- c(2,3/2,1)
#' data <- data.frame(a,b,c)
#' weight_fun(data)
#' }
weight_fun <- function(data){
  data = data.matrix(data)
  rownames(data) = colnames(data)
  sum_vector_row    =  data %>% apply(2,sum)
  decide_matrix     =  data %>% apply(1,function(x) x/sum_vector_row)
  weight_factor     =  decide_matrix %>% apply(2,sum)
  result = list(decide_matrix = decide_matrix, weight_factor  = weight_factor/sum(weight_factor))
  return(result)
}




