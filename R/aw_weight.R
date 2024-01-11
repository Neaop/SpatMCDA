#' @title Output feature vector λ
#' @description Input a judgment matrix after a two-by-two factor comparison
#'     to output feature vector λ.
#'
#' @param data A data.frame after a two-by-two factor comparison.
#' @return Feature vector λ.
#' @export
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' a <- c(1,3/4,1/2)
#' b <- c(4/3,1,1/3)
#' c <- c(2,3/2,1)
#' data <- data.frame(a,b,c)
#' aw_weight(data)
#' }
aw_weight <- function(data) {
  data = data.matrix(data)
  rownames(data) = colnames(data)

  weight_factors = weight_fun(data)$weight_factor
  AW_Vector = data %*% weight_factors
  lambda = sum(AW_Vector / weight_factors) / length(AW_Vector)

  result = list(
    AW_Vector = AW_Vector,
    `Sum_AW_W` = AW_Vector / weight_factors,
    lambda = lambda
  )

  return(result)
}

utils::globalVariables(
  c("weight_fun")
)

