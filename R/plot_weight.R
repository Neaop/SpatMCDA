#' @title Create a weighting plot of the factors
#' @description Input the weights of all factors obtained by weight_fun() to obtain a weight plot.
#' @param data List,the weights of all factors obtained by weight_fun().
#'
#' @return A weighting plot of the factors
#' @export
#' @importFrom graphics barplot
#' @importFrom grDevices rgb
#' @examples
#' \donttest{
#' a <- c(1,3/4,1/2)
#' b <- c(4/3,1,1/3)
#' c <- c(2,3/2,1)
#' data <- data.frame(a,b,c)
#' weight_factor <- weight_fun(data)$weight_factor
#' plot_weight(weight_factor)
#' }
plot_weight <- function(data){
  #Create a weighted data frame
  weight <- as.data.frame(data)
  vector_weight <- data.frame(Vector = rownames(weight),
                              Weight = data,
                              row.names = NULL)
  # Create a weighting plot
  weight_pic <- barplot(height=vector_weight$Weight, names=vector_weight$Vector,
                        col=rgb(0.2,0.4,0.6,0.6),
                        horiz=T,las=1)
  #return(weight_pic)
}




