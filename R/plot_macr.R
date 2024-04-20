#' @title Plot the Mean of Absolute Change Rates
#' 
#' @description Plots the mean of absolute change rates for various factors across different weight adjustments. This function visualizes the sensitivity of model outcomes to changes in factor weights, providing a clear visual representation of how different weight adjustments impact the calculated change rates.
#' 
#' @param data data.frame, a data frame that must include the following columns:
#'   - CRW: a numeric vector representing the change rate of weights (%) for each factor.
#'   - Value: a numeric vector representing the mean of absolute change rates (%) obtained from enhanced_macr() function or similar analyses.
#'   - Variable: a factor vector representing the names of the factors. Each factor name correlates with CRW and Value.
#'
#' @return A ggplot object that plots the mean of absolute change rates against the change rate of weights for each factor, differentiated by colors and shapes for each factor.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @examples
#' \donttest{
#' # Prepare data for three factors with their corresponding change rates and MACRs
#' CRW <- c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
#' Value_A <- c(0.2273, 0.1819, 0.1364, 0.0909, 0.0455, 0, 0.0455, 0.0909, 0.1364, 0.1819, 0.2273)
#' Value_B <- c(0.2213, 0.177, 0.1328, 0.0885, 0.0443, 0, 0.0443, 0.0885, 0.1328, 0.177, 0.2213)
#' Value_C <- c(0.11065, 0.08850, 0.06640, 0.04425, 0.02215, 0, 0.02215, 0.04425, 0.06640, 0.08850, 0.11065)
#' Factor_A <- rep("Factor_A", 11)
#' Factor_B <- rep("Factor_B", 11)
#' Factor_C <- rep("Factor_C", 11)
#'
#' # Combine into a data frame
#' macr_data <- data.frame(CRW = rep(CRW, 3), Value = c(Value_A, Value_B, Value_C),
#'                         Variable = factor(c(Factor_A, Factor_B, Factor_C)))
#'
#' # Plot the Mean of Absolute Change Rates
#' plot_macr(macr_data)
#' }
plot_macr <- function(data){
  ggplot2::ggplot(data = data, ggplot2::aes(x = CRW, y = Value, color = Variable, shape = Variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(y = "Mean of absolute change rate of result(%)",
                  x = "Change rate of weight(%)",
                  title = "Sensitivity Analysis") +
    ggplot2::theme_bw()
}


