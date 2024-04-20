#' @title Visualize Weights of Factors
#'
#' @description Creates a horizontal bar plot visualizing the weights of factors as calculated by the `weight_fun()` function. This plot helps in understanding the relative importance of each factor in a decision-making model.
#'
#' @param data Numeric vector containing the weights of all factors obtained from `weight_fun()`.
#' @param title Optional; the title of the plot.
#'
#' @return Invisible returns the barplot object, primarily used for its side effect of plotting.
#' @export
#' @importFrom graphics barplot
#' @importFrom grDevices rgb
#' @examples
#' \donttest{
#' a <- c(1, 3/4, 1/2)
#' b <- c(4/3, 1, 1/3)
#' c <- c(2, 3/2, 1)
#' data_frame <- data.frame(a, b, c)
#' weight_factors <- weight_fun(data_frame)$weight_factor
#' plot_weight(weight_factors, "Factor Weight Distribution")
#' }
plot_weight <- function(data, title = "Weights of Factors") {
  # Define colors and border
  bar_colors <- rgb(0.1, 0.2, 0.5, 0.7) # A nice shade of blue with transparency
  border_color <- "white"

  # Create labels for the bars
  names <- names(data)
  if (is.null(names)) {
    names <- paste("Factor", seq_along(data))
  }

  # Calculate optimal width for bars based on number of factors
  bar_width <- 0.8 / length(data)

  max_val <- max(data) * 1.2

  # Create a bar plot
  barplot_heights <- barplot(data, names.arg = names, col = bar_colors,
                             border = border_color, horiz = TRUE, las = 1,
                             cex.names = 0.7, main = title, xlim = c(0, max_val),
                             width = bar_width, space = 0.2)

  # Add value labels to the bars
  text(x = data + 0.02, y = barplot_heights, labels = round(data, 2), pos = 4, cex = 0.7, col = "white")

  # Set overall plot margins and layout to optimize space in RStudio plot pane
  par(mar = c(5, 8, 4, 2) + 0.1)

  # Invisible return to emphasize plot creation
  invisible()
}
