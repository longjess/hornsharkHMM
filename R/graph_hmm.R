#' Creates a line plot from data generated from univariate HMM
#'
#' The plot is coloured by state.
#'
#' @param output Dataframe with columns index, obs, state.
#'
#' @return ggplot
#' @export
#'
#' @examples
graph_hmm_output <- function(output) {
  plot <- ggplot(output, aes(x = index, y = obs, color = state)) +
    geom_line() +
    theme_minimal() +
    scale_colour_continuous(type = "viridis")
  return(plot)
}

#' Creates histogram from data generated from univariate HMM
#'
#' @param output Dataframe with columns index, obs
#'
#' @return ggplot
#' @export
#'
#' @examples
graph_hmm_hist <- function(output) {
  plot <- ggplot(output, aes(obs)) +
    geom_histogram(binwidth = 1, colour = "navy", fill = "light blue") +
    theme_minimal()
  return(plot)
}

#' Pseudo residual plots
#'
#' @param data Dataframe including index, npsr (normal pseudo-residuals)
#'
#' @return ggplot, including index plot, histogram, qq plot, acf plot
#' @export
#'
#' @examples
pseudo_residual_plot <- function(data) {
  # Index plot of pseudo-residuals
  plot_index <- ggplot(data) +
    geom_point(aes(x = index, y = npsr), size = 0.5, colour = "black") +
    theme_minimal()
  # Histogram of pseudo-residuals
  plot_hist <- ggplot(data, aes(npsr)) +
    geom_histogram(aes(y = ..density..), colour = "navy", fill = "light blue") +
    stat_function(fun = dnorm, colour = "red") +
    theme_minimal()
  # QQ plot of pseudo-residuals
  plot_qq <- ggplot(data, aes(sample = npsr)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal()
  # ACF of pseudo-residuals
  plot_acf <- ggacf(data$npsr) +
    theme_minimal()
  plot <- grid.arrange(plot_index, plot_hist, plot_qq, plot_acf,
                       nrow = 2, ncol = 2)
  return(plot)
}
