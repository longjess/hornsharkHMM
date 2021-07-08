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
  ggplot(output, aes(obs)) +
    geom_histogram(binwidth = 1, colour = "navy", fill = "light blue") +
    theme_minimal()
}
