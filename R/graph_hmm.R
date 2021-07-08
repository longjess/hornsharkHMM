#' Title
#'
#' @param output
#'
#' @return
#' @export
#'
#' @examples
graph_hmm_output <- function(output) {
  ggplot(output, aes(x = index, y = obs, color = state)) +
    geom_line() +
    theme_minimal() +
    scale_colour_continuous(type = "viridis")
}

graph_hmm_hist <- function(output) {
  ggplot(output, aes(obs)) +
    geom_histogram(binwidth = 1, colour = "navy", fill = "light blue") +
    theme_minimal()
}
