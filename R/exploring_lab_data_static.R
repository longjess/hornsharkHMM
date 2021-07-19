#' Create line plots from static data
#'
#' @param data A dataframe with columns Time, X_static,
#' Y_static, Z_static
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return Image file containing line plots for X_static, Y_static, Z_static
#' @export
#'
#' @examples
line_plot_static <- function(data, filename) {
  plotx <- ggplot(data, aes(x = Time, y = X_static)) +
    geom_line(colour = "dark red") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  ploty <- ggplot(data, aes(x = Time, y = Y_static)) +
    geom_line(colour = "navy") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  plotz <- ggplot(data, aes(x = Time, y = Z_static)) +
    geom_line(colour = "dark green") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "static.png", sep = "_"), plot)
}

#' Create histograms from static data
#'
#' @param data A dataframe with columns X_static,
#' Y_static, Z_static
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return Image file containing histograms for X_static, Y_static, Z_static
#' @export
#'
#' @examples
hist_plot_static <- function(data, filename) {
  plotx <- ggplot(data, aes(X_static)) +
    geom_histogram(colour = "dark red", fill = "salmon") +
    theme_minimal()
  ploty <- ggplot(data, aes(Y_static)) +
    geom_histogram(colour = "navy", fill = "light blue") +
    theme_minimal()
  plotz <- ggplot(data, aes(Z_static)) +
    geom_histogram(colour = "dark green", fill = "light green") +
    theme_minimal()
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "static.png", sep = "_"), plot)
}

#' Create ACF plots from static data
#'
#' @inheritParams hist_plot_static
#'
#' @return Image file containing ACF plots for X_static, Y_static, Z_static
#' @export
#'
#' @examples
acf_plot_static <- function(data, filename) {
  plotx <- ggacf(data$X_static) +
    theme_minimal()
  ploty <- ggacf(data$Y_static) +
    theme_minimal()
  plotz <- ggacf(data$Z_static) +
    theme_minimal()
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "static.png", sep = "_"), plot)
}

#' Create PACF plots from static data
#'
#' @inheritParams hist_plot_static
#'
#' @return Image file containing PACF plots for X_static, Y_static, Z_static
#' @export
#'
#' @examples
pacf_plot_static <- function(data, filename) {
  plotx <- ggpacf(data$X_static) +
    theme_minimal()
  ploty <- ggpacf(data$Y_static) +
    theme_minimal()
  plotz <- ggpacf(data$Z_static) +
    theme_minimal()
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "static.png", sep = "_"), plot)
}

#' Create line plots from static data, which are colored by behavior
#'
#' @param data A dataframe with columns Time, Behavior, X_static,
#' Y_static, Z_static
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return An image file containing line plots for X_static, Y_static, Z_static.
#' Each line plot is colored by behavior.
#' @export
#'
#' @examples
behavior_plot_static <- function(data, filename) {
  plotx <- ggplot(data, aes(x = Time, y = X_static, colour = Behavior)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  ploty <- ggplot(data, aes(x = Time, y = Y_static, colour = Behavior)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  plotz <- ggplot(data, aes(x = Time, y = Z_static, colour = Behavior)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  plot <- grid_arrange_shared_legend(plotx, ploty, plotz,
                                     nrow = 3, ncol = 1,
                                     position = "right")
  ggsave(paste(filename, "static.png", sep = "_"), plot)
}

#' Create histograms from static data, aggregated by behavior
#'
#' @param data A dataframe with columns Behavior, X_static,
#' Y_static, Z_static
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return Several image files containing histograms for
#' X_static, Y_static, Z_static aggregated by behavior.
#' One set of images includes histograms for all behaviors in one
#' plot, divided by data type. Another set includes X_static, Y_static,
#' and Z_static histograms in one image, divided by behavior.
#' @export
#'
#' @examples
filtered_hist_static <- function(data, filename) {
  plotx <- ggplot(data,
                  aes(x = X_static, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "X_static.png", sep = "_"), plotx)
  ploty <- ggplot(data,
                  aes(x = Y_static, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "Y_static.png", sep = "_"), ploty)
  plotz <- ggplot(data,
                  aes(x = Z_static, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "Z_static.png", sep = "_"), plotz)

  behaviors <- unique(data$Behavior)
  for (i in seq_len(length(behaviors))) {
    behavior <- behaviors[i]
    subdata <- data %>% dplyr::filter(Behavior == behavior)

    plotx <- ggplot(subdata, aes(x = X_static)) +
      geom_histogram(colour = "dark red", fill = "salmon") +
      theme_minimal()
    ploty <- ggplot(subdata, aes(x = Y_static)) +
      geom_histogram(colour = "navy", fill = "light blue") +
      theme_minimal()
    plotz <- ggplot(subdata, aes(x = Z_static)) +
      geom_histogram(colour = "dark green", fill = "light green") +
      theme_minimal()
    plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
    ggsave(paste(filename, behavior, "static.png", sep = "_"), plot)
  }
}

#' Create histograms from static data, for each behavior interval
#'
#' A behavior interval is a time interval in which the behavior remains
#' constant.
#' Each behavior interval is labelled by a number, in chronological order.
#' Each plot contains histograms aggregating a given data type over
#' a given behavior.
#' If there are many behavior intervals for a given behavior, the
#' plot may be hard to read.
#'
#' @inheritParams filtered_hist_static
#'
#' @return Several image files. Each image file contains histograms
#' aggregating a given data type (X_static, Y_static, Z_static)
#' over a given behavior.
#' @export
#'
#' @examples
behavior_hist_static <- function(data, filename) {
  n <- length(data$Behavior)
  indicies <- c(1, which(data$Behavior != lag(data$Behavior)), n)
  m <- length(indicies)
  foo <- numeric(n)
  for (i in 1:(m - 1)) {
    foo[indicies[i]:indicies[i + 1]] <-
      rep(i, indicies[i + 1] - indicies[i] + 1)
  }
  data$BehaviorIndex <- as.character(foo)

  behaviors <- unique(data$Behavior)
  for (i in seq_len(length(behaviors))) {
    behavior <- behaviors[i]
    subdata <- data %>% filter(Behavior == behavior)

    plotx <- ggplot(data = subdata,
                    aes(x = X_static,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "X_static.png", sep = "_"), plotx)

    ploty <- ggplot(data = subdata,
                    aes(x = Y_static,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "Y_static.png", sep = "_"), ploty)

    plotz <- ggplot(data = subdata,
                    aes(x = Z_static,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "Z_static.png", sep = "_"), plotz)
  }
}

#' Create correlation plots of X_static, Y_static, and Z_static, divided
#' by behavior
#'
#' @inheritParams filtered_hist_static
#'
#' @return Several image plots of correlation plots, divided by behavior
#' @export
#'
#' @examples
behavior_pairs_plot_static <- function(data, filename) {
  behaviors <- unique(data$Behavior)
  for (i in seq_len(length(behaviors))) {
    behavior <- behaviors[i]
    subdata <- data %>% filter(Behavior == behavior)
    static_data <- subdata %>% select(X_static, Y_static, Z_static)

    plot <- ggpairs(static_data,
                    lower = list(continuous = wrap("smooth", size = 0.1)),
                    diag = list(continuous = "bar")
    ) +
      theme_minimal()

    ggsave(paste(filename, behavior, "correlation.png", sep = "_"), plot)
  }
}

#' Create multiple plots for static data
#'
#' @param names List of strings, containing the names used to identify the
#' data sets
#'
#' @return Multiple image files
#' @export
#'
#' @examples
get_plots_static <- function(names){
  n <- length(names)
  for (i in 1:n){
    name <- names[i]
    filename <- paste("Custom", index, "static.csv", sep = "_")
    data <- read.csv(filename)
    labelled_data <- data %>% filter(!is.na(Behavior))
    static_data <- data %>% select(X_static, Y_static, Z_static)

    line_plot_static(data, paste(name, "line_plot", sep = "_"))
    hist_plot_static(data, paste(name, "histogram", sep = "_"))
    acf_plot_static(data, paste(name, "acf", sep = "_"))
    pacf_plot_static(data, paste(name, "pacf", sep = "_"))
    behavior_plot_static(data, paste(name, "plot_behavior", sep = "_"))
    behavior_plot_static(labelled_data, paste(name, "plot_behavior_filtered", sep = "_"))
    filtered_hist_static(labelled_data, paste(name, "histogram_filtered", sep = "_"))
    behavior_hist_static(labelled_data, paste(name, "histogram_behavior", sep = "_"))
    pairs_plot(dynamic_data, paste(name, "correlation_static.png", sep = "_"))
    behavior_pairs_plot_static(data, paste(name, "static", sep = "_"))
  }
}
