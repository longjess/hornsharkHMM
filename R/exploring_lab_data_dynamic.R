#' Create line plots from dynamic and ODBA data
#'
#' @param data A dataframe with columns Time, X_dynamic,
#' Y_dynamic, Z_dynamic, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return One image file containing line plots for X_dynamic, Y_dynamic, Z_dynamic,
#' and a separate image file containing a line plot for ODBA
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' line_plot_dynamic(data, "Lady_27Mar17_line_plot")
line_plot_dynamic <- function(data, filename) {
  plotx <- ggplot(data, aes(x = Time, y = X_dynamic)) +
    geom_line(colour = "dark red") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  ploty <- ggplot(data, aes(x = Time, y = Y_dynamic)) +
    geom_line(colour = "navy") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  plotz <- ggplot(data, aes(x = Time, y = Z_dynamic)) +
    geom_line(colour = "dark green") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "dynamic.png", sep = "_"), plot)
  plot_odba <- ggplot(data, aes(x = Time, y = ODBA)) +
    geom_line(colour = "dark red") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)
}

#' Create histograms from dynamic and ODBA data
#'
#' @param data A dataframe with columns X_dynamic,
#' Y_dynamic, Z_dynamic, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return One image file containing histograms for X_dynamic, Y_dynamic, Z_dynamic,
#' and a separate image file containing a histogram for ODBA
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' hist_plot_dynamic(data, "Lady_27Mar17_histogram")
hist_plot_dynamic  <- function(data, filename) {
  plotx <- ggplot(data, aes(X_dynamic)) +
    geom_histogram(colour = "dark red", fill = "salmon") +
    theme_minimal()
  ploty <- ggplot(data, aes(Y_dynamic)) +
    geom_histogram(colour = "navy", fill = "light blue") +
    theme_minimal()
  plotz <- ggplot(data, aes(Z_dynamic)) +
    geom_histogram(colour = "dark green", fill = "light green") +
    theme_minimal()
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "dynamic.png", sep = "_"), plot)
  plot_odba <- ggplot(data, aes(ODBA)) +
    geom_histogram(colour = "dark red", fill = "salmon") +
    theme_minimal()
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)
}

#' Create ACF plots from dynamic and ODBA data
#'
#' @inheritParams hist_plot_dynamic
#'
#' @return One image file containing ACF plots for X_dynamic, Y_dynamic, Z_dynamic,
#' and a separate image file containing an ACF plot for ODBA
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' acf_plot_dynamic(data, "Lady_27Mar17_acf")
acf_plot_dynamic <- function(data, filename) {
  plotx <- ggacf(data$X_dynamic) +
    theme_minimal()
  ploty <- ggacf(data$Y_dynamic) +
    theme_minimal()
  plotz <- ggacf(data$Z_dynamic) +
    theme_minimal()
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "dynamic.png", sep = "_"), plot)
  plot_odba <- ggacf(data$ODBA) +
    theme_minimal()
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)
}

#' Create PACF plots from dynamic and ODBA data
#'
#' @inheritParams hist_plot_dynamic
#'
#' @return One image file containing PACF plots for X_dynamic, Y_dynamic, Z_dynamic,
#' and a separate image file containing a PACF plot for ODBA
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' pacf_plot_dynamic(data, "Lady_27Mar17_pacf")
pacf_plot_dynamic  <- function(data, filename) {
  plotx <- ggpacf(data$X_dynamic) +
    theme_minimal()
  ploty <- ggpacf(data$Y_dynamic) +
    theme_minimal()
  plotz <- ggpacf(data$Z_dynamic) +
    theme_minimal()
  plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
  ggsave(paste(filename, "dynamic.png", sep = "_"), plot)
  plot_odba <- ggpacf(data$ODBA) +
    theme_minimal()
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)
}


#' Create line plots from dynamic and ODBA data, which are colored by behavior
#'
#' @param data A dataframe with columns Time, Behavior, X_dynamic,
#' Y_dynamic, Z_dynamic, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return One image file containing line plots for X_dynamic, Y_dynamic, Z_dynamic,
#' and a separate image file containing a line plot for ODBA. Each line plot is colored
#' by behavior.
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' labelled_data <- data %>% filter(!is.na(Behavior))
#' behavior_plot_dynamic(labelled_data, "Lady_27Mar17_plot_behavior")
behavior_plot_dynamic  <- function(data, filename) {
  plotx <- ggplot(data, aes(x = Time, y = X_dynamic, colour = Behavior)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  ploty <- ggplot(data, aes(x = Time, y = Y_dynamic, colour = Behavior)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  plotz <- ggplot(data, aes(x = Time, y = Z_dynamic, colour = Behavior)) +
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
  ggsave(paste(filename, "dynamic.png", sep = "_"), plot)
  plot_odba <- ggplot(data, aes(x = Time, y = ODBA, colour = Behavior)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)
}

#' Create histograms from dynamic and ODBA data, aggregated by behavior
#'
#' @param data A dataframe with columns Behavior, X_dynamic,
#' Y_dynamic, Z_dynamic, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return Several image files containing histograms for
#' X_dynamic, Y_dynamic, Z_dynamic, and ODBA aggregated by behavior.
#' One set of images includes histograms for all behaviors in one
#' plot, divided by data type. Another set includes X_dynamic, Y_dynamic,
#' and Z_dynamic histograms in one image and ODBA histogram in another,
#' divided by behavior.
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' labelled_data <- data %>% filter(!is.na(Behavior))
#' filtered_hist_dynamic(labelled_data, "Lady_27Mar17_histogram_filtered")
filtered_hist_dynamic  <- function(data, filename) {
  plotx <- ggplot(data,
                  aes(x = X_dynamic, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "X_dynamic.png", sep = "_"), plotx)
  ploty <- ggplot(data,
                  aes(x = Y_dynamic, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "Y_dynamic.png", sep = "_"), ploty)
  plotz <- ggplot(data,
                  aes(x = Z_dynamic, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "Z_dynamic.png", sep = "_"), plotz)
  plot_odba <- ggplot(data, aes(x = ODBA, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)

  behaviors <- unique(data$Behavior)
  for (i in seq_len(length(behaviors))) {
    behavior <- behaviors[i]
    subdata <- data %>% dplyr::filter(Behavior == behavior)

    plotx <- ggplot(subdata, aes(x = X_dynamic)) +
      geom_histogram(colour = "dark red", fill = "salmon") +
      theme_minimal()
    ploty <- ggplot(subdata, aes(x = Y_dynamic)) +
      geom_histogram(colour = "navy", fill = "light blue") +
      theme_minimal()
    plotz <- ggplot(subdata, aes(x = Z_dynamic)) +
      geom_histogram(colour = "dark green", fill = "light green") +
      theme_minimal()
    plot <- grid.arrange(plotx, ploty, plotz, nrow = 3)
    ggsave(paste(filename, behavior, "dynamic.png", sep = "_"), plot)
    plot_odba <- ggplot(subdata, aes(x = ODBA)) +
      geom_histogram(colour = "dark red", fill = "salmon") +
      theme_minimal()
    ggsave(paste(filename, behavior, "ODBA.png", sep = "_"), plot_odba)
  }
}

#' Create histograms from dynamic and ODBA data, for each behavior interval
#'
#' A behavior interval is a time interval in which the behavior remains
#' constant.
#' Each behavior interval is labelled by a number, in chronological order.
#' Each plot contains histograms aggregating a given data type over
#' a given behavior.
#' If there are many behavior intervals for a given behavior, the
#' plot may be hard to read.
#'
#' @inheritParams filtered_hist_dynamic
#'
#' @return Several image files. Each image file contains histograms
#' aggregating a given data type (X_dynamic, Y_dynamic, Z_dynamic, ODBA)
#' over a given behavior.
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' labelled_data <- data %>% filter(!is.na(Behavior))
#' behavior_hist_dynamic(labelled_data, "Lady_27Mar17_histogram_behavior")
behavior_hist_dynamic  <- function(data, filename) {
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
                    aes(x = X_dynamic,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "X_dynamic.png", sep = "_"), plotx)

    ploty <- ggplot(data = subdata,
                    aes(x = Y_dynamic,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "Y_dynamic.png", sep = "_"), ploty)

    plotz <- ggplot(data = subdata,
                    aes(x = Z_dynamic,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "Z_dynamic.png", sep = "_"), plotz)

    plot_odba <- ggplot(data = subdata,
                        aes(x = ODBA,
                            colour = BehaviorIndex,
                            fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "ODBA.png", sep = "_"), plot_odba)
  }
}

#' Create correlation plots of X_dynamic, Y_dynamic, and Z_dynamic,
#' divided by behavior
#'
#' @inheritParams filtered_hist_dynamic
#'
#' @return Several image plots of correlation plots, divided by behavior
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' behavior_pairs_plot_dynamic(data, "Lady_27Mar17_dynamic")
behavior_pairs_plot_dynamic <- function(data, filename) {
  behaviors <- unique(data$Behavior)
  for (i in seq_len(length(behaviors))) {
    behavior <- behaviors[i]
    subdata <- data %>% filter(Behavior == behavior)
    dynamic_data <- subdata %>% select(X_dynamic, Y_dynamic, Z_dynamic)

    plot <- ggpairs(dynamic_data,
                    lower = list(continuous = wrap("smooth", size = 0.1)),
                    diag = list(continuous = "bar")
    ) +
      theme_minimal()

    ggsave(paste(filename, behavior, "correlation.png", sep = "_"), plot)
  }
}

#' Create multiple plots for dynamic and ODBA data
#'
#' @param names List of strings, containing the names used to identify the
#' data sets
#'
#' @return Multiple image files
#' @export
#'
#' @examples
#' names <- c("BigDaddy_3Apr17", "BigDaddy_20Mar17",
#' "BigGuy_15Feb18", "Eliza_7Sept17",
#' "Eliza_20Sept17", "Lady_27Mar17")
#' get_plots_dynamic(names)
get_plots_dynamic <- function(names){
  n <- length(names)
  for (i in 1:n){
    name <- names[i]
    filename <- paste("Custom", index, "dynamic.csv", sep = "_")
    data <- read.csv(filename)
    labelled_data <- data %>% filter(!is.na(Behavior))
    dynamic_data <- data %>% select(X_dynamic, Y_dynamic, Z_dynamic)

    line_plot_dynamic(data, paste(name, "line_plot", sep = "_"))
    hist_plot_dynamic(data, paste(name, "histogram", sep = "_"))
    acf_plot_dynamic(data, paste(name, "acf", sep = "_"))
    pacf_plot_dynamic(data, paste(name, "pacf", sep = "_"))
    behavior_plot_dynamic(data, paste(name, "plot_behavior", sep = "_"))
    behavior_plot_dynamic(labelled_data, paste(name, "plot_behavior_filtered", sep = "_"))
    filtered_hist_dynamic(labelled_data, paste(name, "histogram_filtered", sep = "_"))
    behavior_hist_dynamic(labelled_data, paste(name, "histogram_behavior", sep = "_"))
    pairs_plot(dynamic_data, paste(name, "correlation_dynamic.png", sep = "_"))
    behavior_pairs_plot_dynamic(data, paste(name, "dynamic", sep = "_"))
  }
}
