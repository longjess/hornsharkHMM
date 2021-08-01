#' Create line plots from log ODBA data
#'
#' Creates image file containing a line plot for log ODBA.
#'
#' @param data A dataframe with columns Time, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return None
#' @export
#'
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' line_plot_log_odba(data, "Lady_27Mar17_line_plot")
line_plot_log_odba <- function(data, filename) {
  n <- length(data$Date)
  plot <- ggplot(data, aes(x = Time, y = log(ODBA), group = 1)) +
    geom_line(colour = "dark red") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  ggsave(paste(filename, "log_ODBA.png", sep = "_"), plot)
}

#' Create histogram from log ODBA data
#'
#' Creates image file containing a histogram for log ODBA.
#'
#'
#' @param data A dataframe with column ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return None
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' hist_plot_log_odba(data, "Lady_27Mar17_histogram")
hist_plot_log_odba <- function(data, filename) {
  plot <- ggplot(data, aes(log(ODBA))) +
    geom_histogram(colour = "dark red", fill = "salmon") +
    theme_minimal()
  ggsave(paste(filename, "log_ODBA.png", sep = "_"), plot)
}

#' Create ACF plots from log ODBA data
#'
#' Creates image file containing an ACF plot for log ODBA.
#'
#' @inheritParams hist_plot_log_odba
#'
#' @return None
#' @export
#'
#' @examples
acf_plot_log_odba<- function(data, filename) {
  plot <- ggacf(log(data$ODBA)) +
    theme_minimal()
  ggsave(paste(filename, "log_ODBA.png", sep = "_"), plot)
}

#' Create PACF plots from log ODBA data
#'
#' Creates image file containing a PACF plot for log ODBA.
#'
#' @inheritParams hist_plot_log_odba
#'
#' @return None
#' @export
#'
#' @examples
pacf_plot_log_odba<- function(data, filename) {
  plot <- ggpacf(log(data$ODBA)) +
    theme_minimal()
  ggsave(paste(filename, "log_ODBA.png", sep = "_"), plot)
}

#' Create line plots from log ODBA data, which are colored by behavior
#'
#' Creates image file containing a line plot for log ODBA, colored
#' by behavior.
#'
#' @param data A dataframe with columns Time, Behavior, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return None
#' @export
#'
#' @examples
behavior_plot_log_odba <- function(data, filename) {
  n <- length(data$Date)
  plot <- ggplot(data, aes(x = Time, y = log(ODBA), colour = Behavior, group = 1)) +
    geom_line() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "log_ODBA.png", sep = "_"), plot)
}

#' Create histograms from log ODBA data, aggregated by behavior
#'
#' Creates several image files containing histograms for
#' log ODBA, aggregated by behavior. One image includes histograms
#' for all behaviors in one plot. The remaining images contain a histogram
#' for log ODBA for a single behavior.
#'
#' @param data A dataframe with columns Behavior, ODBA
#' @param filename String containing the first part of filename for the
#' image files to be created
#'
#' @return None
#' @export
#'
#' @examples
filtered_hist_log_odba<- function(data, filename) {
  plot <- ggplot(data, aes(x = log(ODBA), colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "log_ODBA.png", sep = "_"), plot)

  behaviors <- unique(data$Behavior)
  for (i in seq_len(length(behaviors))) {
    behavior <- behaviors[i]
    subdata <- data %>% dplyr::filter(Behavior == behavior)

    plot <- ggplot(subdata, aes(x = log(ODBA))) +
      geom_histogram(colour = "dark red", fill = "salmon") +
      theme_minimal()
    ggsave(paste(filename, behavior, "log_ODBA.png", sep = "_"), plot)
  }
}

#' Create histograms from log ODBA data, for each behavior interval
#'
#' Each image file contains histograms aggregating log ODBA
#' over each behavior interval for a given behavior.
#' A behavior interval is a time interval in which the behavior remains
#' constant.
#' Each behavior interval is labelled by a number, in chronological order.
#' If there are many behavior intervals for a given behavior, the
#' plot may be hard to read.
#'
#' @inheritParams filtered_hist_log_odba
#'
#' @return None
#' @export
#'
#' @examples
behavior_hist_log_odba <- function(data, filename) {
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

    plot <- ggplot(data = subdata,
                   aes(x = log(ODBA),
                       colour = BehaviorIndex,
                       fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "log_ODBA.png", sep = "_"), plot)
  }
}

#' Create multiple plots for log ODBA data
#'
#' Creates multiple image files, with different line plots, histograms, etc.
#'
#' @param names List of strings, containing the names used to identify the
#' data sets
#'
#' @return None
#' @export
#'
#' @examples
get_plots_log_odba <- function(names){
  n <- length(names)
  for (i in 1:n){
    name <- names[i]
    filename <- paste("Custom", name, "dynamic.csv", sep = "_")
    data <- read.csv(filename)
    labelled_data <- data %>% filter(!is.na(Behavior))

    line_plot_log_odba(data, paste(name, "line_plot", sep = "_"))
    hist_plot_log_odba(data, paste(name, "histogram", sep = "_"))
    acf_plot_log_odba(data, paste(name, "acf", sep = "_"))
    pacf_plot_log_odba(data, paste(name, "pacf", sep = "_"))
    behavior_plot_log_odba(data, paste(name, "plot_behavior", sep = "_"))
    behavior_plot_log_odba(labelled_data, paste(name, "plot_behavior_filtered", sep = "_"))
    filtered_hist_log_odba(labelled_data, paste(name, "histogram_filtered", sep = "_"))
    behavior_hist_log_odba(labelled_data, paste(name, "histogram_behavior", sep = "_"))
  }
}
