#' Get file of means of dynamic data
#'
#' Means are taken every second (25 data points).
#'
#' @param old_filename Name of csv file containing dynamic data. Must have
#' columns Time, X_dynamic, Y_dynamic, Z_dynamic, Behavior, Prey.
#' @param filename Name of new csv file containing means
#'
#' @return None
#' @export
#'
#' @examples
#' old_filename <- "Custom_BigDaddy_3Apr17_dynamic.csv"
#' filename <- "Mean_BigDaddy_3Apr17_dynamic.csv"
#' get_dynamic_mean_data(old_filename, filename)
get_dynamic_mean_data <- function(old_filename, filename) {
  old_data <- read.csv(file = old_filename)

  means <- tapply(old_data$ODBA, old_data$Time, mean)
  data <- data_frame(ODBA = means, Time = dimnames(means)[[1]])
  data$Time <- chron(times = data$Time)
  data$X_dynamic <- tapply(old_data$X_dynamic, old_data$Time, mean)
  data$Y_dynamic <- tapply(old_data$Y_dynamic, old_data$Time, mean)
  data$Z_dynamic <- tapply(old_data$Z_dynamic, old_data$Time, mean)
  data$Behavior <- tapply(old_data$Behavior, old_data$Time, unique)
  data$Prey <- tapply(old_data$Prey, old_data$Time, unique)

  write.csv(data, file = filename, row.names = FALSE)
}

#' Get file of means of static data
#'
#' Means are taken every second (25 data points).
#'
#' @param old_filename Name of csv file containing static data. Must have
#' columns Time, X_static, Y_static, Z_static, Behavior, Prey.
#' @param filename Name of new csv file containing means
#'
#' @return
#' @export
#'
#' @examples
#' old_filename <- "Custom_BigDaddy_3Apr17_static.csv"
#' filename <- "Mean_BigDaddy_3Apr17_static.csv"
#' get_static_mean_data(old_filename, filename)
get_static_mean_data <- function(old_filename, filename) {
  old_data <- read.csv(file = old_filename)

  means <- tapply(old_data$X_static, old_data$Time, mean)
  data <- data_frame(X_static = means, Time = dimnames(means)[[1]])
  data$Time <- chron(times = data$Time)
  data$Y_static <- tapply(old_data$Y_static, old_data$Time, mean)
  data$Z_static <- tapply(old_data$Z_static, old_data$Time, mean)
  data$Behavior <- tapply(old_data$Behavior, old_data$Time, unique)
  data$Prey <- tapply(old_data$Prey, old_data$Time, unique)

  write.csv(data, file = filename, row.names = FALSE)
}

#' Create multiple files containing dynamic and static mean data
#'
#' @param names List of names of data sets
#'
#' @return None
#' @export
#'
#' @examples
#' names <- c("BigDaddy_3Apr17", "BigDaddy_20Mar17",
#' "BigGuy_15Feb18", "Eliza_7Sept17",
#' "Eliza_20Sept17", "Lady_27Mar17")
#' get_all_mean_plots(names)
get_all_mean_data <- function(names) {
  n <- length(names)
  for (i in 1:n){
    name <- names[i]

    old_dynamic_filename <- paste("Custom", name, "dynamic.csv", sep = "_")
    dynamic_filename <- paste("Mean", name, "dynamic.csv", sep = "_")
    get_dynamic_mean_data(old_dynamic_filename, dynamic_filename)

    old_static_filename <- paste("Custom", name, "static.csv", sep = "_")
    static_filename <- paste("Mean", name, "static.csv", sep = "_")
    get_static_mean_data(old_static_filename, static_filename)
  }
}

#' Create multiple plots for dynamic mean data
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
#' names <- c("BigDaddy_3Apr17", "BigDaddy_20Mar17",
#' "BigGuy_15Feb18", "Eliza_7Sept17",
#' "Eliza_20Sept17", "Lady_27Mar17")
#' get_dynamic_mean_plots(names)
get_dynamic_mean_plots <- function(names){
  n <- length(names)
  for (i in 1:n){
    name <- names[i]
    filename <- paste("Mean", name, "dynamic.csv", sep = "_")
    data <- read.csv(filename)
    labelled_data <- data %>% filter(!is.na(Behavior))
    dynamic_data <- data %>% select(X_dynamic, Y_dynamic, Z_dynamic)

    line_plot(data, paste(name, "mean", "line_plot", sep = "_"))
    hist_plot(data, paste(name, "mean", "histogram", sep = "_"))
    acf_plot(data, paste(name, "mean", "acf", sep = "_"))
    pacf_plot(data, paste(name, "mean", "pacf", sep = "_"))
    behavior_plot(data, paste(name, "mean", "plot_behavior", sep = "_"))
    behavior_plot(labelled_data, paste(name, "mean", "plot_behavior_filtered", sep = "_"))
    filtered_hist(labelled_data, paste(name, "mean", "histogram_filtered", sep = "_"))
    behavior_hist(labelled_data, paste(name, "mean", "histogram_behavior", sep = "_"))
    pairs_plot(dynamic_data, paste(name, "mean", "correlation_dynamic.png", sep = "_"))
    behavior_pairs_plot_dynamic(data, paste(name, "mean", "dynamic", sep = "_"))
  }
}

#' Create multiple plots for static mean data
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
#' names <- c("BigDaddy_3Apr17", "BigDaddy_20Mar17",
#' "BigGuy_15Feb18", "Eliza_7Sept17",
#' "Eliza_20Sept17", "Lady_27Mar17")
#' get_static_mean_plots(names)
get_static_mean_plots <- function(names){
  n <- length(names)
  for (i in 1:n){
    name <- names[i]
    filename <- paste("Mean", name, "static.csv", sep = "_")
    data <- read.csv(filename)
    labelled_data <- data %>% filter(!is.na(Behavior))
    static_data <- data %>% select(X_static, Y_static, Z_static)

    line_plot_static(data, paste(name, "mean", "line_plot", sep = "_"))
    hist_plot_static(data, paste(name, "mean", "histogram", sep = "_"))
    acf_plot_static(data, paste(name, "mean", "acf", sep = "_"))
    pacf_plot_static(data, paste(name, "mean", "pacf", sep = "_"))
    behavior_plot_static(data, paste(name, "mean", "plot_behavior", sep = "_"))
    behavior_plot_static(labelled_data, paste(name, "mean", "plot_behavior_filtered", sep = "_"))
    filtered_hist_static(labelled_data, paste(name, "mean", "histogram_filtered", sep = "_"))
    behavior_hist_static(labelled_data, paste(name, "mean", "histogram_behavior", sep = "_"))
    pairs_plot(static_data, paste(name, "mean", "correlation_static.png", sep = "_"))
    behavior_pairs_plot_static(data, paste(name, "mean", "static", sep = "_"))
  }
}

#' Create multiple plots for log ODBA mean data
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
#' names <- c("BigDaddy_3Apr17", "BigDaddy_20Mar17",
#' "BigGuy_15Feb18", "Eliza_7Sept17",
#' "Eliza_20Sept17", "Lady_27Mar17")
#' get_log_odba_mean_plots(names)
get_log_odba_mean_plots <- function(names){
  n <- length(names)
  for (i in 1:n){
    name <- names[i]
    filename <- paste("Mean", name, "dynamic.csv", sep = "_")
    data <- read.csv(filename)
    labelled_data <- data %>% filter(!is.na(Behavior))

    line_plot_log_odba(data, paste(name, "mean", "line_plot", sep = "_"))
    hist_plot_log_odba(data, paste(name, "mean", "histogram", sep = "_"))
    acf_plot_log_odba(data, paste(name, "mean", "acf", sep = "_"))
    pacf_plot_log_odba(data, paste(name, "mean", "pacf", sep = "_"))
    behavior_plot_log_odba(data, paste(name, "mean", "plot_behavior", sep = "_"))
    behavior_plot_log_odba(labelled_data, paste(name, "mean", "plot_behavior_filtered", sep = "_"))
    filtered_hist_log_odba(labelled_data, paste(name, "mean", "histogram_filtered", sep = "_"))
    behavior_hist_log_odba(labelled_data, paste(name, "mean", "histogram_behavior", sep = "_"))
  }
}
