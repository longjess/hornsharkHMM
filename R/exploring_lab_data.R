#' Combine dynamic or static data with behavior timestamps
#'
#' If date and time are in same column, the data is split into separate
#' Date and Time columns.
#' New columns are created for Behavior and Prey info.
#' Creates new csv file with combined data.
#'
#' @param data_file Name of file containing dynamic or static data
#' @param timestamps_file Name of file containing behavior timestamps
#' @param filename Name of new file created
#'
#' @return None
#' @export
#'
#' @examples
#' get_custom_data("Lady_27Mar17_Dynamic25Hz.csv",
#' "TimestampedData_Lady_27Mar17.csv",
#' "Custom_Lady_27Mar17_dynamic.csv")
get_custom_data <- function(data_file, timestamps_file, filename) {
  data <- read.csv(file = data_file)
  timestamps <- read.csv(file = timestamps_file)

  if ("Date.and.Time" %in% colnames(data)) {
    data <- data %>%
      separate(col = Date.and.Time, into = c("Date", "Time"), sep = " ")
  }
  if ("Prey.Type.Notes" %in% colnames(timestamps)) {
    colnames(timestamps)[which(names(timestamps) ==
                                 "Prey.Type.Notes")] <- "Prey"
  }
  if ("Prey.Type" %in% colnames(timestamps)) {
    colnames(timestamps)[which(names(timestamps) ==
                                 "Prey.Type")] <- "Prey"
  }
  if ("Time.End" %in% colnames(timestamps)) {
    colnames(timestamps)[which(names(timestamps) ==
                                 "Time.End")] <- "Time.Stop"
  }

  data <- data %>% drop_na()

  data$Time <- chron(times = data$Time)
  timestamps$Time.Start <- chron(times = timestamps$Time.Start)
  timestamps$Time.Stop <- chron(times = timestamps$Time.Stop)
  data$Behavior <- NA
  data$Prey <- NA

  for(i in 1:nrow(timestamps)) {
    time_start <- timestamps[i,]$Time.Start
    time_stop <- timestamps[i,]$Time.Stop
    data$Behavior <- ifelse(time_start <= data$Time &
                              time_stop >= data$Time,
                            timestamps[i,]$Behavior, data$Behavior
    )
    data$Prey <- ifelse(time_start <= data$Time &
                          time_stop >= data$Time,
                        timestamps[i,]$Prey, data$Prey
    )
  }

  write.csv(data, file = filename, row.names = FALSE)
}

#' Create correlation plot
#'
#' Saves correlation plot as an image file.
#'
#' @param data A dataframe
#' @param filename Name of image file
#'
#' @return None
#' @export
#'
#' @examples
#' filename <- "Custom_Lady_27Mar17_dynamic.csv"
#' data <- read.csv(filename)
#' dynamic_data <- data %>% select(X_dynamic, Y_dynamic, Z_dynamic)
#' pairs_plot(dynamic_data, "Lady_27Mar17_correlation.png")
pairs_plot <- function(data, filename) {
  plot <- ggpairs(data,
                  lower = list(continuous = wrap("smooth", alpha = 0.2, size = 0.1)),
                  diag = list(continuous = "bar")
  ) +
    theme_minimal()
  ggsave(filename, plot)
}
