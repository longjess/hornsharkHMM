get_custom_data <- function(data_file, timestamps_file, filename) {
  data <- read.csv(file = data_file)
  timestamps <- read.csv(file = timestamps_file)

  # Alter and rename columns to be consistent
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

  # Filter out rows with NA for data values
  data <- data %>% drop_na()

  # Add Behavior and Prey column to main data set, based on data in timestamps
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

  # Write to CSV
  write.csv(data, file = filename, row.names = FALSE)
}

pairs_plot <- function(data, filename) {
  plot <- ggpairs(data,
                  lower = list(continuous = wrap("smooth", alpha = 0.2, size = 0.1)),
                  diag = list(continuous = "bar")
  ) +
    theme_minimal()
  ggsave(filename, plot)
}
