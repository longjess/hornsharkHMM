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

  # Add Behavior and Prey column to main data set, based on data in timestamps
  data$Time <- chron(times. = data$Time)
  timestamps$Time.Start <- chron(times. = timestamps$Time.Start)
  timestamps$Time.Stop <- chron(times. = timestamps$Time.Stop)
  data$Behavior <- ifelse(timestamps$Time.Start <= data$Time &
                            timestamps$Time.Stop >= data$Time,
                          timestamps$Behavior, NA
  )
  data$Prey <- ifelse(timestamps$Time.Start <= data$Time &
                        timestamps$Time.Stop >= data$Time,
                      timestamps$Prey, NA
  )
  # Write to CSV
  write.csv(data, file = filename, row.names = FALSE)
}

line_plot <- function(data, filename) {
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

hist_plot <- function(data, filename) {
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

acf_plot <- function(data, filename) {
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

behavior_plot <- function(data, filename) {
  plotx <- ggplot(data, aes(x = Time, y = X_dynamic, colour = Behavior)) +
    geom_point(size = 0.5) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  ploty <- ggplot(data, aes(x = Time, y = Y_dynamic, colour = Behavior)) +
    geom_point(size = 0.5) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  plotz <- ggplot(data, aes(x = Time, y = Z_dynamic, colour = Behavior)) +
    geom_point(size = 0.5) +
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
    geom_point(size = 0.5) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      text = element_text(size = 7)
    ) +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "ODBA.png", sep = "_"), plot_odba)
}

filtered_hist <- function(data, filename) {
  plotx <- ggplot(data,
                  aes(x = X_dynamic, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "X.png", sep = "_"), plotx)
  ploty <- ggplot(data,
                  aes(x = Y_dynamic, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "Y.png", sep = "_"), ploty)
  plotz <- ggplot(data,
                  aes(x = Z_dynamic, colour = Behavior, fill = Behavior)) +
    geom_histogram() +
    facet_wrap(~Behavior, ncol = 2) +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    scale_color_brewer(palette = "Set1")
  ggsave(paste(filename, "Z.png", sep = "_"), plotz)
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
    subdata <- data %>% filter(Behavior == behavior)

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

behavior_hist <- function(data, filename) {
  # Create new column indicating each subinterval of behavior
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
    ggsave(paste(filename, behavior, "X.png", sep = "_"), plotx)

    ploty <- ggplot(data = subdata,
                    aes(x = Y_dynamic,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "Y.png", sep = "_"), ploty)

    plotz <- ggplot(data = subdata,
                    aes(x = Z_dynamic,
                        colour = BehaviorIndex,
                        fill = BehaviorIndex)) +
      geom_histogram() +
      facet_wrap(~BehaviorIndex, ncol = 2) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      scale_color_brewer(palette = "Set1")
    ggsave(paste(filename, behavior, "Z.png", sep = "_"), plotz)

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

pairs_plot <- function(data, filename) {
  plot <- ggpairs(data,
                  lower = list(continuous = wrap("smooth", alpha = 0.2, size = 0.1)),
                  diag = list(continuous = "bar")
  ) +
    theme_minimal()
  ggsave(filename, plot)
}

pseudo_residual_plot <- function(data, filename) {
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
  ggsave(paste(filename, "png", sep = "."), plot)
}
