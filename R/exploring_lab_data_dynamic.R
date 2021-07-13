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

behavior_hist_dynamic  <- function(data, filename) {
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
