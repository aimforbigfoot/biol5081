# Load required libraries
library(ggplot2)
library(gridExtra)  # For side-by-side plots
# Parameters
numOfRepeats <- 4
noise_sd <- 1
old_ego_bias_strength <- 0.1
new_ego_bias_strength <- 0.2
allo_bias_strength <- 0.2
# Specified ego positions
ego_positions <- list(
  old = c(0, 0),
  new1 = c(0, 90),
  new2 = c(-60, 45),
  new3 = c(60, 45)
)
# Specified points
points <- data.frame(
  x = c(30, -30, 30, -30),
  y = c(30, 30, 60, 60)
)
# Compute average point for allocentric bias
avg_point <- colMeans(points)
# Function to generate biased data
generate_biased_data <- function(points, numOfRepeats, noise_sd, old_ego_bias, new_ego_bias, allo_bias, ego_position) {
  data <- data.frame()
  
  for (i in 1:nrow(points)) {
    x_noise <- rnorm(numOfRepeats, mean = points$x[i], sd = noise_sd)
    y_noise <- rnorm(numOfRepeats, mean = points$y[i], sd = noise_sd)
    
    # Apply old ego bias toward (0, 0)
    x_biased <- x_noise + old_ego_bias * (0 - x_noise)
    y_biased <- y_noise + old_ego_bias * (0 - y_noise)
    
    # Apply new ego bias toward specified position
    x_biased <- x_biased + new_ego_bias * (ego_position[1] - x_biased)
    y_biased <- y_biased + new_ego_bias * (ego_position[2] - y_biased)
    
    # Apply allocentric bias toward the average point
    x_biased <- x_biased + allo_bias * (avg_point["x"] - x_biased)
    y_biased <- y_biased + allo_bias * (avg_point["y"] - y_biased)
    
    temp_data <- data.frame(
      x = x_biased,
      y = y_biased,
      group = paste0("Point_", i)
    )
    data <- rbind(data, temp_data)
  }
  
  return(data)
}
# Generate data for all ego positions
data_list <- lapply(ego_positions, function(pos) {
  generate_biased_data(points, numOfRepeats, noise_sd, old_ego_bias_strength, new_ego_bias_strength, allo_bias_strength, pos)
})

# Compute meta points for all data sets
meta_points_list <- lapply(data_list, function(data) {
  aggregate(cbind(x, y) ~ group, data = data, FUN = mean)
})
# Combine meta points for bar plot
meta_points_combined <- do.call(rbind, lapply(seq_along(meta_points_list), function(i) {
  meta_points <- meta_points_list[[i]]
  meta_points$variant <- names(ego_positions)[i]
  return(meta_points)
}))
# Bar plot for X and Y averages
bar_plot_x <- ggplot(meta_points_combined, aes(x = group, y = x, fill = variant)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average X Positions", x = "Location", y = "X Average") +
  theme_minimal()
bar_plot_y <- ggplot(meta_points_combined, aes(x = group, y = y, fill = variant)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Y Positions", x = "Location", y = "Y Average") +
  theme_minimal()
# Arrange bar plots side by side
grid.arrange(bar_plot_x, bar_plot_y, ncol = 2)

