# ---------------- Data Generation Setup ------------
# Purpose: This script generates simulated data for experiments evaluating positional deviations in response to different movement conditions.

## ---------------- Library Importing ---------------
# Load essential libraries for data manipulation, visualization, and statistics
library(ggplot2)       # For creating plots
library(gridExtra)     # For arranging multiple plots
library(dplyr)         # For data manipulation
library(tidyr)         # For reshaping data structures

## ---------------- Parameters ---------------
set.seed(123)
# Define experiment parameters
numOfPeople <- 10      # Number of participants
numOfRepeats <- 4      # Number of repetitions for each point
noise_sd <- 2.5        # Standard deviation for random noise added to the data

# Define bias strengths for various movement conditions
# These parameters determine how positions are distorted based on biases:
old_ego_bias_strength_inital <- 0.1
old_ego_bias_strength_opposite <- 0.1
old_ego_bias_strength_adj_cw <- 0.05
old_ego_bias_strength_adj_ccw <- 0.05

new_ego_bias_strength_opposite <- 0.1
new_ego_bias_strength_adj_cw <- 0.2
new_ego_bias_strength_adj_ccw <- 0.2

allo_bias_strength_inital <- 0.05
allo_bias_strength_opposite <- 0.2
allo_bias_strength_adj_cw <- 0.1
allo_bias_strength_adj_ccw <- 0.1

# Define the table's dimensions (90x90 cm) and ego positions around it
# Positions are centered at (0, 0), with predefined movement conditions
ego_positions_around_table <- list(
  inital = c(45, 0),
  opposite = c(45, 90),
  adj_ccw = c(90, 45),
  adj_cw = c(0, 45)
)

# Define the coordinates of points to be measured relative to the table's center
pointsToMeasure <- list(
  bl = c(-30, -30),  # Bottom-left
  br = c(30, -30),   # Bottom-right
  tl = c(-30, 30),   # Top-left
  tr = c(30, 30)     # Top-right
)

## ---------------- Data Generation ------------
# Initialize an empty data frame for storing results
df <- data.frame(
  Person = integer(),
  PointName = character(),
  moveCondition = character(),
  Repeat = integer(),
  X = numeric(),
  Y = numeric(),
  responseTime = numeric(), # Simulated response time
  timeToMove = numeric()     # Time taken to move between conditions
)

# Nested loops generate data for each participant, movement condition, and point
for (person in 1:numOfPeople) {
  for (movementCondition in names(ego_positions_around_table)) {
    # Set initial ego position and biases for each condition
    initalEgoPos <- ego_positions_around_table[["inital"]]
    finalEgoPos <- initalEgoPos
    old_ego_bias_strength <- 0.0
    new_ego_bias_factor <- 0.0
    allocentric_bias_factor <- 0.0
    responseTime <- rnorm(1, 2.5, 2)  # Simulate response time with normal distribution
    timeToMove <- 5                  # Default time to move
    
    # Adjust parameters based on movement condition
    if (movementCondition == "opposite") {
      finalEgoPos <- ego_positions_around_table[["opposite"]]
      old_ego_bias_strength <- old_ego_bias_strength_opposite
      new_ego_bias_factor <- new_ego_bias_strength_opposite
      allocentric_bias_factor <- allo_bias_strength_opposite
      timeToMove <- timeToMove + abs(rnorm(1, mean = 0, sd = 1))
    } else if (movementCondition == "adj_ccw") {
      finalEgoPos <- ego_positions_around_table[["adj_ccw"]]
      old_ego_bias_strength <- old_ego_bias_strength_adj_ccw
      new_ego_bias_factor <- new_ego_bias_strength_adj_ccw
      allocentric_bias_factor <- allo_bias_strength_adj_ccw
      timeToMove <- timeToMove + abs(rnorm(1, mean = 0, sd = 0.5))
    } else if (movementCondition == "adj_cw") {
      finalEgoPos <- ego_positions_around_table[["adj_cw"]]
      old_ego_bias_strength <- old_ego_bias_strength_adj_cw
      new_ego_bias_factor <- new_ego_bias_strength_adj_cw
      allocentric_bias_factor <- allo_bias_strength_adj_cw
      timeToMove <- timeToMove + abs(rnorm(1, mean = 0, sd = 0.5))
    } else { # inital
      old_ego_bias_strength <- old_ego_bias_strength_inital
      allocentric_bias_factor <- allo_bias_strength_inital
    }
    
    # Generate data for each point and repeat the measurement
    for (point_name in names(pointsToMeasure)) {
      for (i in 1:numOfRepeats) {
        # Calculate actual point coordinates with adjustments for center
        point_coords <- pointsToMeasure[[point_name]] + c(45, 45)
        actualPosition <- point_coords
        
        # Add random noise to coordinates
        point_coords[1] <- point_coords[1] + rnorm(1, mean = 0, sd = noise_sd)
        point_coords[2] <- point_coords[2] + rnorm(1, mean = 0, sd = noise_sd)
        
        # Apply biases and distortions
        point_coords <- point_coords + allocentric_bias_factor
        point_coords <- point_coords + old_ego_bias_strength * (initalEgoPos - actualPosition)
        if (movementCondition != "inital") {
          point_coords <- point_coords + new_ego_bias_factor * (finalEgoPos - actualPosition)
        }
        distortion_factor <- abs(rnorm(1, mean = 0.02, sd = 0.05)) * (1 + (timeToMove + responseTime) / 10)
        point_coords <- point_coords + distortion_factor * (c(45, 45) - point_coords)
        
        # Append generated data to the data frame
        df <- rbind(df, data.frame(
          Person = person,
          PointName = point_name,
          moveCondition = movementCondition,
          Repeat = i,
          X = point_coords[1],
          Y = point_coords[2],
          responseTime = responseTime,
          timeToMove = timeToMove
        ))
      }
    }
  }
}

# Rename column for clarity
df <- df %>% rename(movementCondition = moveCondition)

# View the resulting data frame to confirm generation
print(df)

# --------------- Visualization ---------------------
# Prepare data frames for plotting ego positions and measurement points
ego_positions_df <- do.call(rbind, lapply(names(ego_positions_around_table), function(name) {
  data.frame(
    movementCondition = name,
    X = ego_positions_around_table[[name]][1],
    Y = ego_positions_around_table[[name]][2]
  )
}))

points_to_measure_df <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X = pointsToMeasure[[name]][1] + 45,
    Y = pointsToMeasure[[name]][2] + 45
  )
}))
# Define descriptive labels for PointName
point_labels <- c(
  "tl" = "Top Left",
  "tr" = "Top Right",
  "bl" = "Bottom Left",
  "br" = "Bottom Right"
)

# Plot raw data with points and labels
raw_plot <- ggplot(df, aes(x = X, y = Y, color = movementCondition, shape = movementCondition)) +
  # Plot raw data
  geom_point(size = 2, alpha = 0.6) +
  # Plot ego positions with movementCondition
  geom_point(data = ego_positions_df, aes(x = X, y = Y, color = movementCondition, shape = movementCondition), size = 4) +
  # Plot points to measure with color determined by PointName
  geom_point(data = points_to_measure_df, aes(x = X, y = Y, color = PointName), size = 3, stroke = 3, shape = 4) +
  # Customize labels
  labs(
    title = "Raw Data with Ego Positions and Measurement Points",
    x = "X Coordinate",
    y = "Y Coordinate",
    color = "Legend",
    shape = "Ego Position"
  ) +
  # Map descriptive labels to PointName
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(raw_plot)

# ---------------- Data reshapping for plotting the averaged data --------------

# Summarize the data by PointName and movementCondition
summary_data <- df %>%
  group_by(PointName, movementCondition) %>%
  summarise(
    X_mean = mean(X),
    Y_mean = mean(Y),
    X_sd = sd(X),
    Y_sd = sd(Y)
  ) %>%
  ungroup()

# Convert ego positions and pointsToMeasure to data frames for plotting
ego_positions_df <- do.call(rbind, lapply(names(ego_positions_around_table), function(name) {
  data.frame(
    movementCondition = name,
    X = ego_positions_around_table[[name]][1],
    Y = ego_positions_around_table[[name]][2]
  )
}))

points_to_measure_df <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X = pointsToMeasure[[name]][1] + 45,  # Adjust for table's center
    Y = pointsToMeasure[[name]][2] + 45
  )
}))

# Add expected positions (measurement points) to summary data
expected_positions <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X_expected = pointsToMeasure[[name]][1] + 45,  # Adjust for table's center
    Y_expected = pointsToMeasure[[name]][2] + 45
  )
}))

# Merge expected positions with summary data
summary_data <- merge(summary_data, expected_positions, by = "PointName")

# Add ego positions to summary data
ego_positions_with_conditions <- ego_positions_df %>%
  rename(Ego_X = X, Ego_Y = Y)  # Rename for clarity

summary_data <- merge(summary_data, ego_positions_with_conditions, by = "movementCondition")



# Define shapes for movementCondition
shape_mapping <- c("inital" = 16,   # Circle
                   "opposite" = 17, # Triangle
                   "adj_ccw" = 18,  # Diamond
                   "adj_cw" = 15)   # Square


# Plot averaged data with error bars, lines, and separate graphs
avg_plot <- ggplot(summary_data, aes(x = X_mean, y = Y_mean, color = movementCondition, shape = movementCondition)) +
  geom_point(size = 4) +  # Averaged points
  geom_errorbar(aes(ymin = Y_mean - Y_sd, ymax = Y_mean + Y_sd), width = 1, size = 0.7) +  # Error bars (Y)
  geom_errorbarh(aes(xmin = X_mean - X_sd, xmax = X_mean + X_sd), height = 1, size = 0.7) +  # Error bars (X)
  geom_segment(aes(x = X_mean, y = Y_mean, xend = X_expected, yend = Y_expected), size = 0.8, linetype = "solid", color = "blue") +  # Line to expected position
  geom_point(data = ego_positions_df, aes(x = X, y = Y), size = 5, color = "black", shape = 3 , color = movementCondition, shape = movementCondition) +  # Ego positions (crosses)
  geom_point(data = points_to_measure_df, aes(x = X, y = Y), size = 3, shape = 4, color = "red") +  # Measurement points (red crosses)
  scale_shape_manual(values = shape_mapping) +
  labs(
    title = "Averaged Data by Movement Condition",
    x = "X (Mean)",
    y = "Y (Mean)",
    color = "Movement Condition (Color)",
    shape = "Movement Condition (Shape)"
  ) +
  facet_wrap(~ movementCondition) +  # Separate graphs for each movement condition
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

# Display the plot
print(avg_plot)



# -------------- Analysis -------------------


## -------------- Euclidean Analysis -------------------
df
# Add expected positions to the data frame for reference
df <- df %>%
  left_join(
    points_to_measure_df %>%
      rename(X_expected = X, Y_expected = Y),  # Rename for clarity
    by = c("PointName")
  )

# Compute Euclidean deviation for each data point
df <- df %>%
  mutate(
    Euclidean_Deviation = sqrt((X - X_expected)^2 + (Y - Y_expected)^2)
  )
# Summarize mean and SD of Euclidean deviations by PointName and movementCondition
deviation_summary <- df %>%
  group_by(PointName, movementCondition) %>%
  summarise(
    Mean_Deviation = mean(Euclidean_Deviation),
    SD_Deviation = sd(Euclidean_Deviation),
    .groups = "drop"
  )

# Set the order of movement conditions
deviation_summary <- deviation_summary %>%
  mutate(movementCondition = factor(
    movementCondition,
    levels = c("inital", "opposite", "adj_cw", "adj_ccw"),
    labels = c("Initial", "Opposite", "Adj CW", "Adj CCW")  # Custom labels
  ))



# Bar plot of deviation to expected position with facet_wrap
facet_plot <- ggplot(deviation_summary, aes(x = movementCondition, y = Mean_Deviation, fill = movementCondition)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar heights represent mean deviations
  geom_errorbar(
    aes(
      ymin = Mean_Deviation - SD_Deviation,  # Lower bound of error
      ymax = Mean_Deviation + SD_Deviation   # Upper bound of error
    ),
    width = 0.2,  # Width of error bar caps
    position = position_dodge(width = 0.9)  # Align with bars
  ) +
  labs(
    title = "Deviation to Expected Position by Movement Condition",
    x = "Movement Condition",
    y = "Mean Euclidean Deviation (± SD)",
    fill = "Movement Condition"
  ) +
  facet_wrap(~ PointName, ncol = 2) +  # Create separate panels for each PointName
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

# Display the plot
print(facet_plot)


# Perform ANOVA on Euclidean deviations
anova_result <- aov(Euclidean_Deviation ~ movementCondition, data = df)
summary(anova_result)




# Pairwise comparisons using Tukey's HSD
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Create a data frame for movementCondition pairs
tukey_df <- as.data.frame(tukey_result$movementCondition) %>%
  mutate(Pair = rownames(tukey_result$movementCondition))

# Prepare data for difference plot
tukey_diff_plot <- tukey_df %>%
  separate(Pair, into = c("Group1", "Group2"), sep = "-") %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Significant", "Not Significant"))

# Difference plot
diff_plot <- ggplot(tukey_diff_plot, aes(x = diff, y = reorder(paste(Group1, "vs", Group2), diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
  labs(
    title = "Pairwise Differences from Tukey's HSD",
    x = "Difference in Mean Euclidean Deviation",
    y = "Comparison"
  ) +
  theme_minimal()

print(diff_plot)




## -------------- Angular Analysis -------------------
# Add ego positions to df
df <- df %>%
  left_join(
    ego_positions_df %>%
      rename(Ego_X = X, Ego_Y = Y),  # Rename columns for clarity
    by = "movementCondition"
  )
df
# Compute angular deviation for each data point
df <- df %>%
  mutate(
    Angle_Actual = atan2(Y - Ego_Y, X - Ego_X),  # Angle of actual position (radians)
    Angle_Expected = atan2(Y_expected - Ego_Y, X_expected - Ego_X),  # Angle of expected position (radians)
    Angular_Deviation = (Angle_Actual - Angle_Expected) %% (2 * pi)  # Angular deviation (wrap to [0, 2π])
  ) %>%
  mutate(
    Angular_Deviation = ifelse(Angular_Deviation > pi, Angular_Deviation - 2 * pi, Angular_Deviation)  # Wrap to [-π, π]
  )



# Summarize mean and SD of angular deviations by PointName and movementCondition
angular_summary <- df %>%
  group_by(PointName, movementCondition) %>%
  summarise(
    Mean_Angular_Deviation = mean(Angular_Deviation),
    SD_Angular_Deviation = sd(Angular_Deviation),
    .groups = "drop"
  )

# Set the order of movement conditions
angular_summary <- angular_summary %>%
  mutate(movementCondition = factor(
    movementCondition,
    levels = c("inital", "opposite", "adj_cw", "adj_ccw"),
    labels = c("Initial", "Opposite", "Adj CW", "Adj CCW")  # Custom labels
  ))



# Bar plot of angular deviation with facet_wrap
angular_facet_plot <- ggplot(angular_summary, aes(x = movementCondition, y = Mean_Angular_Deviation, fill = movementCondition)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar heights represent mean angular deviations
  geom_errorbar(
    aes(
      ymin = Mean_Angular_Deviation - SD_Angular_Deviation,
      ymax = Mean_Angular_Deviation + SD_Angular_Deviation
    ),
    width = 0.2, position = position_dodge(width = 0.9)
  ) +
  labs(
    title = "Angular Deviation to Expected Position by Movement Condition",
    x = "Movement Condition",
    y = "Mean Angular Deviation (Radians ± SD)",
    fill = "Movement Condition"
  ) +
  facet_wrap(~ PointName, ncol = 2) +  # Create separate panels for each PointName
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(angular_facet_plot)




# Perform ANOVA on angular deviations
angular_anova_result <- aov(Angular_Deviation ~ movementCondition, data = df)
summary(angular_anova_result)

# Pairwise comparisons using Tukey's HSD
angular_tukey_result <- TukeyHSD(angular_anova_result)
print(angular_tukey_result)

# Prepare data for pairwise difference plot
angular_tukey_df <- as.data.frame(angular_tukey_result$movementCondition) %>%
  mutate(Pair = rownames(angular_tukey_result$movementCondition)) %>%
  separate(Pair, into = c("Group1", "Group2"), sep = "-") %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Significant", "Not Significant"))

# Pairwise difference plot
angular_diff_plot <- ggplot(angular_tukey_df, aes(x = diff, y = reorder(paste(Group1, "vs", Group2), diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
  labs(
    title = "Pairwise Angular Differences from Tukey's HSD",
    x = "Difference in Mean Angular Deviation (Radians)",
    y = "Comparison"
  ) +
  theme_minimal()

print(angular_diff_plot)





# ---------------------------------

# Compute X and Y deviations
df <- df %>%
  mutate(
    X_Deviation = (X - X_expected),
    Y_Deviation = (Y - Y_expected)
  )
df


# Summarize mean and SD of X and Y deviations
xy_deviation_summary <- df %>%
  group_by(PointName, movementCondition) %>%
  summarise(
    Mean_X_Deviation = mean(X_Deviation),
    SD_X_Deviation = sd(X_Deviation),
    Mean_Y_Deviation = mean(Y_Deviation),
    SD_Y_Deviation = sd(Y_Deviation),
    .groups = "drop"
  )

# Set the order of movement conditions
xy_deviation_summary <- xy_deviation_summary %>%
  mutate(movementCondition = factor(
    movementCondition,
    levels = c("inital", "opposite", "adj_cw", "adj_ccw"),
    labels = c("Initial", "Opposite", "Adj CW", "Adj CCW")  # Custom labels
  ))



# Bar plot for X deviation
x_deviation_plot <- ggplot(xy_deviation_summary, aes(x = movementCondition, y = Mean_X_Deviation, fill = movementCondition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(
      ymin = Mean_X_Deviation - SD_X_Deviation,
      ymax = Mean_X_Deviation + SD_X_Deviation
    ),
    width = 0.2, position = position_dodge(width = 0.9)
  ) +
  labs(
    title = "X Deviation by Movement Condition",
    x = "Movement Condition",
    y = "Mean X Deviation (± SD)",
    fill = "Movement Condition"
  ) +
  facet_wrap(~ PointName, ncol = 2) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(x_deviation_plot)

# Perform ANOVA on X deviations
anova_x <- aov(X_Deviation ~ movementCondition, data = df)
summary(anova_x)

# Tukey's HSD for X deviations
tukey_x <- TukeyHSD(anova_x)
print(tukey_x)


# Prepare data for pairwise difference plot
tukey_x_df <- as.data.frame(tukey_x$movementCondition) %>%
  mutate(Pair = rownames(tukey_x$movementCondition)) %>%
  separate(Pair, into = c("Group1", "Group2"), sep = "-") %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Significant", "Not Significant"))

# Pairwise difference plot for X deviations
x_diff_plot <- ggplot(tukey_x_df, aes(x = diff, y = reorder(paste(Group1, "vs", Group2), diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
  labs(
    title = "Pairwise X Deviation Differences (Tukey's HSD)",
    x = "Difference in Mean X Deviation",
    y = "Comparison"
  ) +
  theme_minimal()

print(x_diff_plot)






# Bar plot for Y deviation
y_deviation_plot <- ggplot(xy_deviation_summary, aes(x = movementCondition, y = Mean_Y_Deviation, fill = movementCondition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(
      ymin = Mean_Y_Deviation - SD_Y_Deviation,
      ymax = Mean_Y_Deviation + SD_Y_Deviation
    ),
    width = 0.2, position = position_dodge(width = 0.9)
  ) +
  labs(
    title = "Y Deviation by Movement Condition",
    x = "Movement Condition",
    y = "Mean Y Deviation (± SD)",
    fill = "Movement Condition"
  ) +
  facet_wrap(~ PointName, ncol = 2) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(y_deviation_plot)

# Perform ANOVA on Y deviations
anova_y <- aov(Y_Deviation ~ movementCondition, data = df)
summary(anova_y)

# Tukey's HSD for Y deviations
tukey_y <- TukeyHSD(anova_y)
print(tukey_y)


# Prepare data for pairwise difference plot
tukey_y_df <- as.data.frame(tukey_y$movementCondition) %>%
  mutate(Pair = rownames(tukey_y$movementCondition)) %>%
  separate(Pair, into = c("Group1", "Group2"), sep = "-") %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Significant", "Not Significant"))

# Pairwise difference plot for Y deviations
y_diff_plot <- ggplot(tukey_y_df, aes(x = diff, y = reorder(paste(Group1, "vs", Group2), diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
  labs(
    title = "Pairwise Y Deviation Differences (Tukey's HSD)",
    x = "Difference in Mean Y Deviation",
    y = "Comparison"
  ) +
  theme_minimal()

print(y_diff_plot)




# ----------------------------------

# Assign motion equivalence groups with semantic names
df <- df %>%
  mutate(Motion_Group = case_when(
    # Simple left motion close
    (movementCondition == "inital" & PointName == "bl") |
      (movementCondition == "opposite" & PointName == "tr") |
      (movementCondition == "adj_cw" & PointName == "tl") |
      (movementCondition == "adj_ccw" & PointName == "br") ~ "left_close",
    
    # Complex right motion close
    (movementCondition == "inital" & PointName == "br") |
      (movementCondition == "opposite" & PointName == "tl") |
      (movementCondition == "adj_cw" & PointName == "bl") |
      (movementCondition == "adj_ccw" & PointName == "bl") ~ "right_close",
    
    # Simple left motion far
    (movementCondition == "inital" & PointName == "tl") |
      (movementCondition == "opposite" & PointName == "bl") |
      (movementCondition == "adj_cw" & PointName == "br") |
      (movementCondition == "adj_ccw" & PointName == "tr") ~ "left_far",
    
    # Complex right motion far
    (movementCondition == "inital" & PointName == "tr") |
      (movementCondition == "opposite" & PointName == "br") |
      (movementCondition == "adj_cw" & PointName == "bl") |
      (movementCondition == "adj_ccw" & PointName == "tl") ~ "right_far",
    
    TRUE ~ "Other"  # Exclude non-relevant data
  ))

# Filter only the groups of interest
motion_data <- df %>%
  filter(Motion_Group != "Other")

# Reorder Motion_Group to the specified order
motion_data <- motion_data %>%
  mutate(Motion_Group = factor(Motion_Group, levels = c("left_close", "right_close", "left_far", "right_far")))
motion_data

# Calculate absolute Euclidean deviation from expected position
motion_data <- motion_data %>%
  mutate(Euclidean_Deviation = sqrt(((abs(X) - abs(X_expected))^2 + abs(abs(Y) - abs(Y_expected) )^2)))

# Compute mean absolute Euclidean deviations
mean_deviation_data <- motion_data %>%
  group_by(Motion_Group) %>%
  summarize(Mean_Deviation = mean(Euclidean_Deviation, na.rm = TRUE))


motion_box_plot <- ggplot(motion_data, aes(x = Motion_Group, y = Euclidean_Deviation, fill = Motion_Group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Box Plot of Mean Absolute Euclidean Deviations by Motion Group",
    x = "Motion Group",
    y = "Mean Absolute Euclidean Deviation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(motion_box_plot)


motion_violin_plot <- ggplot(motion_data, aes(x = Motion_Group, y = Euclidean_Deviation, fill = Motion_Group)) +
  geom_violin(trim = FALSE, alpha = 0.7) + # Show full range of data
  geom_boxplot(width = 0.1, alpha = 0.5, color = "black") + # Add a box plot within the violin
  labs(
    title = "Violin Plot of Mean Absolute Euclidean Deviations by Motion Group",
    x = "Motion Group",
    y = "Mean Absolute Euclidean Deviation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(motion_violin_plot)




# ANOVA for mean absolute Euclidean deviations
anova_motion <- aov(Euclidean_Deviation ~ Motion_Group, data = motion_data)
summary(anova_motion)

# Tukey's HSD for pairwise group comparisons
tukey_motion <- TukeyHSD(anova_motion)
print(tukey_motion)

# Convert Tukey's results into a data frame
tukey_group_df <- as.data.frame(tukey_motion$Motion_Group) %>%
  mutate(Pair = rownames(tukey_motion$Motion_Group)) %>%
  separate(Pair, into = c("Group1", "Group2"), sep = "-") %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Significant", "Not Significant"))

# Create the pairwise differences plot
pairwise_plot <- ggplot(tukey_group_df, aes(x = diff, y = reorder(paste(Group1, "vs", Group2), diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
  labs(
    title = "Pairwise Differences in Mean Absolute Euclidean Deviations",
    x = "Difference in Mean Absolute Euclidean Deviation",
    y = "Group Comparison",
    color = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

print(pairwise_plot)



group_means <- motion_data %>%
  group_by(Motion_Group) %>%
  summarize(
    Mean_Deviation = mean(Euclidean_Deviation, na.rm = TRUE),
    SD_Deviation = sd(Euclidean_Deviation, na.rm = TRUE)
  )
print(group_means)


pairwise_t_test <- pairwise.t.test(
  motion_data$Euclidean_Deviation, 
  motion_data$Motion_Group, 
  p.adjust.method = "bonferroni"
)

print(pairwise_t_test)

