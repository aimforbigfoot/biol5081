# ---------- Data Generation Setup ------------
##  -------------------- Library Importing --------------- 
# Load required 
library(ggplot2)
library(gridExtra)  # For side-by-side plots
library(dplyr)
library(tidyr)


##  -------------------- Parameters --------------- 
numOfPeople <- 16
numOfRepeats <- 4
noise_sd <- 4


old_ego_bias_strength_inital <- 0.1
old_ego_bias_strength_opposite <- 0.1
old_ego_bias_strength_adj_cw <- 0.05
old_ego_bias_strength_adj_ccw <- 0.05

# new_ego_bias_strength_inital <- 0.0 -> this doesnt count cuz they dont move, no new pos
new_ego_bias_strength_opposite <- 0.2
new_ego_bias_strength_adj_cw <- 0.1
new_ego_bias_strength_adj_ccw<- 0.1

allo_bias_strength_inital <- 0.1
allo_bias_strength_opposite <- 0.1
allo_bias_strength_adj_cw <- 0.2
allo_bias_strength_adj_ccw <- 0.2

# table is about 90 cm by 90 so lets say the center of the table is 0,0
# Specified ego positions

ego_positions_around_table <- list(
  inital = c(45, 0),
  opposite = c(45, 90), # these 2 are top and bottom ofthe table 
  adj_ccw = c(90, 45),
  adj_cw = c(0, 45) # middle of the table, left or right 
)

# Specified points - directional meanings from inital point of view
pointsToMeasure <- list(
  bl = c(-30, -30),
  br = c(30, -30),
  tl = c(-30, 30),
  tr = c(30, 30)
)


##  -------------------- Generator ------------

# Initialize an empty data frame
df <- data.frame(
  Person = integer(),
  PointName = character(),
  moveCondition = character(),
  Repeat = integer(),
  X = numeric(),
  Y = numeric()
)

# Loop over the number of people
for (person in 1:numOfPeople) {
  # Loop through each movement condition
  for (moveCondition in names(ego_positions_around_table)) {
    
    initalEgoPos <- ego_positions_around_table[["inital"]]
    finalEgoPos <- initalEgoPos
    # Define biases specific to the current condition
    old_ego_bias_strength <- 0.0
    new_ego_bias_factor <- 0.0
    allocentric_bias_factor <- 0.0
    
    
    ego_position <- ego_positions_around_table[["inital"]]  # Default
    
    if (moveCondition == "opposite") {
      finalEgoPos <- ego_positions_around_table[["opposite"]]
      old_ego_bias_strength <- old_ego_bias_strength_opposite
      new_ego_bias_factor <- new_ego_bias_strength_opposite
      allocentric_bias_factor <- allo_bias_strength_opposite
    } else if (moveCondition == "adj_ccw") {
      finalEgoPos <- ego_positions_around_table[["adj_ccw"]]
      old_ego_bias_strength <- old_ego_bias_strength_adj_ccw
      new_ego_bias_factor <- new_ego_bias_strength_adj_ccw
      allocentric_bias_factor <- allo_bias_strength_adj_ccw
    } else if (moveCondition == "adj_cw") {
      finalEgoPos <- ego_positions_around_table[["adj_cw"]]
      old_ego_bias_strength <- old_ego_bias_strength_adj_cw
      new_ego_bias_factor <- new_ego_bias_strength_adj_cw
      allocentric_bias_factor <- allo_bias_strength_adj_cw
    } else { # inital params 
      old_ego_bias_strength <- old_ego_bias_strength_inital
      allocentric_bias_factor <- allo_bias_strength_inital
    }
    
    # Loop through each point in the points list
    for (point_name in names(pointsToMeasure)) {
      # Loop each point for the number of repeats
      for (i in 1:numOfRepeats) {
        
        
        
        
        
        # Extract the point's coordinates
        point_coords <- pointsToMeasure[[point_name]] + c(45,45)
        actualPosition <- point_coords
        # Add noise
        point_coords <- point_coords + rnorm(n = 2, mean = 0, sd = noise_sd)
        
        # Apply biases
        # An inital position Ego bias, same for all types 
        point_coords <- point_coords + old_ego_bias_strength * (ego_position - actualPosition)
        # A final ego pos bias, diff for opposite, adj_cw, adj_ccw
        if (moveCondition != "inital"){
        point_coords <- point_coords + new_ego_bias_factor * (finalEgoPos - actualPosition)
          
        }
        
        # Allocentric bias (toward center 0, 0)
        point_coords <- point_coords + allocentric_bias_factor * (c(45, 45) - actualPosition)
        
        # Add the data to the data frame
        df <- rbind(df, data.frame(
          Person = person,
          PointName = point_name,
          moveCondition = moveCondition,
          Repeat = i,
          X = point_coords[1],
          Y = point_coords[2]
        ))
      }
    }
  }
}

# View the resulting data frame
print(df)


# ----------- Plotting --------------
##  -------------------- Raw data --------------


# Convert ego_positions_around_table into a data frame for plotting
ego_positions_df <- do.call(rbind, lapply(names(ego_positions_around_table), function(name) {
  data.frame(
    moveConditionName = name,
    X = ego_positions_around_table[[name]][1] ,
    Y = ego_positions_around_table[[name]][2]
  )
}))

points_to_measure_df <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X = pointsToMeasure[[name]][1] + 45,  # Shift to align with the coordinate system
    Y = pointsToMeasure[[name]][2] + 45
  )
}))


# Plot raw data with ego positions and pointsToMeasure
raw_plot <- ggplot(df, aes(x = X, y = Y, color = moveCondition)) +
  geom_point(size = 3) +  # Raw data points
  geom_point(data = ego_positions_df, aes(x = X, y = Y, shape = moveConditionName), size = 4, color = "black") +  # Ego positions
  geom_point(data = points_to_measure_df, aes(x = X, y = Y), size = 4, stroke = 4, shape = 4, color = "red") +  # pointsToMeasure markers
  labs(title = "Raw Data with Ego Positions and Measurement Points", x = "X", y = "Y") +
  theme_minimal()

# Display the plot
print(raw_plot)





##  -------------------- Averaged Data On Seperated Plots -------------
summary_data <- df %>%
  group_by(PointName, moveCondition) %>%
  summarise(
    X_mean = mean(X),
    Y_mean = mean(Y),
    X_sd = sd(X),
    Y_sd = sd(Y)
  ) %>%
  ungroup()

# Add actual expected positions
expected_positions <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X_expected = pointsToMeasure[[name]][1] + 45,  # Adjust for the coordinate shift
    Y_expected = pointsToMeasure[[name]][2] + 45
  )
}))

# Merge expected positions into summary data
summary_data <- merge(summary_data, expected_positions, by = "PointName")
# Add ego positions to the summary data
ego_positions_with_conditions <- ego_positions_df %>%
  rename(Ego_X = X, Ego_Y = Y)  # Rename columns for clarity

# Merge ego positions with the summary data
summary_data <- merge(summary_data, ego_positions_with_conditions,
                      by.x = "moveCondition", by.y = "moveConditionName")
summary_data

# Define the shapes for each moveCondition
shape_mapping <- c("inital" = 16,   # Circle
                   "opposite" = 17, # Triangle
                   "adj_ccw" = 18,  # Diamond
                   "adj_cw" = 15)   # Square

spread_plot <- ggplot(summary_data, aes(x = X_mean, y = Y_mean, color = moveCondition, shape = moveCondition)) +
  geom_segment(aes(x = Ego_X, y = Ego_Y, xend = X_mean, yend = Y_mean, color = moveCondition), size = 0.8) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Y_mean - Y_sd, ymax = Y_mean + Y_sd), width = 2, size = 0.7) +
  geom_errorbarh(aes(xmin = X_mean - X_sd, xmax = X_mean + X_sd), height = 2, size = 0.7) +
  geom_point(data = ego_positions_df, aes(x = X, y = Y, shape = moveConditionName), size = 4, color = "black") +
  geom_point(data = points_to_measure_df, aes(x = X, y = Y), size = 4, shape = 4, color = "red") +
  scale_shape_manual(values = shape_mapping) +
  labs(
    title = "Comparison of Averaged Data Across Movement Conditions",
    x = "X (Mean)", y = "Y (Mean)",
    shape = "Movement Condition"
  ) +
  theme_minimal() +
  facet_wrap(~ moveCondition)  # This will create separate panels for each condition

print(spread_plot)


# --------- Analysis -----------

# 1. Join df with expected positions
expected_positions <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X_expected = pointsToMeasure[[name]][1] + 45,
    Y_expected = pointsToMeasure[[name]][2] + 45
  )
}))

df_full <- merge(df, expected_positions, by = "PointName")

# 2. Compute deviations per replicate
df_full <- df_full %>%
  mutate(
    X_dev = X - X_expected,
    Y_dev = Y - Y_expected,
    Euclidian_dev = sqrt((X - X_expected)^2 + (Y - Y_expected)^2),
    angle_expected = atan2(Y_expected - 45, X_expected - 45),
    angle_observed = atan2(Y - 45, X - 45),
    angle_dev = (angle_observed - angle_expected) * (180 / pi)
  )

# 3. Summarize the data for each deviation type
summary_devs <- df_full %>%
  group_by(PointName, moveCondition) %>%
  summarise(
    X_dev_mean = mean(X_dev),
    X_dev_sd = sd(X_dev),
    Y_dev_mean = mean(Y_dev),
    Y_dev_sd = sd(Y_dev),
    Euclidian_dev_mean = mean(Euclidian_dev),
    Euclidian_dev_sd = sd(Euclidian_dev),
    angle_dev_mean = mean(angle_dev),
    angle_dev_sd = sd(angle_dev)
  ) %>% ungroup()

# 4. Set the factor order for moveCondition
summary_devs$moveCondition <- factor(summary_devs$moveCondition, 
                                     levels = c("inital", "opposite", "adj_cw", "adj_ccw"))




## --------- comparing each axis --------- 

# Euclidean deviations with error bars
euclidian_dev_plot <- ggplot(summary_devs, aes(x = PointName, y = Euclidian_dev_mean, fill = moveCondition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Euclidian_dev_mean - Euclidian_dev_sd, ymax = Euclidian_dev_mean + Euclidian_dev_sd),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Euclidean Deviations from Expected Positions",
       x = "Point", y = "Euclidean Deviation (cm)") +
  theme_minimal() +
  facet_wrap(~ moveCondition)
euclidian_dev_plot

## --------- comparing distance plotsrelatively --------- 
# 
euclidian_plot <- ggplot(summary_data, aes(x = PointName, y = Euclidian_dev, fill = moveCondition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Euclidean Deviations from Expected Positions",
       x = "Point",
       y = "Euclidean Deviation (cm)") +
  theme_minimal()

print(euclidian_plot)

# 
# -------------- comparing angles

# Angular deviations with error bars
angle_dev_plot <- ggplot(summary_devs, aes(x = PointName, y = angle_dev_mean, fill = moveCondition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = angle_dev_mean - angle_dev_sd, ymax = angle_dev_mean + angle_dev_sd),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Angular Deviations from Expected Positions",
       x = "Point", y = "Angular Deviation (degrees)") +
  theme_minimal() +
  facet_wrap(~ moveCondition)

print(angle_plot)

# 
# 
# 
# 
# 
# 
# 
# ----------- the difference one 


# Extract 'inital' condition data
inital_data <- summary_devs %>%
  filter(moveCondition == "inital") %>%
  select(PointName, Euclidian_dev_mean_inital = Euclidian_dev_mean, 
         Euclidian_dev_sd_inital = Euclidian_dev_sd)

# Join the inital data to get baseline values for each PointName
comparison_data <- summary_devs %>%
  filter(moveCondition != "inital") %>%
  left_join(inital_data, by = "PointName")

# Compute differences
# Difference: condition mean - inital mean
# Approximate SD for the difference using sqrt of sum of variances (assuming independence)
comparison_data <- comparison_data %>%
  mutate(
    Euclidian_diff_mean = Euclidian_dev_mean - Euclidian_dev_mean_inital,
    Euclidian_diff_sd = sqrt(Euclidian_dev_sd^2 + Euclidian_dev_sd_inital^2)
  )

# Create a difference plot
# This will show how much each condition differs from the inital condition in terms of Euclidian deviation
diff_plot <- ggplot(comparison_data, aes(x = PointName, y = Euclidian_diff_mean, fill = moveCondition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Euclidian_diff_mean - Euclidian_diff_sd, 
                    ymax = Euclidian_diff_mean + Euclidian_diff_sd),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = "Difference in Euclidean Deviations Relative to 'inital' Condition",
    x = "Point",
    y = "Difference in Euclidean Deviation (cm)",
    fill = "Condition"
  ) +
  theme_minimal()

print(diff_plot)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# Add actual positions to the main data frame
actual_positions_df <- do.call(rbind, lapply(names(pointsToMeasure), function(name) {
  data.frame(
    PointName = name,
    X_actual = pointsToMeasure[[name]][1] + 45,
    Y_actual = pointsToMeasure[[name]][2] + 45
  )
}))

df <- merge(df, actual_positions_df, by = "PointName")




df <- df %>%
  mutate(
    Deviation = sqrt((X - X_actual)^2 + (Y - Y_actual)^2)
  )



deviation_summary <- df %>%
  group_by(PointName, moveCondition) %>%
  summarise(
    avg_Deviation = mean(Deviation, na.rm = TRUE),
    sd_Deviation = sd(Deviation, na.rm = TRUE)
  )



deviation_summary <- deviation_summary %>%
  mutate(moveCondition = factor(moveCondition, levels = c("inital", "opposite", "adj_cw", "adj_ccw")))


ggplot(deviation_summary, aes(x = PointName, y = avg_Deviation, fill = moveCondition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = avg_Deviation - sd_Deviation, ymax = avg_Deviation + sd_Deviation),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    title = "Average Euclidean Deviation by PointName and MoveCondition",
    x = "Point Name",
    y = "Average Deviation",
    fill = "Move Condition"
  ) +
  theme_minimal()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# summary_df <- df %>%
#   group_by(PointName, moveCondition) %>%
#   summarise(
#     avg_X = mean(X, na.rm = TRUE),
#     sd_X = sd(X, na.rm = TRUE),
#     avg_Y = mean(Y, na.rm = TRUE),
#     sd_Y = sd(Y, na.rm = TRUE)
#   )
# 
# print(summary_df)
# 
# long_summary_df <- summary_df %>%
#   pivot_longer(
#     cols = c(avg_X, avg_Y, sd_X, sd_Y),
#     names_to = c(".value", "Axis"),
#     names_pattern = "(.+)_(X|Y)"
#   )
# 
# 
# ggplot(long_summary_df, aes(x = PointName, y = avg, fill = Axis)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   geom_errorbar(
#     aes(ymin = avg - sd, ymax = avg + sd),
#     position = position_dodge(width = 0.9),
#     width = 0.2
#   ) +
#   facet_wrap(~moveCondition) +
#   labs(
#     title = "Average X and Y with SD for PointName and moveCondition",
#     x = "Point Name",
#     y = "Average Value",
#     fill = "Axis"
#   ) +
#   theme_minimal()
