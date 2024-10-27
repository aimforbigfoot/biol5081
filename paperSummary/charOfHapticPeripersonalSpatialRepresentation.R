
library(ggplot2)
library(dplyr)
set.seed( rnorm(n = 1, mean = 0, sd = 2000   ) )

# Participant IDs and their handedness
participantCount <- 1:6
participant_tags <- data.frame(
  ID = participantCount,
  hand = rep(c("left-handed", "right-handed"), each = 3)  # Assuming 3 left-handed and 3 right-handed participants
)

# Define reference points for different spatial positions
base_points <- data.frame(
  ref_x = c(0, 0, 10, -10, 10, -10, 14.2, -14.2),  # Base reference x-coordinates
  ref_y = c(15, 45, 20, 20, 40, 40, 30, 30)  # Base reference y-coordinates
  # ref_x and ref_y values represent different spatial positions:
  # (0, 15) - Front
  # (0, 45) - Front (further)
  # (10, 20) - Oblique right
  # (-10, 20) - Oblique left
  # (10, 40) - Right
  # (-10, 40) - Left
  # (14.2, 30) - Extreme right oblique
  # (-14.2, 30) - Extreme left oblique
)


# Sample size
N <- length(participantCount)
fac <- 3
bigFac <- 3

# Calculating mean adjustments for right-handed participants
right_handed_means <- list(
  front = data.frame(
    x = c(rep(0.29 * sqrt(N), 2), rep(0.36 * sqrt(N), 6)),
    y = c(rep(0.57 * sqrt(N), 2), rep(0.43 * sqrt(N), 6))
  ),
  right = data.frame(
    x = c(rep(0.31 * sqrt(N) * fac, 2), rep(0.38 * sqrt(N) * fac, 6)),
    y = c(rep(0.52 * sqrt(N) * fac, 2), rep(0.47 * sqrt(N) * fac, 6))
  )
)
# right_handed_means$front generated

right_handed_sds <- list(
  front = data.frame(
    x = c(rep(0.19 * sqrt(N ) * fac , 2), rep(0.18 * sqrt(N) * fac, 6)),
    y = c(rep(0.3 * sqrt(N)   * fac,  2), rep(0.14 * sqrt(N) * fac, 6))
  ),
  right = data.frame(
    x = c(rep(0.21 * sqrt(N) * fac, 2), rep(0.19 * sqrt(N) * fac, 6)),
    y = c(rep(0.28 * sqrt(N) * fac, 2), rep(0.16 * sqrt(N) * fac, 6))
  )
)
# right_handed_sds$front generated

# Mean and SE adjustments for left-handed participants
# Calculating mean adjustments for left-handed participants
left_handed_means <- list(
  front = data.frame(
    x = c(rep(0.21 * sqrt(N)* bigFac, 2), rep(0.25 * sqrt(N) * bigFac, 6)),
    y = c(rep(0.45 * sqrt(N)* bigFac, 2), rep(0.35 * sqrt(N) * bigFac, 6))
  ),
  left = data.frame(
    x = c(rep(0.23 * sqrt(N)* bigFac, 2), rep(0.28 * sqrt(N)* bigFac, 6)),
    y = c(rep(0.4 * sqrt(N) * bigFac, 2), rep(0.33 * sqrt(N)* bigFac, 6))
  )
)
# left_handed_means$front generated

left_handed_sds <- list(
  front = data.frame(
    x = c(rep(0.15 * sqrt(N)* bigFac, 2), rep(0.17 * sqrt(N)* bigFac, 6)),
    y = c(rep(0.25 * sqrt(N)* bigFac, 2), rep(0.2 * sqrt(N)* bigFac, 6))
  ),
  left = data.frame(
    x = c(rep(0.2 * sqrt(N)* bigFac, 2), rep(0.22 * sqrt(N)* bigFac, 6)),
    y = c(rep(0.3 * sqrt(N)* bigFac, 2), rep(0.27 * sqrt(N)* bigFac, 6))
  )
)
# left_handed_sds$front generated




# Initialize an empty data frame to accumulate rows
my_data <- data.frame(
  ID = integer(),
  subjectX = numeric(),
  subjectY = numeric(),
  ref_x = numeric(),
  ref_y = numeric(),
  hand = character(),
  space = character()
)

# Set number of repetitions for each spatial position
reps_front <- 2  # Number of repetitions for front space
reps_side <- 3   # Number of repetitions for side spaces
numOfPoints <- nrow(base_points)

# Loop over participants
for (i in participantCount) {
  current_hand <- participant_tags$hand[i]
  cat("Processing participant ID:", i, "Handedness:", current_hand, "\n")
  
  # Select the appropriate mean and sd adjustments based on handedness
  if (current_hand == "right-handed") {
    means <- right_handed_means
    sds <- right_handed_sds
    side_space <- "right"
    side_offset <- 45  # Offset for right space
  } else {
    means <- left_handed_means
    sds <- left_handed_sds
    side_space <- "left"
    side_offset <- -45  # Offset for left space
  }
  
  # Loop over reference points
  for (j in 1:numOfPoints) {
    cat("  Processing reference point index:", j, "\n")
    
    # Front space repetitions
    for (k in 1:reps_front) {
      cat("    Adding front space repetition:", k, "\n")
      # Generate random adjustments
      deltaX <- rnorm(1, mean = means$front$x[j], sd = sds$front$x[j])
      deltaY <- rnorm(1, mean = means$front$y[j], sd = sds$front$y[j])
      # Add base reference points
      subjectX <- base_points$ref_x[j] + deltaX
      subjectY <- base_points$ref_y[j] + deltaY
      
      rowToAdd <- data.frame(
        ID = i,
        subjectX = subjectX,
        subjectY = subjectY,
        ref_x = base_points$ref_x[j],
        ref_y = base_points$ref_y[j],
        hand = current_hand,
        space = "front"
      )
      my_data <- rbind(my_data, rowToAdd)
    }
    
    # Side space repetitions
    for (k in 1:reps_side) {
      cat("    Adding", side_space, "space repetition:", k, "\n")
      # Generate random adjustments
      deltaX <- rnorm(1, mean = means[[side_space]]$x[j], sd = sds[[side_space]]$x[j])
      deltaY <- rnorm(1, mean = means[[side_space]]$y[j], sd = sds[[side_space]]$y[j])
      # Add base reference points and side offset
      subjectX <- base_points$ref_x[j] + side_offset + deltaX
      subjectY <- base_points$ref_y[j] + deltaY
      
      rowToAdd <- data.frame(
        ID = i,
        subjectX = subjectX,
        subjectY = subjectY,
        ref_x = base_points$ref_x[j] + side_offset,
        ref_y = base_points$ref_y[j],
        hand = current_hand,
        space = side_space
      )
      my_data <- rbind(my_data, rowToAdd)
    }
  }
}


# Display the final data frame
print("Final data frame:")
print(head(my_data))

# Write the final data frame to a CSV file
# write.csv(my_data, file = "my_data.csv", row.names = FALSE)

my_data_left <- my_data %>% filter(hand == "left-handed")
my_data_right <- my_data %>% filter(hand == "right-handed")
# Plot for left-handed participants
print("Creating plot for left-handed participants")
ggplot(my_data_left, aes(x = subjectX, y = subjectY)) +
  geom_point(aes(color = space), size = 3, shape = 4) +
  # Plot base reference points
  geom_point(
    data = base_points,
    aes(x = ref_x, y = ref_y),
    color = "black",
    size = 2,
    shape = 21,
    fill = "white",
    alpha = 0.8
  ) +
  # Plot side space reference points (adjusted for side_offset)
  geom_point(
    data = transform(base_points, ref_x = ref_x + (-45)),
    aes(x = ref_x, y = ref_y),
    color = "black",
    size = 2,
    shape = 21,
    fill = "white",
    alpha = 0.8
  ) +
  labs(
    x = "Subject X",
    y = "Subject Y",
    title = "Data Points for Left-Handed Participants",
    color = "Space"
  ) +
  theme_minimal() +
  xlim(-70, 20) +
  ylim(0, 50)

# Plot for right-handed participants
print("Creating plot for right-handed participants")
ggplot(my_data_right, aes(x = subjectX, y = subjectY)) +
  geom_point(aes(color = space), size = 3, shape = 4) +
  # Plot base reference points
  geom_point(
    data = base_points,
    aes(x = ref_x, y = ref_y),
    color = "black",
    size = 2,
    shape = 21,
    fill = "white",
    alpha = 0.8
  ) +
  # Plot side space reference points (adjusted for side_offset)
  geom_point(
    data = transform(base_points, ref_x = ref_x + 45),
    aes(x = ref_x, y = ref_y),
    color = "black",
    size = 2,
    shape = 21,
    fill = "white",
    alpha = 0.8
  ) +
  labs(
    x = "Subject X",
    y = "Subject Y",
    title = "Data Points for Right-Handed Participants",
    color = "Space"
  ) +
  theme_minimal() +
  xlim(-20, 70) +
  ylim(0, 50)



# Load necessary libraries
# Calculate deviation from the reference point for each data point
my_data <- my_data %>%
  mutate(
    deviationX = abs(subjectX - ref_x),
    deviationY = abs(subjectY - ref_y)
  )

# Calculate the mean deviation for each group (hand, space) for both X and Y axes
summary_deviation <- my_data %>%
  group_by(hand, space) %>%
  summarize(
    mean_deviationX = mean(deviationX, na.rm = TRUE),
    mean_deviationY = mean(deviationY, na.rm = TRUE),
    sd_deviationX = sd(deviationX, na.rm = TRUE),
    sd_deviationY = sd(deviationY, na.rm = TRUE)
  )

# Display the summary data
print(summary_deviation)


# Create a bar plot with error bars for mean deviations
ggplot(summary_deviation, aes(x = interaction(hand, space), y = mean_deviationX, fill = hand)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = mean_deviationX - sd_deviationX, ymax = mean_deviationX + sd_deviationX),
                width = 0.2, position = position_dodge(0.6)) +
  labs(
    x = "Handedness and Space",
    y = "Mean Deviation (X Axis)",
    title = "Mean Deviation from Reference Point for X Axis",
    fill = "Handedness"
  ) +
  theme_minimal()

# If you also want to plot for the Y axis deviations
ggplot(summary_deviation, aes(x = interaction(hand, space), y = mean_deviationY, fill = hand)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = mean_deviationY - sd_deviationY, ymax = mean_deviationY + sd_deviationY),
                width = 0.2, position = position_dodge(0.6)) +
  labs(
    x = "Handedness and Space",
    y = "Mean Deviation (Y Axis)",
    title = "Mean Deviation from Reference Point for Y Axis",
    fill = "Handedness"
  ) +
  theme_minimal()
