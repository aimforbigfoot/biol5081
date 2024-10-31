

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # for pivot_longer()

set.seed( rnorm(n = 1, mean = 0, sd = 2000   ) )

# Participant IDs and their handedness
participantCount <- 1:20
participant_tags <- data.frame(
  ID = participantCount,
  hand = rep(c("left-handed", "right-handed"), each = 10)  # Assuming 3 left-handed and 3 right-handed participants
)

# Define reference points for different spatial positions
base_points <- data.frame(
  ref_x = c(0, 0, 14.2, -14.2, -10, 10, 10, -10),  # Base reference x-coordinates
  ref_y = c(15, 45,  30, 30, 20, 20, 40, 40)  # Base reference y-coordinates
  # ref_x and ref_y values represent different spatial positions:
  # (0, 15) - Front     # (0, 45) - Front (further)     # (14.2, 30) - Extreme right oblique     # (-14.2, 30) - Extreme left oblique
  # (10, 20) - Oblique right close     # (-10, 20) - Oblique left close     # (10, 40) - Oblique right far     # (-10, 40) - Oblique left far 
)

# Set number of repetitions for each spatial position
reps_front <- 2  # Number of repetitions for front space
reps_side <- 3  # Number of repetitions for side spaces
numOfPoints <- length(base_points$ref_x)

# Sample size
N <- length(participantCount)

right_handed_means <- list(
  front = data.frame(
    x = c( rep(1.29, 4), rep(1.36, 4)),
    y = c( rep(1.57, 4 ), rep(1.43, 4))
  ),
  right = data.frame(
    x = c(rep(1.92 , 4), rep(2.04 , 4)),
    y = c(rep(1.91 , 4), rep(1.94 , 4))
  )
)
right_handed_sds <- list(
  front = data.frame(
    x = c(rep(0.19 * sqrt(reps_front) , 4), rep(0.18* sqrt(reps_front) , 4)),
    y = c(rep(0.3* sqrt(reps_front)   , 4), rep(0.19* sqrt(reps_front) , 4))
  ),
  right = data.frame(
    x = c(rep(0.27* sqrt(reps_side) , 4), rep(0.18* sqrt(reps_side) , 4)),
    y = c(rep(0.28* sqrt(reps_side) , 4), rep(0.19* sqrt(reps_side) , 4))
  )
)



# Mean and SE adjustments for left-handed participants
# Calculating mean adjustments for left-handed participants
left_handed_means <- list(
  front = data.frame(
    x = c(rep(1.82, 4), rep(1.59, 4)),
    y = c(rep(1.34, 4), rep(1.67, 4))
  ),
  left = data.frame(
    x = c(rep(2.14 , 4), rep(1.82 , 4)),
    y = c(rep(1.64 , 4), rep(2.1 , 4))
  )
)

left_handed_sds <- list(
  front = data.frame(
    x = c(rep(0.24* sqrt(reps_front)   , 4), rep(0.24* sqrt(reps_front) , 4)),
    y = c(rep(0.13* sqrt(reps_front)   , 4), rep(0.14* sqrt(reps_front) , 4))
  ),
  left = data.frame(
    x = c(rep(0.24* sqrt(reps_side) , 4), rep(0.21* sqrt(reps_side) , 4)),
    y = c(rep(0.13* sqrt(reps_side) , 4), rep(0.31* sqrt(reps_side) , 4))
  )
)




# Define angular bin ranges
angle_bins <- list(
  "0-90°" = c(0, 90),
  "90-180°" = c(90, 180),
  "180-270°" = c(180, 270),
  "270-360°" = c(270, 360)
)

# Create the frequency data as a data frame
freq_data <- data.frame(
  hand = c(rep("left-handed", 2), rep("right-handed", 2)),
  space = c("left", "front", "right", "front"),
  X0t90 = c(10, 17, 16, 23),
  X90t180 = c(0, 16, 8, 17),
  X180t270 = c(53, 20, 8, 14),
  X270t360 = c(9, 19, 39, 18)
)
freq_data

# Compute the total counts for each row
total_counts <- rowSums(freq_data[, 3:6])

# Normalize the frequency data
normalized_data <- freq_data
normalized_data[, 3:6] <- sweep(freq_data[, 3:6], 1, total_counts, FUN = "/")

# Extract each row into contextually named variables
left_hand_left_space_freq_of_angles <- as.numeric(normalized_data[1, 3:6])
left_hand_front_space_freq_of_angles <- as.numeric(normalized_data[2, 3:6])
right_hand_right_space_freq_of_angles <- as.numeric(normalized_data[3, 3:6])
right_hand_front_space_freq_of_angles <- as.numeric(normalized_data[4, 3:6])


# Function to randomly sample an angle from a given range
sample_angle <- function(bin) {
  range <- angle_bins[[bin]]
  runif(1, min = range[1], max = range[2])  # Random value within the bin range
}



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



# Function to apply rotation transformation to (x, y) based on angle in degrees
rotate_point <- function(x, y, angle) {
  theta <- angle * (pi / 180)  # Convert degrees to radians
  x_new <- x * cos(theta) - y * sin(theta)
  y_new <- x * sin(theta) + y * cos(theta)
  return(c(x_new, y_new))
}

# Data generation loop with rotation transformation
for (i in participantCount) {
  current_hand <- participant_tags$hand[i]
  if (current_hand == "right-handed") {
    means <- right_handed_means
    sds <- right_handed_sds
    side_space <- "right"
    side_offset <- 45
    front_angle_probs <- right_hand_front_space_freq_of_angles
    side_angle_probs <- right_hand_right_space_freq_of_angles
  } else {
    means <- left_handed_means
    sds <- left_handed_sds
    side_space <- "left"
    side_offset <- -45
    front_angle_probs <- left_hand_front_space_freq_of_angles
    side_angle_probs <- left_hand_left_space_freq_of_angles
  }
  
  for (j in 1:numOfPoints) {
    for (k in 1:reps_front) {
      angle_bin <- sample(names(angle_bins), 1, prob = front_angle_probs)
      angle_value <- sample_angle(angle_bin)
      
      # Generate random adjustments
      # Corrected code
      meanX <- runif(1, 0, means$front$x[j])
      deltaX <- rnorm(1,meanX , sds$front$x[j])
      meanY <- runif(1,0,means$front$y[j] )
      deltaY <- rnorm(1, meanY, sds$front$y[j])
      
      
      # Apply rotation to the (deltaX, deltaY)
      rotated_coords <- rotate_point(deltaX, deltaY, angle_value)
      
      subjectX <- base_points$ref_x[j] + rotated_coords[1]
      subjectY <- base_points$ref_y[j] + rotated_coords[2]
      
      rowToAdd <- data.frame(
        ID = i,
        subjectX = subjectX,
        subjectY = subjectY,
        ref_x = base_points$ref_x[j],
        ref_y = base_points$ref_y[j],
        hand = current_hand,
        space = "front",
        angle_bin = angle_bin,
        angle_value = angle_value
      )
      my_data <- rbind(my_data, rowToAdd)
    }
    
    for (k in 1:reps_side) {
      angle_bin <- sample(names(angle_bins), 1, prob = side_angle_probs)
      angle_value <- sample_angle(angle_bin)
      
      # Corrected code
      deltaX <- rnorm(1, means[[side_space]]$x[j], sds[[side_space]]$x[j])
      deltaY <- rnorm(1, means[[side_space]]$y[j], sds[[side_space]]$y[j])
      
      
      # Apply rotation to the (deltaX, deltaY)
      rotated_coords <- rotate_point(deltaX, deltaY, angle_value)
      
      subjectX <- base_points$ref_x[j] + side_offset + rotated_coords[1]
      subjectY <- base_points$ref_y[j] + rotated_coords[2]
      
      rowToAdd <- data.frame(
        ID = i,
        subjectX = subjectX,
        subjectY = subjectY,
        ref_x = base_points$ref_x[j] + side_offset,
        ref_y = base_points$ref_y[j],
        hand = current_hand,
        space = side_space,
        angle_bin = angle_bin,
        angle_value = angle_value
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


















# 
# 
# 
my_data_left <- my_data %>% filter(hand == "left-handed")
my_data_right <- my_data %>% filter(hand == "right-handed")
# # Plot for left-handed participants
# 
# print("Creating plot for left-handed participants")
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
# # 
# #   # Plot for right-handed participants
# # print("Creating plot for right-handed participants")
# ggplot(my_data_right, aes(x = subjectX, y = subjectY)) +
#   geom_point(aes(color = space), size = 3, shape = 4) +
#   # Plot base reference points
#   geom_point(
#     data = base_points,
#     aes(x = ref_x, y = ref_y),
#     color = "black",
#     size = 2,
#     shape = 21,
#     fill = "white",
#     alpha = 0.8
#   ) +
#   # Plot side space reference points (adjusted for side_offset)
#   geom_point(
#     data = transform(base_points, ref_x = ref_x + 45),
#     aes(x = ref_x, y = ref_y),
#     color = "black",
#     size = 2,
#     shape = 21,
#     fill = "white",
#     alpha = 0.8
#   ) +
#   labs(
#     x = "Subject X",
#     y = "Subject Y",
#     title = "Data Points for Right-Handed Participants",
#     color = "Space"
#   ) +
#   theme_minimal() +
#   xlim(-20, 70) +
#   ylim(0, 50)
# # 











# 
# 
# my_data
# # Calculate absolute X and Y deviations
my_data$x_deviation <- abs(my_data$subjectX - my_data$ref_x)
my_data$y_deviation <- abs(my_data$subjectY - my_data$ref_y)
# 
# 
# 
# # Reshape the data to long format using pivot_longer()
# anova_data <- my_data %>%
#   pivot_longer(
#     cols = c(x_deviation, y_deviation),  # Pivot X and Y deviations
#     names_to = "axis",                    # Create an 'axis' column (X/Y)
#     values_to = "deviationVal"            # Store deviations in 'deviationVal'
#   ) %>%
#   select(ID, hand, space, axis, deviationVal)  # Select relevant columns
# 
# # View the first few rows to ensure the structure is correct
# head(anova_data)
# 
# 
# 
# # Perform the mixed ANOVA with aov()
# anova_model <- aov(
#   deviationVal ~ hand * space * axis + Error(ID/(space * axis)),  # Mixed model formula
#   data = anova_data
# )
# 
# # View the ANOVA summary
# summary(anova_model)
# 
# 
# 
# 
# 
# 
# # Create separate datasets for X and Y deviations
# x_data <- my_data %>%
#   select(ID, hand, space, x_deviation) %>%
#   rename(deviationVal = x_deviation)  # Rename for consistency
# 
# y_data <- my_data %>%
#   select(ID, hand, space, y_deviation) %>%
#   rename(deviationVal = y_deviation)  # Rename for consistency
# 
# 
# # ANOVA for X-axis deviations
# anova_model_x <- aov(
#   deviationVal ~ hand * space + Error(ID/space),  # Mixed model formula
#   data = x_data
# )
# summary(anova_model_x)  # View results for X-axis
# 
# # ANOVA for Y-axis deviations
# anova_model_y <- aov(
#   deviationVal ~ hand * space + Error(ID/space),  # Mixed model formula
#   data = y_data
# )
# summary(anova_model_y)  # View results for Y-axis




















# 
# Step 1: Calculate mean deviations and SE
summary_data <- my_data %>%
  group_by(hand, space) %>%
  summarise(
    meanX = mean(x_deviation, na.rm = TRUE),
    meanY = mean(y_deviation, na.rm = TRUE),
    seX = sd(x_deviation, na.rm = TRUE) / sqrt(n()),
    seY = sd(y_deviation, na.rm = TRUE) / sqrt(n())
  )

# Step 2: Pivot the data to long format
pivotedData <- summary_data %>%
  pivot_longer(
    cols = c(meanX, meanY),
    names_to = "deviationDirection",
    values_to = "deviationVal"
  ) %>%
  mutate(SE = ifelse(deviationDirection == "meanX", seX, seY)) %>%
  select(-seX, -seY)

# Step 3: Create custom x-axis labels
pivotedData$condition <- factor(paste(pivotedData$hand, pivotedData$space, sep = " - "),
                                levels = c(
                                  "left-handed - left",
                                  "left-handed - front",
                                  "right-handed - front",
                                  "right-handed - right"
                                ))

# Step 4: Plot the data with custom x-axis labels
ggplot(pivotedData, aes(x = condition, y = deviationVal, fill = deviationDirection)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +

  # Add error bars
  geom_errorbar(
    aes(ymin = deviationVal - SE, ymax = deviationVal + SE),
    position = position_dodge(0.8), width = 0.25
  ) +

  # Customize the labels
  labs(
    title = "Mean Deviations by Hand and Space",
    x = "Condition (Hand + Space)",
    y = "Directional Deviation (cm)",
    fill = "Axis"
  ) +

  # Use theme to adjust x-axis text
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(vjust = -1.5),  # Push the x-axis label down
    plot.margin = margin(20, 20, 20, 20)  # Add some margin around the plot
  ) +

  # Add custom group labels (manually positioning them)
  annotate("text", x = 1.5, y = -1.8, label = "Left Hand", size = 5, hjust = 0.5) +
  annotate("text", x = 3.5, y = -1.8, label = "Right Hand", size = 5, hjust = 0.5)
# #
# # # 
# # # 
# # # 
# 
# 
# 
# 
# 
# 
# 
# 













