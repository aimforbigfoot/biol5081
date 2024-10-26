library(ggplot2)
library(dplyr)
set.seed(123)

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

# Parameters for generating subject data points
# Adjusted based on the provided deviations for different conditions
mean_subject_x_left_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Adjusted means for left-handed X-axis (front positions)
mean_subject_y_left_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Adjusted means for left-handed Y-axis (front positions)
mean_subject_x_right_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Adjusted means for right-handed X-axis (front positions)
mean_subject_y_right_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Adjusted means for right-handed Y-axis (front positions)

sd_subject_x_left_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Standard deviations for left-handed X-axis (front positions)
sd_subject_y_left_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Standard deviations for left-handed Y-axis (front positions)
sd_subject_x_right_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Standard deviations for right-handed X-axis (front positions)
sd_subject_y_right_handed_front <- c(0, 0, 0, 0, 0, 0, 0, 0)  # Standard deviations for right-handed Y-axis (front positions)

mean_subject_x_left <- c(0,0,0,0,0,0,0,0)  # Adjusted means for left-handed X-axis (other positions)
mean_subject_y_left <- c(0,0,0,0,0,0,0,0)  # Adjusted means for left-handed Y-axis (other positions)
sd_subject_x_left <- c(0,0,0,0,0,0,0,0)  # Standard deviations for left-handed X-axis
sd_subject_y_left <- c(0,0,0,0,0,0,0,0)  # Standard deviations for left-handed Y-axis

mean_subject_x_right <- c(0,0,0,0,0,0,0,0)  # Adjusted means for right-handed X-axis (other positions)
mean_subject_y_right <- c(0,0,0,0,0,0,0,0)  # Adjusted means for right-handed Y-axis (other positions)
sd_subject_x_right <- c(0,0,0,0,0,0,0,0)  # Standard deviations for right-handed X-axis
sd_subject_y_right <- c(0,0,0,0,0,0,0,0)  # Standard deviations for right-handed Y-axis

# mean_subject_x_left <- c(-0.21, 1.59, -0.18, 2.04, -0.18, 1.43, -0.24, 1.29)  # Adjusted means for left-handed X-axis (other positions)
# mean_subject_y_left <- c(-0.31, 1.67, -0.19, 1.94, -0.14, 1.57, -0.13, 1.91)  # Adjusted means for left-handed Y-axis (other positions)
# sd_subject_x_left <- c(0.2, 1.8, 0.19, 2.0, 0.2, 1.5, 0.25, 1.6)  # Standard deviations for left-handed X-axis
# sd_subject_y_left <- c(0.3, 1.9, 0.21, 2.1, 0.25, 1.7, 0.22, 1.8)  # Standard deviations for left-handed Y-axis
# 
# mean_subject_x_right <- c(0.25, 1.75, -0.2, 2.1, -0.15, 1.5, -0.3, 1.35)  # Adjusted means for right-handed X-axis (other positions)
# mean_subject_y_right <- c(-0.28, 1.7, -0.22, 1.96, -0.12, 1.6, -0.15, 1.95)  # Adjusted means for right-handed Y-axis (other positions)
# sd_subject_x_right <- c(0.25, 1.85, 0.2, 2.1, 0.22, 1.55, 0.3, 1.65)  # Standard deviations for right-handed X-axis
# sd_subject_y_right <- c(0.35, 1.95, 0.23, 2.15, 0.28, 1.75, 0.24, 1.85)  # Standard deviations for right-handed Y-axis

# Set number of repetitions for each spatial position
reps_front <- 2  # Number of repetitions for front space
reps_side <- 3  # Number of repetitions for side spaces
numOfPoints <- length(base_points$ref_x)
participantCount <- 1:6

# Add tags to participants
participant_tags <- data.frame(
  ID = participantCount,
  hand = rep(c("left-handed", "right-handed"), length.out = length(participantCount))  # Assign tags to each participant
)

# Initialize an empty data frame to accumulate rows
my_data <- data.frame(ID = integer(), subjectX = numeric(), subjectY = numeric(), ref_x = numeric(), ref_y = numeric(), hand = character(), space = character())

# Add rows to the data frame in a loop
for (i in participantCount) {
  cat("Processing participant ID:", i, "\n")  # Debug log for participant ID
  for (j in 1:numOfPoints) {
    cat("  Processing reference point index:", j, "\n")  # Debug log for reference point index
    # Add front space points (2 repetitions)
    for (k in 1:reps_front) {
      cat("    Adding front space repetition:", k, "\n")  # Debug log for front space repetition
      rowToAdd <- data.frame(
        ID = i,
        subjectX = rnorm(1, mean = base_points$ref_x[j] + ifelse(participant_tags$hand[i] == 'right-handed', mean_subject_x_right_handed_front[j] + 45, mean_subject_x_left_handed_front[j] - 45), sd = ifelse(participant_tags$hand[i] == 'right-handed', sd_subject_x_right_handed_front[j], sd_subject_x_left_handed_front[j])),  # Generate random X coordinate based on mean and sd
        subjectY = rnorm(1, mean = base_points$ref_y[j] + ifelse(participant_tags$hand[i] == 'left-handed', mean_subject_y_left_handed_front[j], mean_subject_y_right_handed_front[j]), sd = ifelse(participant_tags$hand[i] == 'left-handed', sd_subject_y_left_handed_front[j], sd_subject_y_right_handed_front[j])),  # Generate random Y coordinate based on mean and sd
        ref_x = base_points$ref_x[j] + ifelse(participant_tags$hand[i] == 'right-handed', 45, -45),  # Reference X coordinate with correct offset for handedness
        ref_y = base_points$ref_y[j],  # Reference Y coordinate
        hand = participant_tags$hand[i],  # Participant handedness
        space = "front"  # Space type: front
      )
      my_data <- rbind(my_data, rowToAdd)  # Append row to data frame
    }
    # Add side space points (3 repetitions for left/right based on handedness)
    if (participant_tags$hand[i] == "right-handed") {
      for (k in 1:reps_side) {
        cat("    Adding right space repetition:", k, "\n")  # Debug log for right space repetition
        rowToAdd <- data.frame(
          ID = i,
          subjectX = rnorm(1, mean = base_points$ref_x[j] + ifelse(participant_tags$hand[i] == 'right-handed', mean_subject_x_right[j] + 45, mean_subject_x_left[j] - 45), sd = ifelse(participant_tags$hand[i] == 'right-handed', sd_subject_x_right[j], sd_subject_x_left[j])),  # Generate random X coordinate for right-handed participants
          subjectY = rnorm(1, mean = base_points$ref_y[j] + ifelse(participant_tags$hand[i] == 'left-handed', mean_subject_y_left[j], mean_subject_y_right[j]), sd = ifelse(participant_tags$hand[i] == 'left-handed', sd_subject_y_left[j], sd_subject_y_right[j])),  # Generate random Y coordinate
          ref_x = base_points$ref_x[j] + ifelse(participant_tags$hand[i] == 'right-handed', 45, -45),  # Reference X coordinate with correct offset for handedness
          ref_y = base_points$ref_y[j],  # Reference Y coordinate
          hand = participant_tags$hand[i],  # Participant handedness
          space = "right"  # Space type: right
        )
        my_data <- rbind(my_data, rowToAdd)  # Append row to data frame
      }
    } else {
      for (k in 1:reps_side) {
        cat("    Adding left space repetition:", k, "\n")  # Debug log for left space repetition
        rowToAdd <- data.frame(
          ID = i,
          subjectX = rnorm(1, mean = base_points$ref_x[j] + ifelse(participant_tags$hand[i] == 'left-handed', mean_subject_x_left[j], mean_subject_x_right[j]), sd = ifelse(participant_tags$hand[i] == 'left-handed', sd_subject_x_left[j], sd_subject_x_right[j])),  # Generate random X coordinate for left-handed participants
          subjectY = rnorm(1, mean = base_points$ref_y[j] + ifelse(participant_tags$hand[i] == 'left-handed', mean_subject_y_left[j], mean_subject_y_right[j]), sd = ifelse(participant_tags$hand[i] == 'left-handed', sd_subject_y_left[j], sd_subject_y_right[j])),  # Generate random Y coordinate
          ref_x = base_points$ref_x[j],  # Reference X coordinate with offset for left space
          ref_y = base_points$ref_y[j],  # Reference Y coordinate
          hand = participant_tags$hand[i],  # Participant handedness
          space = "left"  # Space type: left
        )
        my_data <- rbind(my_data, rowToAdd)  # Append row to data frame
      }
    }
  }
}

# Display the final data frame
print("Final data frame:")  # Debug log for displaying the final data frame
print(my_data)

# Write the final data frame to a CSV file
write.csv(my_data, file = "my_data.csv", row.names = FALSE)

# Create separate scatter plots for left-handed and right-handed participants, including front, left, and right data
my_data_left <- my_data %>% filter(hand == "left-handed")  # Filter data for left-handed participants
my_data_right <- my_data %>% filter(hand == "right-handed")  # Filter data for right-handed participants

# Plot for left-handed participants including front, left, and right spaces
print("Creating plot for left-handed participants")  # Debug log for plotting left-handed participants
ggplot(my_data_left, aes(x = subjectX, y = subjectY)) +
  geom_point(aes(color = space), size = 3, shape = 4) +  # Plot data points as 'x'
  geom_point(data = base_points, aes(x = ref_x, y = ref_y), color = "black", size = 2, shape = 21, fill = "white", alpha = 0.8) +  # Plot reference points as white circles
  geom_point(data = transform(base_points, ref_x = base_points$ref_x - 45), aes(x = ref_x, y = ref_y), color = "black", size = 2, shape = 21, fill = "white", alpha = 0.8) +  # Offset reference points for left space
  labs(
    x = "Subject X",
    y = "Subject Y",
    title = "Generated Data Points for Left-Handed Participants (Front, Left, and Right Spaces)",
    color = "Space"
  ) +
  theme_minimal() +
  xlim(-60, 20) +
  ylim(0, 50)

# Plot for right-handed participants including front, left, and right spaces
print("Creating plot for right-handed participants")  # Debug log for plotting right-handed participants
ggplot(my_data_right, aes(x = subjectX, y = subjectY)) +
  geom_point(aes(color = space), size = 3, shape = 4) +  # Plot data points as 'x'
  geom_point(data = base_points, aes(x = ref_x, y = ref_y), color = "black", size = 2, shape = 21, fill = "white", alpha = 0.8) +  # Plot reference points as white circles
  geom_point(data = transform(base_points, ref_x = base_points$ref_x + 45), aes(x = ref_x, y = ref_y), color = "black", size = 2, shape = 21, fill = "white", alpha = 0.8) +  # Offset reference points for right space
  labs(
    x = "Subject X",
    y = "Subject Y",
    title = "Generated Data Points for Right-Handed Participants (Front, Left, and Right Spaces)",
    color = "Space"
  ) +
  theme_minimal() +
  xlim(-20, 60) +
  ylim(0, 50)
