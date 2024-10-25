
library(ggplot2)
# Set seed for reproducibility
set.seed(123)

# Define reference points (you can modify these)
ref_points <- data.frame(
  ref_x = c(0,   0, 10, -10, 10, -10, 14.2, -14.2),  # Example reference x-coordinates
  ref_y = c(15, 45, 20,  20, 40, 40, 30,  30)  # Example reference y-coordinates
)

# Set the number of repetitions per reference point
reps_per_point <- 10

# Function to generate data for one participant
generate_participant_data <- function(participant_id) {
  # Expand reference points based on repetitions
  ref_data <- ref_points[rep(1:nrow(ref_points), each = reps_per_point), ]
  
  # Assign trial numbers
  ref_data$trial_number <- 1:nrow(ref_data)
  
  # Generate unique placed positions using normal distribution
  ref_data$placed_x <- rnorm(nrow(ref_data), mean = ref_data$ref_x, sd = 0.2)
  ref_data$placed_y <- rnorm(nrow(ref_data), mean = ref_data$ref_y, sd = 2)
  
  # Calculate angle in degrees from the reference point to the placed point
  ref_data$angle <- atan2(ref_data$placed_y - ref_data$ref_y, 
                          ref_data$placed_x - ref_data$ref_x) * (180 / pi)
  
  # Adjust negative angles to fall within the 0-360 degree range
  ref_data$angle[ref_data$angle < 0] <- ref_data$angle[ref_data$angle < 0] + 360
  
  # Add participant ID to the data
  ref_data$participant_id <- participant_id
  
  # Reorder columns so trial_number comes first
  ref_data <- ref_data[, c("participant_id", "trial_number", "ref_x", 
                           "ref_y", "placed_x", "placed_y", "angle")]
  
  return(ref_data)
}

# Generate data for 20 participants and combine into one dataframe
all_participants_data <- bind_rows(lapply(1:20, generate_participant_data))

# View the combined data
print(head(all_participants_data, 10))
all_participants_data


# Calculate the average placed positions for each reference point
avg_points <- all_participants_data %>%
  group_by(ref_x, ref_y) %>%
  summarize(avg_x = mean(placed_x), avg_y = mean(placed_y), .groups = 'drop')

# Create the plot
ggplot(all_participants_data, aes(x = placed_x, y = placed_y)) +
  # Plot the individual placed points
  geom_point(color = "blue", alpha = 0.5) +
  # Plot the reference points
  geom_point(data = ref_points, aes(x = ref_x, y = ref_y), 
             color = "red", size = 3, shape = 4) +
  # Plot the average placed points
  geom_point(data = avg_points, aes(x = avg_x, y = avg_y), 
             color = "green", size = 3, shape = 1) +
  # Connect reference points to their average placed points
  geom_segment(data = avg_points, aes(x = ref_x, y = ref_y, 
                                      xend = avg_x, yend = avg_y),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  # Labels and themes
  labs(title = "Reproduced Positions vs. Reference Points",
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()
  
