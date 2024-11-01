
# ------------------------------------------------------
# ----------------- Data generation  -------------------
# ------------------------------------------------------


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # for pivot_longer()

set.seed(123)  # Set seed for reproducibility

# Participant IDs and their handedness
participantCount <- 1:20
participant_tags <- data.frame(
  ID = participantCount
)

# Define reference points for different spatial positions with arrangement
base_points <- data.frame(
  ref_x = c(30, 30, 15, 45),
  ref_y = c(14.2, -14.2, -10, 10),
  arrangement = c("horizontal-vertical", "horizontal-vertical", "oblique", "oblique")
)

# Set number of repetitions for each spatial position
reps_points <- 5 
numOfPoints <- length(base_points$ref_x)

# Sample size
N <- length(participantCount)

shoulderMeans <- list(
  shoulder = data.frame(
    x = c(rep(1.14, 2), rep(2.19, 2)),
    y = c(rep(1.65, 2), rep(1.09, 2))
  ),
  aboveShoulder = data.frame(
    x = c(rep(2.02, 2), rep(3.21, 2)),
    y = c(rep(1.87, 2), rep(1.3, 2))
  )
)
shoulderSDs <- list(
  shoulder = data.frame(
    x = c(rep(0.23 * sqrt(reps_points), 2), rep(0.43 * sqrt(reps_points), 2)),
    y = c(rep(0.33 * sqrt(reps_points), 2), rep(0.2 * sqrt(reps_points), 2))
  ),
  aboveShoulder = data.frame(
    x = c(rep(0.18 * sqrt(reps_points), 2), rep(0.54 * sqrt(reps_points), 2)),
    y = c(rep(0.2 * sqrt(reps_points), 2), rep(0.22 * sqrt(reps_points), 2))
  )
)

accAngleFreqShoulderFreq <- c(14, 6, 13, 7)
accAngleFreqAboveShoulderFreq <- c(5, 3, 19, 13)
normalizedFreqShoulder <- accAngleFreqShoulderFreq / sum(accAngleFreqShoulderFreq)
normalizedFreqAboveShoulder <- accAngleFreqAboveShoulderFreq / sum(accAngleFreqAboveShoulderFreq)

# Initialize an empty data frame to accumulate rows
my_data <- data.frame(
  ID = integer(),
  subjectX = numeric(),
  subjectY = numeric(),
  ref_x = numeric(),
  ref_y = numeric(),
  space = character(),
  quadrant = character(),
  arrangement = character()
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
  for (j in 1:numOfPoints) {
    arrangement_type <- base_points$arrangement[j]
    
    for (k in 1:reps_points) {
      # Generate random adjustments for shoulder
      deltaX <- rnorm(1, shoulderMeans$shoulder$x[j], shoulderSDs$shoulder$x[j])
      deltaY <- rnorm(1, shoulderMeans$shoulder$y[j], shoulderSDs$shoulder$y[j])
      
      # Assign a quadrant based on predefined frequencies
      quadrant <- sample(c(1, 2, 3, 4), 1, prob = normalizedFreqShoulder)
      quadrant
      angle <- runif(1, (quadrant - 1) * 90, quadrant * 90)
      angle
      rotated_coords <- rotate_point(deltaX, deltaY, angle)
      
      subjectX <- base_points$ref_x[j] + rotated_coords[1]
      subjectY <- base_points$ref_y[j] + rotated_coords[2]
      
      quadrant_label <- paste0((quadrant - 1) * 90, "-", quadrant * 90)
      
      rowToAdd <- data.frame(
        ID = i,
        subjectX = subjectX,
        subjectY = subjectY,
        ref_x = base_points$ref_x[j],
        ref_y = base_points$ref_y[j],
        space = "shoulder",
        quadrant = quadrant_label,
        arrangement = arrangement_type
      )
      my_data <- bind_rows(my_data, rowToAdd)
    }
    
    for (k in 1:reps_points) {
      # Generate random adjustments for above shoulder
      deltaX <- rnorm(1, shoulderMeans$aboveShoulder$x[j], shoulderSDs$aboveShoulder$x[j])
      deltaY <- rnorm(1, shoulderMeans$aboveShoulder$y[j], shoulderSDs$aboveShoulder$y[j])
      
      # Assign a quadrant based on predefined frequencies
      quadrant <- sample(c(1, 2, 3, 4), 1, prob = normalizedFreqAboveShoulder)
      angle <- runif(1, (quadrant - 1) * 90, quadrant * 90)
      rotated_coords <- rotate_point(deltaX, deltaY, angle)
      
      subjectX <- base_points$ref_x[j] + rotated_coords[1]
      subjectY <- base_points$ref_y[j] + rotated_coords[2] + 30
      
      quadrant_label <- paste0((quadrant - 1) * 90, "-", quadrant * 90)
      
      rowToAdd <- data.frame(
        ID = i,
        subjectX = subjectX,
        subjectY = subjectY,
        ref_x = base_points$ref_x[j],
        ref_y = base_points$ref_y[j] + 30,
        space = "shoulderAbove",
        quadrant = quadrant_label,
        arrangement = arrangement_type
      )
      my_data <- bind_rows(my_data, rowToAdd)
    }
  }
}


# Scatter plot of subject positions with reference points
ggplot(my_data, aes(x = subjectX, y = subjectY, color = space, shape = arrangement)) +
  geom_point(alpha = 0.6, size = 3) +  # Points for each data point
  geom_point(aes(x = ref_x, y = ref_y), color = "black", shape = 4, size = 3) +  # Reference points
  facet_wrap(~ space) +  # Facet by arrangement type if desired - THIS LINE IS COOL IF U REMOVE OR ADD IT
  labs(
    title = "Scatter Plot of Subject Positions",
    x = "X Position",
    y = "Y Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )



# ------------------------------------------------------
# ------------------- Data analysis --------------------
# ------------------------------------------------------



# Calculate deviations from the reference points
my_data <- my_data %>%
  mutate(
    deviationX = (subjectX - ref_x),
    deviationY = (ref_y-subjectY)
  )



# ------------------- Two way ANOVA --------------------

# Two-way ANOVA for deviationX
anova_x <- aov(deviationX ~ space * arrangement, data = my_data)
summary(anova_x)

# Two-way ANOVA for deviationY
anova_y <- aov(deviationY ~ space * arrangement, data = my_data)
summary(anova_y)

# -------------- Direction of Deviations ---------------



# Calculate directional deviations with positive values as anti-clockwise for above shoulder height
# Let's say positive deviation on X and Y separately represents anti-clockwise in your setup

# Create a directional deviation based on X and Y, positive values are anti-clockwise deviations
my_data <- my_data %>%
  mutate(
    directional_deviationX = if_else(space == "shoulderAbove", deviationX, -deviationX),
    directional_deviationY = if_else(space == "shoulderAbove", deviationY, -deviationY)
  )

# Perform one-sample t-tests for each axis and reproduction space
# 1. T-test for X-axis directional deviation in each space
t_test_results_x <- my_data %>%
  group_by(space) %>%
  summarize(
    t_test_p_value_x = t.test(directional_deviationX, mu = 0)$p.value,
    t_test_statistic_x = t.test(directional_deviationX, mu = 0)$statistic,
    mean_directional_deviationX = mean(directional_deviationX)
  )

# 2. T-test for Y-axis directional deviation in each space
t_test_results_y <- my_data %>%
  group_by(space) %>%
  summarize(
    t_test_p_value_y = t.test(directional_deviationY, mu = 0)$p.value,
    t_test_statistic_y = t.test(directional_deviationY, mu = 0)$statistic,
    mean_directional_deviationY = mean(directional_deviationY)
  )

# Display the results
t_test_results_x
t_test_results_y












# ---- VIsualizing deviatiosn 
# Summarize mean and standard error for deviations by space and arrangement
summary_table <- my_data %>%
  group_by(space, arrangement) %>%
  summarize(
    mean_deviationX = mean(deviationX),
    absDeviationX = mean(abs(deviationX)),
    se_deviationX = sd(deviationX) / sqrt(n()),
    mean_deviationY = mean(deviationY),
    absDeviationY = mean(abs(deviationY)),
    se_deviationY = sd(deviationY) / sqrt(n())
  ) %>%
  # Optionally round values for readability
  mutate(across(starts_with("mean"), round, 2),
         across(starts_with("se"), round, 2))
summary_table


# Summarize deviations with SEs
summary_se <- my_data %>%
  group_by(space, arrangement) %>%
  summarize(
    mean_deviationX = mean(deviationX),
    se_deviationX = sd(deviationX) / sqrt(n()),
    mean_deviationY = mean(deviationY),
    se_deviationY = sd(deviationY) / sqrt(n())
  )

# Reshape the data for ggplot
summary_se_long <- summary_se %>%
  pivot_longer(
    cols = c(mean_deviationX, mean_deviationY, se_deviationX, se_deviationY),
    names_to = c(".value", "deviation"),
    names_sep = "_"
  )

# Plot mean deviations with error bars
ggplot(summary_se_long, aes(x = space, y = mean, fill = deviation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.9), width = 0.2) +
  facet_wrap(~ arrangement) +
  labs(
    title = "Mean Deviations with Standard Errors",
    x = "Space",
    y = "Deviation",
    fill = "Deviation Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )










# Summarize deviations with SEs
summary_se <- my_data %>%
  group_by(space) %>%
  summarize(
    mean_deviationX = mean(deviationX),
    se_deviationX = sd(deviationX) / sqrt(n()),
    mean_deviationY = mean(deviationY),
    se_deviationY = sd(deviationY) / sqrt(n())
  )

# Reshape the data for ggplot
summary_se_long <- summary_se %>%
  pivot_longer(
    cols = c(mean_deviationX, mean_deviationY, se_deviationX, se_deviationY),
    names_to = c(".value", "deviation"),
    names_sep = "_"
  )

# Plot mean deviations with error bars without splitting by arrangement
ggplot(summary_se_long, aes(x = space, y = mean, fill = deviation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = "Mean Deviations with Standard Errors",
    x = "Space",
    y = "Deviation",
    fill = "Deviation Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )







