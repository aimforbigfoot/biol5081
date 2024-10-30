  
  library(ggplot2)
  library(dplyr)
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
    # (0, 15) - Front
    # (0, 45) - Front (further)
    # (14.2, 30) - Extreme right oblique
    # (-14.2, 30) - Extreme left oblique
    # (10, 20) - Oblique right close
    # (-10, 20) - Oblique left close
    # (10, 40) - Oblique right far
    # (-10, 40) - Oblique left far 
  )
  
  
  # Sample size
  N <- length(participantCount)
  fac <- 1
  bigFac <- 3
  
    
  
  right_handed_means <- list(
    front = data.frame(
      x = c( rep(1.92, 4), rep(1.36, 4)),
      y = c( rep(1.91, 4 ), rep(1.29, 4))
    ),
    right = data.frame(
      x = c(rep(2.04 , 4), rep(1.92 , 4)),
      y = c(rep(1.94 , 4), rep(1.57 , 4))
    )
  )
  right_handed_sds <- list(
    front = data.frame(
      x = c(rep(0.19 * sqrt(N)   , 4), rep(0.18* sqrt(N) , 4)),
      y = c(rep(0.3* sqrt(N)   ,  4), rep(0.19* sqrt(N) , 4))
    ),
    right = data.frame(
      x = c(rep(0.27* sqrt(N) , 4), rep(0.18* sqrt(N) , 4)),
      y = c(rep(0.28* sqrt(N) , 4), rep(0.19* sqrt(N) , 4))
    )
  )
  
  # right_handed_sds <- list(
  #   front = data.frame(
  #     x = c(rep(0.19* sqrt(N)   , 4), rep(0.18* sqrt(N) , 4)),
  #     y = c(rep(0.3* sqrt(N)   ,  4), rep(0.14* sqrt(N) , 4))
  #   ),
  #   right = data.frame(
  #     x = c(rep(0.27* sqrt(N) , 4), rep(0.18* sqrt(N) , 4)),
  #     y = c(rep(0.28* sqrt(N) , 4), rep(0.19* sqrt(N) , 4))
  #   )
  # )
  # 
  
  
  
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
      x = c(rep(0.24* sqrt(N)   , 4), rep(0.24* sqrt(N) , 4)),
      y = c(rep(0.14* sqrt(N)   ,  4), rep(0.13* sqrt(N) , 4))
    ),
    left = data.frame(
      x = c(rep(0.21* sqrt(N) , 4), rep(0.24* sqrt(N) , 4)),
      y = c(rep(0.31* sqrt(N) , 4), rep(0.13* sqrt(N) , 4))
    )
  )
  
  
  
  
  
  
  
  
  
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
  
  # Define angular bin ranges
  angle_bins <- list(
    "0-90°" = c(0, 90),
    "90-180°" = c(90, 180),
    "180-270°" = c(180, 270),
    "270-360°" = c(270, 360)
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
      angle_probs <- c(0.0, 0.0, 0.0, 1.0)
    } else {
      means <- left_handed_means
      sds <- left_handed_sds
      side_space <- "left"
      side_offset <- -45
      angle_probs <- c(0.1, 0.0, 0.7, 0.2)
    }
    
    for (j in 1:numOfPoints) {
      for (k in 1:reps_front) {
        angle_bin <- sample(names(angle_bins), 1, prob = c(0.00, 0.00, 1.00, 0.00))
        angle_value <- sample_angle(angle_bin)
        
        # Generate random adjustments
        deltaX <- means$front$x[j] + rnorm(1, 0, sds$front$x[j])
        deltaY <- means$front$y[j] + rnorm(1, 0, sds$front$y[j])
        
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
        angle_bin <- sample(names(angle_bins), 1, prob = angle_probs)
        angle_value <- sample_angle(angle_bin)
        
        deltaX <- rnorm(1, mean = means[[side_space]]$x[j], sd = sds[[side_space]]$x[j])
        deltaY <- rnorm(1, mean = means[[side_space]]$y[j], sd = sds[[side_space]]$y[j])
        
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
  
  
  
  print(colnames(freq_data))
  
  
  # Create the frequency data as a data frame
  freq_data <- data.frame(
    hand = c(rep("left-handed", 2), rep("right-handed", 2)),
    space = c("left", "front", "right", "front"),
    "0t90" = c(10, 17, 16, 23),
    "90t180" = c(0, 16, 8, 17),
    "180t270" = c(53, 20, 8, 14),
    "270t360" = c(9, 19, 39, 18)
  )
  
  # Print the frequency data
  print(freq_data)
  
  library(tidyr)
  # Convert the data to long format using the correct column names
  freq_data_long <- freq_data %>%
    pivot_longer(
      cols = c(X0t90, X90t180, X180t270, X270t360),
      names_to = "angle_bin",
      values_to = "frequency"
    )
  
  # Print the result to verify
  print(freq_data_long)
  
  
  
  
  # Create the frequency data as a data frame
  freq_data <- data.frame(
    hand = c(rep("left-handed", 2), rep("right-handed", 2)),
    space = c("left", "front", "right", "front"),
    X0t90 = c(10, 17, 16, 23),
    X90t180 = c(0, 16, 8, 17),
    X180t270 = c(53, 20, 8, 14),
    X270t360 = c(9, 19, 39, 18)
  )
  
  # Compute the total counts for each row
  total_counts <- rowSums(freq_data[, 3:6])
  
  # Normalize the frequency data
  normalized_data <- freq_data
  normalized_data[, 3:6] <- sweep(freq_data[, 3:6], 1, total_counts, FUN = "/")
  
  # Extract each row into separate variables
  row1 <- normalized_data[1, 3:6]  # Left-handed, Left space
  row2 <- normalized_data[2, 3:6]  # Left-handed, Front space
  row3 <- normalized_data[3, 3:6]  # Right-handed, Right space
  row4 <- normalized_data[4, 3:6]  # Right-handed, Front space
  
  # Print the variables to verify
  print(row1)
  print(row2)
  print(row3)
  print(row4)
  
  # Extract row values as numeric vectors
  row1_values <- as.numeric(normalized_data[1, 3:6])  # Left-handed, Left space
  row2_values <- as.numeric(normalized_data[2, 3:6])  # Left-handed, Front space
  row3_values <- as.numeric(normalized_data[3, 3:6])  # Right-handed, Right space
  row4_values <- as.numeric(normalized_data[4, 3:6])  # Right-handed, Front space
  
  # Print the row values to verify (without column names)
  print(row1_values)
  print(row2_values)
  print(row3_values)
  print(row4_values)
  