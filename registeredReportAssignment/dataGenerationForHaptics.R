# Load necessary libraries for data manipulation and visualization
library(ggplot2)  # For creating plots
library(tidyr)    # For data tidying
library(dplyr)    # For data manipulation
library(forcats)  # For working with categorical variables (factors)
library(pwr)      # For power analysis
library(broom)    # For tidying model outputs

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- DATA GENERATION -----------------------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Define the spatial points where the tasks are performed
xPoints <- c(7.5, 22.5, 37.5)  # X-coordinates for pegs
yPoints <- c(45, 30, 15)       # Y-coordinates for different shapes
diamondY <- 45
triangleY <- 30
circleY <- 15

numOfRepsPerPoint <- 3  # Number of repetitions per point
numOfPpl <- 20        # Total number of participants

# Initialize an empty data frame to store the simulated data
data <- data.frame(
  Person = integer(),
  Hand = character(),
  ExpType = character(),
  AnchorPegX = numeric(),
  AnchorPegY = numeric(),
  RefPegX = numeric(),
  RefPegY = numeric(),
  MirrorPegX = numeric(),
  RealMirrorPegX = numeric(),
  MirrorPegY = numeric(),
  RealMirrorPegY = numeric(),
  OffsetX = numeric(),
  OffsetY = numeric(),
  Height = numeric(),
  Wingspan = numeric(),
  stringsAsFactors = FALSE
)

# Define the means and standard deviations for the offsets (deviations)
# These represent how much participants deviate from the target positions
specificCircleMeansX <- c(-2, -1, -0.5, 0.5, 1, 2)
specificCircleMeansY <- c(0, 0, 0, 0, 0, 0)
specificCircleSDsX <- c(1.5, 1.2, 1.2, 1.2, 1.2, 1.5)
specificCircleSDsY <- c(1.7, 1.5, 1.2, 1.2, 1.5, 1.7)

specificTriangleMeansX <- c(-3.5, -3, -1.5, 1.5, 3, 3.5)
specificTriangleMeansY <- c(2, 1, 1, 1, 1, 2)
specificTriangleSDsX <- c(2, 1.8, 1, 1, 1.8, 2)
specificTriangleSDsY <- c(2.2, 2, 1.5, 1.5, 2, 2.2)

specificDiamondMeansX <- c(-4.5, -4, -2, 2, 4, 4.5)
specificDiamondMeansY <- c(3, 2, 2, 2, 2, 3)
specificDiamondSDsX <- c(2.5, 2.3, 1.5, 1.5, 2.3, 2.5)
specificDiamondSDsY <- c(3.8, 2.7, 2.5, 2.5, 2.7, 3.8)

# Create a loop for xPoints that includes negative values (mirror positions)
xPointsLoop <- c(-xPoints[3], -xPoints[2], -xPoints[1], xPoints[1], xPoints[2], xPoints[3])

# Loop over each participant to generate their data
for (person in 1:numOfPpl) {
  # Randomly assign height and wingspan to each participant
  height <- rnorm(1, mean = 170, sd = 30)      # Mean height 170 cm
  wingspan <- rnorm(1, mean = 180, sd = 30)    # Mean wingspan 180 cm
  hand <- ifelse(runif(1) < 0.5, "left", "right")  # Randomly assign hand dominance
  
  # Calculate scaling factors based on height and wingspan
  heightScalingFactor <- height / 170        # Relative to average height
  wingspanScalingFactor <- wingspan / 180    # Relative to average wingspan
  scalingFactor <- (heightScalingFactor + wingspanScalingFactor) / 2  # Average scaling factor
  
  # Loop through the three experimental types (diamond, triangle, circle)
  for (h in 1:3) {
    currY <- yPoints[h]
    
    # Depending on the experiment type, assign the appropriate Y-coordinates and means
    if (h == 1) {
      AnchorPegY <- circleY
      RefPegY <- triangleY
      MirrorPegY <- diamondY
      expType <- "diamondToCircle"
      
      # Adjust means based on the participant's scaling factor
      XmeansToUse <- specificDiamondMeansX * scalingFactor
      YmeansToUse <- specificDiamondMeansY * scalingFactor
      XsdsToUse <- specificDiamondSDsX
      YsdsToUse <- specificDiamondSDsY
      
    } else if (h == 2) {
      AnchorPegY <- diamondY
      RefPegY <- circleY
      MirrorPegY <- triangleY
      expType <- "midPointTriangle"
      
      XmeansToUse <- specificTriangleMeansX * scalingFactor
      YmeansToUse <- specificTriangleMeansY * scalingFactor
      XsdsToUse <- specificTriangleSDsX
      YsdsToUse <- specificTriangleSDsY
      
    } else if (h == 3) {
      AnchorPegY <- diamondY
      RefPegY <- triangleY
      MirrorPegY <- circleY
      expType <- "circleToDiamond"
      
      XmeansToUse <- specificCircleMeansX * scalingFactor
      YmeansToUse <- specificCircleMeansY * scalingFactor
      XsdsToUse <- specificCircleSDsX
      YsdsToUse <- specificCircleSDsY
    }
    
    # Loop over each condition (different x positions)
    for (i in 1:6) {
      for (repsOfPoint in 1:numOfRepsPerPoint) {
        # Generate random offsets based on the means and standard deviations
        offsetX <- rnorm(1, mean = XmeansToUse[i], sd = XsdsToUse[i])
        offsetY <- rnorm(1, mean = YmeansToUse[i], sd = YsdsToUse[i])
        
        # Calculate the participant's response positions
        realMirrorPegX <- xPointsLoop[i]
        mirrorPegX <- realMirrorPegX + offsetX
        mirrorPegY <- MirrorPegY + offsetY
        
        # Add the new data point to the data frame
        data <- rbind(data, data.frame(
          Person = person,
          hand = hand,
          ExpType = expType,
          AnchorPegX = xPointsLoop[i],
          AnchorPegY = AnchorPegY,
          RefPegX = xPointsLoop[i],
          RefPegY = RefPegY,
          MirrorPegX = mirrorPegX,
          RealMirrorPegX = realMirrorPegX,
          MirrorPegY = mirrorPegY,
          RealMirrorPegY = MirrorPegY,
          OffsetX = offsetX,
          OffsetY = offsetY,
          Height = height,
          Wingspan = wingspan
        ))
      }
    }
  }
}

# Randomly assign 'Hand' as a factor (this seems redundant since 'hand' is already assigned)
data <- data %>%
  mutate(Hand = rep(c("Left", "Right"), length.out = nrow(data)))

# Adjust offsets slightly based on hand dominance and anchor positions
data <- data %>%
  mutate(
    HandOffsetX = case_when(
      Hand == "Left" & AnchorPegX == 7.5 ~ OffsetX - 0.1,
      Hand == "Left" & AnchorPegX == 22.5 ~ OffsetX - 0.05,
      Hand == "Left" & AnchorPegX == 37.5 ~ OffsetX - 0.2,
      Hand == "Right" & AnchorPegX == 7.5 ~ OffsetX + 0.1,
      Hand == "Right" & AnchorPegX == 22.5 ~ OffsetX + 0.05,
      Hand == "Right" & AnchorPegX == 37.5 ~ OffsetX + 0.2,
      TRUE ~ OffsetX  # If none of the above, no adjustment
    )
  )

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- VIEW RAW DATA -------------------------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Visualize the data to see the distribution of pegs and responses
ggplot(data) +
  geom_point(aes(x = MirrorPegX, y = MirrorPegY), color="coral", alpha = 0.5, size = 3) +  # Participant responses
  geom_point(aes(x = AnchorPegX, y = AnchorPegY), color = "red", size = 2) +              # Anchor pegs
  geom_point(aes(x = RefPegX, y = RefPegY), size = 2, color =  "darkgreen") +             # Reference pegs
  geom_point(aes(x = RealMirrorPegX, y = RealMirrorPegY), color = "blue", size = 5, shape = 4, stroke = 2) +  # Target positions
  xlim(-50, 50) +
  ylim(0, 60) +
  theme_minimal() +
  labs(color = "Person")

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- VIEW DEVIATIONS (with hand effects) ---------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Summarize deviations grouped by experiment type, real peg coordinates, and hand
deviation_summary <- data %>%
  group_by(ExpType, RealMirrorPegX, RealMirrorPegY, hand) %>%
  summarise(
    AvgDeviationX = mean((OffsetX)),  # Average deviation in X
    AvgDeviationY = mean((OffsetY)),  # Average deviation in Y
    StdDevX = sd(OffsetX),            # Standard deviation in X
    StdDevY = sd(OffsetY),            # Standard deviation in Y
    .groups = "drop"
  )

# Print the summarized deviations
print(deviation_summary)

# Plot the deviations with error bars
ggplot(deviation_summary, 
       aes(x = RealMirrorPegX + AvgDeviationX, 
           y = RealMirrorPegY + AvgDeviationY)) +
  geom_point(aes(color = ExpType, shape = hand), size = 3) +  # Deviations colored by experiment type and shaped by hand
  geom_errorbar(aes(
    ymin = (RealMirrorPegY + AvgDeviationY) - StdDevY,
    ymax = (RealMirrorPegY + AvgDeviationY) + StdDevY
  ), width = 0.2, color = "blue") +  # Y-axis error bars
  geom_errorbarh(aes(
    xmin = (RealMirrorPegX + AvgDeviationX) - StdDevX,
    xmax = (RealMirrorPegX + AvgDeviationX) + StdDevX
  ), height = 0.2, color = "blue") +  # X-axis error bars
  geom_point(aes(x = RealMirrorPegX, y = RealMirrorPegY), 
             color = "red", size = 4) +  # Original target positions
  labs(title = "Deviation Around Adjusted Peg Coordinates by Hand",
       x = "X Coordinate",
       y = "Y Coordinate",
       color = "Experiment Type",
       shape = "Hand") +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- VIEW DEVIATIONS AS BOX PLOTS PER AXIS -------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Set up plotting area for side-by-side boxplots
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Ensure the experiment types are in a specific order
data$ExpType <- factor(data$ExpType, levels = c("circleToDiamond", "midPointTriangle", "diamondToCircle"))

# Boxplot for X-axis deviations
boxplot(OffsetX ~ ExpType, data = data, main = "OffsetX Across Experimental Types",
        xlab = "Experimental Type", ylab = "OffsetX", col = "lightblue")

# Boxplot for Y-axis deviations
boxplot(OffsetY ~ ExpType, data = data, main = "OffsetY Across Experimental Types",
        xlab = "Experimental Type", ylab = "OffsetY", col = "lightgreen")

# Reset plotting layout to default
par(mfrow = c(1, 1))

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- ANALYZING DEVIATIONS FOR SIGNIFICANCEVIA LINEAR MODELS AND ANOVAs ----------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Summarize deviations per participant and experimental condition
row_trends <- data %>%
  group_by(Person, ExpType, AnchorPegX, AnchorPegY) %>%
  summarise(
    TotalDeviation = sqrt(mean(OffsetX^2, na.rm = TRUE) + mean(OffsetY^2, na.rm = TRUE)),  # Euclidean distance
    AvgOffsetX = mean(OffsetX, na.rm = TRUE),
    SDOffsetX = sd(OffsetX, na.rm = TRUE),
    AvgOffsetY = mean(OffsetY, na.rm = TRUE),
    SDOffsetY = sd(OffsetY, na.rm = TRUE),
    SDDeviation = sd(sqrt(OffsetX^2 + OffsetY^2), na.rm = TRUE),
    .groups = "drop"
  )

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- BOX PLOT OF MEAN TOTAL DEVIATION ------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Summarize data for plotting
summary_data <- row_trends %>%
  group_by(ExpType) %>%
  summarise(
    MeanTotalDeviation = mean(TotalDeviation, na.rm = TRUE),
    SDTotalDeviation = sd(TotalDeviation, na.rm = TRUE),
    MeanOffsetX = mean(AvgOffsetX, na.rm = TRUE),
    SDOffsetX = sd(AvgOffsetX, na.rm = TRUE),
    MeanOffsetY = mean(AvgOffsetY, na.rm = TRUE),
    SDOffsetY = sd(AvgOffsetY, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary data
print(summary_data)

# Plot mean total deviation by experiment type
ggplot(summary_data, aes(x = ExpType, y = MeanTotalDeviation, fill = ExpType)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = MeanTotalDeviation - SDTotalDeviation,
                    ymax = MeanTotalDeviation + SDTotalDeviation),
                width = 0.2, color = "black") +
  labs(title = "Mean Total Deviation by Experiment Type",
       x = "Experiment Type",
       y = "Mean Total Deviation") +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- LINEAR MODELS FOR EACH EXPERIMENT TYPE ------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Fit linear models to see if Total Deviation depends on Person for each experiment type
models <- row_trends %>%
  group_by(ExpType) %>%
  summarise(
    Model = list(lm(TotalDeviation ~ Person, data = cur_data())),
    .groups = "drop"
  )

# Extract summaries of the models
model_summaries <- models %>%
  mutate(
    R_squared = sapply(Model, function(x) summary(x)$r.squared),
    Formula = sapply(Model, function(x) {
      coef <- coef(x)
      paste0("y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x")
    })
  )

# Print model summaries
print(model_summaries)

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- ANOVA AND TUKEY HSD ANALYSIS ----------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Perform Tukey's HSD test to see where significant differences lie between different AnchorPegX positions
tukey_all <- row_trends %>%
  group_by(ExpType) %>%
  do({
    # ANOVA for TotalDeviation based on AnchorPegX
    anova_model <- aov(TotalDeviation ~ factor(AnchorPegX), data = .)
    tukey_results <- TukeyHSD(anova_model, "factor(AnchorPegX)")$`factor(AnchorPegX)`
    
    # Convert Tukey results to a data frame
    tukey_df <- as.data.frame(tukey_results)
    tukey_df$Comparison <- rownames(tukey_df)
    tukey_df$ExpType <- unique(.$ExpType)
    tukey_df
  }) %>%
  ungroup()

# Clean and format Tukey results
tukey_all <- tukey_all %>%
  select(ExpType, Comparison, diff, lwr, upr, `p adj`) %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Yes", "No"))

# Print the Tukey results
print(tukey_all)

# Plot Tukey's HSD results
ggplot(tukey_all, aes(x = reorder(Comparison, diff), y = diff, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_wrap(~ ExpType, scales = "free_y") +
  coord_flip() +
  labs(title = "Tukey's HSD Results by ExpType",
       x = "Pairwise Comparisons",
       y = "Mean Difference",
       color = "Significant") +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- DEVIATIONS IN X-AXIS ONLY -------------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Summarize deviations in X-axis across columns (AnchorPegX)
column_trends <- data %>%
  group_by(ExpType, AnchorPegX, AnchorPegY) %>%
  summarise(
    AvgDeviationX = mean(OffsetX, na.rm = TRUE),
    AvgDeviationY = mean(OffsetY, na.rm = TRUE),
    TotalDeviation = mean(sqrt(OffsetX^2 + OffsetY^2), na.rm = TRUE),
    SDTotalDeviation = sd(sqrt(OffsetX^2 + OffsetY^2), na.rm = TRUE),
    .groups = "drop"
  )

# Print the column trends
print(column_trends)

# Plot average deviation in X-axis across AnchorPegX
ggplot(column_trends, aes(x = AnchorPegX, y = AvgDeviationX, color = ExpType)) +
  geom_line() +
  labs(title = "Positional Deviations Across Columns",
       x = "AnchorPegX",
       y = "Avg Deviation (X)") +
  theme_minimal()

# Fit linear models for AvgDeviationX by AnchorPegX for each ExpType
lm_models <- column_trends %>%
  group_by(ExpType) %>%
  summarise(
    Model = list(lm(AvgDeviationX ~ AnchorPegX, data = cur_data())),
    .groups = "drop"
  )

# Generate predictions from the linear models
fitted_data <- column_trends %>%
  group_by(ExpType) %>%
  mutate(
    FittedDeviationX = predict(lm(AvgDeviationX ~ AnchorPegX, data = cur_data()))
  ) %>%
  ungroup()

# Plot the original data and the fitted lines
ggplot(fitted_data, aes(x = AnchorPegX, y = AvgDeviationX, color = ExpType)) +
  geom_point(size = 3) +
  geom_line(aes(y = FittedDeviationX), linetype = "dashed", size = 1) +
  labs(title = "Linear Models of Deviation Across AnchorPegX",
       x = "AnchorPegX",
       y = "Average Deviation (X)",
       color = "Experiment Type") +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- MULTIPLE ANOVA ON DEVIATION IN X ------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Split data by experiment type
split_data <- split(row_trends, row_trends$ExpType)

# Perform ANOVA and Tukey HSD test for each experiment type
anova_results <- lapply(split_data, function(sub_data) {
  aov(abs(AvgOffsetX) ~ factor(AnchorPegX), data = sub_data)
})

tukey_results <- lapply(anova_results, TukeyHSD)

# Display Tukey HSD results
for (exp_type in names(tukey_results)) {
  cat("\n### Tukey HSD Results for ExpType:", exp_type, "###\n")
  print(tukey_results[[exp_type]])
}

# Create a boxplot of absolute average offset in X across AnchorPegX
ggplot(row_trends, aes(x = factor(AnchorPegX), y = abs(AvgOffsetX))) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~ ExpType) +
  labs(title = "Absolute AvgOffsetX Across AnchorPegX by ExpType",
       x = "AnchorPegX",
       y = "Absolute AvgOffsetX") +
  theme_minimal()

# Convert Tukey results into a data frame for plotting
tukey_data <- lapply(names(tukey_results), function(exp_type) {
  result <- as.data.frame(tukey_results[[exp_type]]$`factor(AnchorPegX)`)
  result$Comparison <- rownames(result)
  result$ExpType <- exp_type
  result
})

tukey_data <- do.call(rbind, tukey_data)
tukey_data$Significant <- ifelse(tukey_data$`p adj` < 0.05, "Yes", "No")
tukey_data$ExpType <- factor(tukey_data$ExpType, levels = c("circleToDiamond", "midPointTriangle", "diamondToCircle"))

# Plot the Tukey HSD results for AvgOffsetX
ggplot(tukey_data, aes(x = reorder(Comparison, diff), y = diff, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ ExpType, scales = "free_y") +
  coord_flip() +
  labs(title = "Tukey HSD Results for AvgOffsetX Across AnchorPegX",
       x = "Pairwise Comparisons",
       y = "Mean Difference",
       color = "Significant") +
  scale_color_manual(values = c("Yes" = "red", "No" = "gray")) +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- DEVIATIONS IN Y-AXIS ONLY -------------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Similar analysis is performed for Y-axis deviations

# Summarize deviations in Y-axis across columns (AnchorPegX)
column_trends <- data %>%
  group_by(ExpType, AnchorPegX, AnchorPegY) %>%
  summarise(
    AvgDeviationX = mean(OffsetX, na.rm = TRUE),
    AvgDeviationY = mean(OffsetY, na.rm = TRUE),
    TotalDeviation = mean(sqrt(OffsetX^2 + OffsetY^2), na.rm = TRUE),
    SDTotalDeviation = sd(sqrt(OffsetX^2 + OffsetY^2), na.rm = TRUE),
    .groups = "drop"
  )

# Print the column trends
print(column_trends)

# Plot average deviation in Y-axis across AnchorPegX
ggplot(column_trends, aes(x = AnchorPegX, y = AvgDeviationY, color = ExpType)) +
  geom_line() +
  labs(title = "Positional Deviations Across Columns",
       x = "AnchorPegX",
       y = "Avg Deviation (Y)") +
  theme_minimal()

# Fit linear models for AvgDeviationY by AnchorPegY for each ExpType
lm_models <- column_trends %>%
  group_by(ExpType) %>%
  summarise(
    Model = list(lm(AvgDeviationY ~ AnchorPegY, data = cur_data())),
    .groups = "drop"
  )

# Generate predictions from the linear models
fitted_data <- column_trends %>%
  group_by(ExpType) %>%
  mutate(
    FittedDeviationY = predict(lm(AvgDeviationY ~ AnchorPegY, data = cur_data()))
  ) %>%
  ungroup()

# Plot the original data and the fitted lines for Y-axis
ggplot(fitted_data, aes(x = AnchorPegX, y = AvgDeviationY, color = ExpType)) +
  geom_point(size = 3) +
  geom_line(aes(y = FittedDeviationY), linetype = "dashed", size = 1) +
  labs(title = "Linear Models of Deviation Across AnchorPegX",
       x = "AnchorPegX",
       y = "Average Deviation (Y)",
       color = "Experiment Type") +
  theme_minimal()

# Perform ANOVA and Tukey HSD test for Y-axis deviations
split_data <- split(row_trends, row_trends$ExpType)
anova_results <- lapply(split_data, function(sub_data) {
  aov(abs(AvgOffsetY) ~ factor(AnchorPegX), data = sub_data)
})
tukey_results <- lapply(anova_results, TukeyHSD)

# Display Tukey HSD results
for (exp_type in names(tukey_results)) {
  cat("\n### Tukey HSD Results for ExpType:", exp_type, "###\n")
  print(tukey_results[[exp_type]])
}

# Create a boxplot of absolute average offset in Y across AnchorPegX
ggplot(row_trends, aes(x = factor(AnchorPegX), y = abs(AvgOffsetY))) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~ ExpType) +
  labs(title = "Absolute AvgOffsetY Across AnchorPegX by ExpType",
       x = "AnchorPegX",
       y = "Absolute AvgOffsetY") +
  theme_minimal()

# Convert Tukey results into a data frame for plotting
tukey_data <- lapply(names(tukey_results), function(exp_type) {
  result <- as.data.frame(tukey_results[[exp_type]]$`factor(AnchorPegX)`)
  result$Comparison <- rownames(result)
  result$ExpType <- exp_type
  result
})

tukey_data <- do.call(rbind, tukey_data)
tukey_data$Significant <- ifelse(tukey_data$`p adj` < 0.05, "Yes", "No")
tukey_data$ExpType <- factor(tukey_data$ExpType, levels = c("circleToDiamond", "midPointTriangle", "diamondToCircle"))

# Plot the Tukey HSD results for AvgOffsetY
ggplot(tukey_data, aes(x = reorder(Comparison, diff), y = diff, color = Significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ ExpType, scales = "free_y") +
  coord_flip() +
  labs(title = "Tukey HSD Results for AvgOffsetY Across AnchorPegX",
       x = "Pairwise Comparisons",
       y = "Mean Difference",
       color = "Significant") +
  scale_color_manual(values = c("Yes" = "red", "No" = "gray")) +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- ANALYZING DEVIATION ON ANGULAR DIMENSION ----
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #

# Visualize the relationship between MirrorPegX and MirrorPegY, grouped by hand dominance
ggplot(data, aes(x = MirrorPegX, y = MirrorPegY, color = as.factor(AnchorPegX), linetype = hand)) +
  geom_point(alpha = 0.5, aes(shape = hand)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Correlations by AnchorPegX, Overlapping by Hand",
    x = "MirrorPegX",
    y = "MirrorPegY",
    color = "AnchorPegX",
    linetype = "Hand",
    shape = "Hand"
  ) +
  theme_minimal()

# Split data by hand dominance
left_hand_data <- data %>% filter(hand == "left")
right_hand_data <- data %>% filter(hand == "right")

# Fit linear models for each hand group
lm_left <- lm(MirrorPegY ~ MirrorPegX, data = left_hand_data)
lm_right <- lm(MirrorPegY ~ MirrorPegX, data = right_hand_data)

# View model summaries
summary(lm_left)
summary(lm_right)

# Combine data and test interaction effects
data_combined <- data %>%
  mutate(Group = ifelse(hand == "left", "Left-Handed", "Right-Handed"))

# Fit a model with interaction term
combined_lm <- lm(MirrorPegY ~ MirrorPegX * Group, data = data_combined)

# Perform ANOVA to test interaction effects
anova_result <- anova(combined_lm)
summary(anova_result)


# Compute slopes and angles
angles_summary <- data %>%
  group_by(AnchorPegX, hand) %>%
  summarise(
    Slope = lm(MirrorPegY ~ MirrorPegX)$coefficients[2],
    OriginalAngle = atan(Slope) * (180 / pi),
    DeviationAngle = 90 - abs(atan(Slope) * (180 / pi))
  )

# View the angle summary
print(angles_summary)

# Perform ANOVA on deviation angles
anova_result <- aov(DeviationAngle ~ AnchorPegX, data = angles_summary)
summary(anova_result)

# Visualize deviation angles
ggplot(angles_summary, aes(x = as.factor(AnchorPegX), y = DeviationAngle, fill = hand)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Deviation Angles by AnchorPegX and Handedness",
    x = "AnchorPegX",
    y = "Deviation Angle (degrees)",
    fill = "Handedness"
  ) +
  theme_minimal()

# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
#------------- POWER ANALYSIS ------------------------------
# # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #


# Additional calculations using standard errors (SE) and sample size
n <- 10  # Sample size


# SE values from the table (absolute values of the negatives)
se_left_oblique_front <- abs(-0.24)
se_left_hv_front <- abs(-0.24)
se_left_oblique_left <- abs(-0.21)
se_left_hv_left <- abs(-0.24)

se_right_oblique_front <- abs(-0.18)
se_right_hv_front <- abs(-0.19)
se_right_oblique_right <- abs(-0.18)
se_right_hv_right <- abs(-0.27)

# Calculate SD values
sd_left_oblique_front <- se_left_oblique_front * sqrt(n)
sd_left_hv_front <- se_left_hv_front * sqrt(n)
sd_left_oblique_left <- se_left_oblique_left * sqrt(n)
sd_left_hv_left <- se_left_hv_left * sqrt(n)

sd_right_oblique_front <- se_right_oblique_front * sqrt(n)
sd_right_hv_front <- se_right_hv_front * sqrt(n)
sd_right_oblique_right <- se_right_oblique_right * sqrt(n)
sd_right_hv_right <- se_right_hv_right * sqrt(n)

# Print the calculated SDs
cat("SD values:\n")
cat("Left Oblique Front:", sd_left_oblique_front, "\n")
cat("Left HV Front:", sd_left_hv_front, "\n")
cat("Left Oblique Left:", sd_left_oblique_left, "\n")
cat("Left HV Left:", sd_left_hv_left, "\n")
cat("Right Oblique Front:", sd_right_oblique_front, "\n")
cat("Right HV Front:", sd_right_hv_front, "\n")
cat("Right Oblique Right:", sd_right_oblique_right, "\n")
cat("Right HV Right:", sd_right_hv_right, "\n")

# Example for Cohen's d using these SDs
# Define mean offsets for left and right (X-axis as an example)
left_means_x <- c(1.59, 1.82)  # Oblique Front, HV Front
right_means_x <- c(1.36, 1.29) # Oblique Front, HV Front

# Compute mean of means
mean_left_x <- mean(left_means_x)
mean_right_x <- mean(right_means_x)

# Pooled SD for Oblique Front and HV Front
pooled_sd_x <- sqrt((sd_left_oblique_front^2 + sd_right_oblique_front^2) / 2)

# Cohen's d for X-axis
cohen_d_x <- (mean_left_x - mean_right_x) / pooled_sd_x
cat("Cohen's d for X-axis:", cohen_d_x, "\n")
# Perform power analysis with the new effect size
power_analysis <- pwr.t.test(
  d = cohen_d_x,
  sig.level = 0.05,
  power = 0.9,
  type = "two.sample"
)

# Print the power analysis result
print(power_analysis)

