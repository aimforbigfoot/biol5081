library(ggplot2)
library(tidyr)
library(dplyr)

# Set the initial parameters
xPoints <- c(7.5, 22.5, 37.5)
yPoints <- c(45, 30, 15)
diamondY <- 45
triangleY <- 30
circleY <- 15

numOfRepsPerPoint <- 3
numOfPpl <- 5

globalMean <- 0.1
globalSD <- 1

# Define the data frame structure
data <- data.frame(Person = integer(),
                   Point = character(),
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
# Defining the means and SDs

# Adjusted means and standard deviations


specificCircleMeansX <- c(-2, -1, -0.5, 0.5, 1,2)
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


xPointsLoop <- c(-xPoints[3], -xPoints[2], -xPoints[1], xPoints[3], xPoints[2], xPoints[1])

# Loop to generate data for each person
for (person in 1:numOfPpl) {
  height <- rnorm(1, mean = 170, sd = 30)  
  wingspan <- rnorm(1, mean = 180, sd = 30)
  
  # Calculate scaling factors based on height and wingspan
  heightScalingFactor <- height / 170  # Relative to the average height (170 cm)
  wingspanScalingFactor <- wingspan / 180  # Relative to the average wingspan (180 cm)
  
  # Combine the scaling factors for modifying the means (can adjust this formula as needed)
  scalingFactor <- (heightScalingFactor + wingspanScalingFactor) / 2  # Average of height and wingspan factors
  
  # Loop through each of the three y-levels (diamond, triangle, circle)
  for (h in 1:3) {
    currY <- yPoints[h]
    
    # Use if-else statements to decide y-coordinates and means to use based on h
    if (h == 1) {
      # Assign values for diamondY
      AnchorPegY <- diamondY
      RefPegY <- triangleY
      MirrorPegY <- diamondY
      
      # Means and SDs to use when h == 1 (diamond)
      XmeansToUse <- specificDiamondMeansX * scalingFactor  # Adjust the X means based on scaling factor
      YmeansToUse <- specificDiamondMeansY * scalingFactor  # Adjust the Y means similarly
      XsdsToUse <- specificDiamondSDsX  # SD remains the same for now
      YsdsToUse <- specificDiamondSDsY
    } else if (h == 2) {
      # Assign values for triangleY
      AnchorPegY <- triangleY
      RefPegY <- circleY
      MirrorPegY <- triangleY
      
      # Means and SDs to use when h == 2 (triangle)
      XmeansToUse <- specificTriangleMeansX * scalingFactor
      YmeansToUse <- specificTriangleMeansY * scalingFactor
      XsdsToUse <- specificTriangleSDsX
      YsdsToUse <- specificTriangleSDsY
    } else if (h == 3) {
      # Assign values for circleY
      AnchorPegY <- circleY
      RefPegY <- circleY
      MirrorPegY <- circleY
      
      # Means and SDs to use when h == 3 (circle)
      XmeansToUse <- specificCircleMeansX * scalingFactor
      YmeansToUse <- specificCircleMeansY * scalingFactor
      XsdsToUse <- specificCircleSDsX
      YsdsToUse <- specificCircleSDsY
    }
    
    # Loop through each of the six conditions (different x values)
    for (i in 1:6) {
      for (repsOfPoint in 1:numOfRepsPerPoint) {
        offsetX <- rnorm(1, mean = XmeansToUse[i], sd = XsdsToUse[i])
        offsetY <- rnorm(1, mean = YmeansToUse[i], sd = YsdsToUse[i])
        
        # Generate MirrorPeg coordinates with offsets
        realMirrorPegX <- xPointsLoop[i]
        mirrorPegX <- realMirrorPegX + offsetX
        mirrorPegY <- MirrorPegY + offsetY
        
        # Bind new row to data frame with flat structure for peg coordinates
        data <- rbind(data, data.frame(
          Person = person,
          Point = "circleToDiamond",
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

# Display the first few rows of the generated data
head(data)

# Plot the data
ggplot(data) +
  geom_point(aes(x = MirrorPegX, y = MirrorPegY), color="coral", alpha = 0.5, size = 3) +
  geom_point(aes(x = AnchorPegX, y = AnchorPegY), color = "red", size = 2) +
  geom_point(aes(x = RefPegX, y = RefPegY), size = 2, color =  "darkgreen") +
  geom_point(aes(x = RealMirrorPegX, y = RealMirrorPegY), color = "blue", size = 5, shape = 4, stroke = 2) +
  xlim(-50, 50) +
  ylim(0, 50) +
  theme_minimal() +
  labs(color = "Person")  # Add a legend title for clarity





library(pwr)

# Example: power for repeated-measures ANOVA
# Assume effect size f = 0.25, α = 0.05, 5 participants, 6 conditions
b <- pwr.anova.test(k = 6, f = 0.8, sig.level = 0.05, power = 0.8)
b



library(simr)
head(data)
# Define a mixed model (e.g., offsets modeled by height scaling factor)
model <- lmer(OffsetX ~ Height + (1|Person), data = data)

# Perform a power simulation for the fixed effect (scalingFactor)
powerSim(model, nsim = 100)  # Run 100 simulations
