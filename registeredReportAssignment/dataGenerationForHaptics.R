
library(ggplot2)
library(tidyr)

# Set the initial parameters
xPoints <- c(7.5, 22.5, 37.5)
diamondX <- xPoints
diamondY <- 45

triangleX <- xPoints
triangleY <- 30

circleX <- xPoints 
circleY <- 15

numOfRepsPerPoint <- 3
numOfPpl <- 10

globalMean <- 0.1
globalSD <- 3

# Define the data frame structure
data <- data.frame(Person = integer(),
                   Point = character(),
                   AnchorPegX = numeric(),
                   AnchorPegY = numeric(),
                   RefPegX = numeric(),
                   RefPegY = numeric(),
                   MirrorPegX = numeric(),
                   MirrorPegY = numeric(),
                   OffsetX = numeric(),
                   OffsetY = numeric(),
                   Height = numeric(),
                   Wingspan = numeric(),
                   stringsAsFactors = FALSE)

# Loop to generate data for each person
for (person in 1:numOfPpl) {
  height <- rnorm(1, mean = 170, sd = 10)  
  wingspan <- rnorm(1, mean = 180, sd = 20)
  
  # Loop over each repetition of the point
  for (repsOfPoint in 1:numOfRepsPerPoint) {
    
    
    
    offsetX <- rnorm(1, mean = globalMean, sd = globalSD)
    offsetY <- rnorm(1, mean = globalMean, sd = globalSD)
    # Generate MirrorPeg coordinates with offsets
    mirrorPegX <- offsetX - 37.5
    mirrorPegY <- offsetY + 45
    
    # Bind new row to data frame with flat structure for peg coordinates
    data <- rbind(data, data.frame(
      Person = person,
      Point = "circleToDiamond",
      AnchorPegX = -37.5,
      AnchorPegY = 30,
      RefPegX = -37.5,
      RefPegY = 15,
      MirrorPegX = mirrorPegX,
      MirrorPegY = mirrorPegY,
      OffsetX = offestX,
      OffsetY = offestY,
      Height = height,
      Wingspan = wingspan
    ))
  }
  
  
  
  # Loop over each repetition of the point
  for (repsOfPoint in 1:numOfRepsPerPoint) {
    
    
    
    offsetX <- rnorm(1, mean = globalMean, sd = globalSD)
    offsetY <- rnorm(1, mean = globalMean, sd = globalSD)
    # Generate MirrorPeg coordinates with offsets
    mirrorPegX <- offsetX - 22.5
    mirrorPegY <- offsetY + 45
    
    # Bind new row to data frame with flat structure for peg coordinates
    data <- rbind(data, data.frame(
      Person = person,
      Point = "circleToDiamond",
      AnchorPegX = -22.5,
      AnchorPegY = 30,
      RefPegX = -22.5,
      RefPegY = 15,
      MirrorPegX = mirrorPegX,
      MirrorPegY = mirrorPegY,
      OffsetX = offestX,
      OffsetY = offestY,
      Height = height,
      Wingspan = wingspan
    ))
  }
}

# Display the first few rows of the generated data
head(data)



ggplot( data, aes(x = MirrorPegX , y= MirrorPegY   ) ) +
  geom_point(size =3, color = "red")+
  geom_point(size = 2, color = "blue", mapping = aes( AnchorPegX, AnchorPegY )) +
  geom_point(size = 2, color = "green", mapping = aes( RefPegX , RefPegY )) +
  xlim(-50, 50) +
  ylim(0, 50) +
  theme_minimal()  # Apply a minimalistic theme
