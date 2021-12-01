# Libraries
library(data.table)

# Load data
input <- readLines("Day 1/input.txt") |>
  as.numeric()

# Part 1
output = data.frame(original = input)
output$shift = shift(output$original, -1)
output$check = ifelse(output$shift > output$original, 1, 0)

solution = sum(output$check, na.rm = TRUE)

# Part 2
output = data.table(original = input)
output$rollsum = frollsum(output[, original], 3)
output <- na.omit(output)
output$rollsum_shift = shift(output$rollsum, -1)
output$check = ifelse(output$rollsum_shift > output$rollsum, 1, 0)

solution = sum(output$check, na.rm = TRUE)
