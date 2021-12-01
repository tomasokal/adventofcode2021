# Libraries
library(data.table)

# Load data
input <- data.table::fread("Day 1/input.csv"
                           , header = FALSE
                           )

# Part 1
output = data.frame(original = as.numeric(input$V1))
output$shift = shift(output$original, -1)
output$check = ifelse(output$shift > output$original, 1, 0)

solution = sum(output$check, na.rm = TRUE)

# Part 2
output = data.table(original = as.numeric(input$V1))
output$rollsum = frollsum(output[, original], 3)
output <- na.omit(output)
output$rollsum_shift = shift(output$rollsum, -1)
output$check = ifelse(output$rollsum_shift > output$rollsum, 1, 0)

solution = sum(output$check, na.rm = TRUE)
