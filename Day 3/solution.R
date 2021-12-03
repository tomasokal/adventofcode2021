# Libraries
library(data.table)

# Load data
input <- readLines("Day 3/input.txt") 

# Part 1
output <- data.table::data.table(do.call(rbind, strsplit(input, "")))

cols = names(output)
output <- output[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
output <- output[ , (cols) := lapply(.SD, sum), .SDcols = cols][1, ]
output <- round(output / length(input))

output_gamma = output
output_epsilon = abs(1-output)

output_gamma <- output_gamma[, x := Reduce(function(...) paste(..., sep = ""), .SD[, mget(cols)])][, x]
output_epsilon <- output_epsilon[, x := Reduce(function(...) paste(..., sep = ""), .SD[, mget(cols)])][, x]

solution = strtoi(output_gamma, base = 2) * strtoi(output_epsilon, base = 2)

# Part 2
output <- data.table::data.table(do.call(rbind, strsplit(input, "")))

i_output <- output
for(i in 1:(ncol(output)-1)) {

    i_col <- names(output)[i]
    i_common <- i_output[, get(i_col)]
    i_common <- names(sort(table(i_common))[2])
    
    i_output <- i_output[get(i_col) == i_common, ]

}

output_o2 = i_output

i_output <- output
for(i in 1:(ncol(output)-1)) {

    i_col <- names(output)[i]
    i_common <- i_output[, get(i_col)]
    i_common <- names(sort(table(i_common))[1])
    
    i_output <- i_output[get(i_col) == i_common, ]

}

output_co2 = i_output

output_o2 <- output_o2[, x := Reduce(function(...) paste(..., sep = ""), .SD[, mget(cols)])][, x]
output_co2 <- output_co2[, x := Reduce(function(...) paste(..., sep = ""), .SD[, mget(cols)])][, x]

solution = strtoi(output_o2, base = 2) * strtoi(output_co2, base = 2)
