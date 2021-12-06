# Libraries
library(data.table)

# Load data
input <- readLines("Day 5/input.txt") 

# Part 1

output <- data.table::data.table(input)
output[, c('start', 'end') := tstrsplit(input, " -> ", fixed = TRUE)]
output[, c('start_x', 'start_y') := tstrsplit(start, ",", fixed = TRUE)]
output[, c('end_x', 'end_y') := tstrsplit(end, ",", fixed = TRUE)]
output <- output[start_x == end_x | start_y == end_y, .(start_x, start_y, end_x, end_y)]

output_list <- split(output, seq(nrow(output)))

draw_line <- function(x){

    output_matrix <- matrix(0, nrow = 1000, ncol = 1000)
    output_matrix[as.numeric(x$start_x):as.numeric(x$end_x), as.numeric(x$start_y):as.numeric(x$end_y)] <- 1
    
    return(output_matrix)

}

matrix_list <- lapply(output_list, draw_line)
thermal_map <- Reduce('+', matrix_list)

solution <- sum(as.vector(thermal_map) > 1)

# Part 2

output <- data.table::data.table(input)
output[, c('start', 'end') := tstrsplit(input, " -> ", fixed = TRUE)]
output[, c('start_x', 'start_y') := tstrsplit(start, ",", fixed = TRUE)]
output[, c('end_x', 'end_y') := tstrsplit(end, ",", fixed = TRUE)]
output <- output[, .(start_x, start_y, end_x, end_y)]
output[, names(output) := lapply(.SD, as.numeric)]

output_list <- split(output, seq(nrow(output)))

draw_line_better <- function(x){

    output_matrix <- matrix(0, nrow = 1000, ncol = 1000)

    d_side = (min(x$start_y, x$end_y))
    u_side = (max(x$start_y, x$end_y))

    l_side = (min(x$start_x, x$end_x))
    r_side = (max(x$start_x, x$end_x))

    if(x$start_x == x$end_x | x$start_y == x$end_y) {

        output_matrix[x$start_x:x$end_x, x$start_y:x$end_y] <- 1

    } else if((x$start_x < x$end_x & x$start_y < x$end_y) | (x$start_x > x$end_x & x$start_y > x$end_y)) {

        print('working')

        for(i in seq_along(l_side:r_side)){

            output_matrix[(l_side+i-1), (d_side+i-1)] <- 1

        }

    } else {

        print('working')

        for(i in seq_along(l_side:r_side)){

            output_matrix[(l_side+i-1), (u_side-i+1)] <- 1

        }

    }

    return(output_matrix)

}

matrix_list <- lapply(output_list, draw_line_better)
thermal_map <- Reduce('+', matrix_list)

solution <- sum(as.vector(thermal_map) > 1)
