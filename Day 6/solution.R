# Libraries
library(data.table)

# Functions

f06_read <- function(x) {
    readLines(x) |>
        strsplit(",") |>
        unlist() |>
        as.numeric()
}

f06_part1 <- function(x, days) {
    for (i in 1:days) {
        if (sum(x == 0) > 0) {
            new_fish <- sum(x == 0)
            x <- x - 1
            x[x == -1] <- 6
            x <- c(x, rep(8, new_fish))
        } else {
            x <- x - 1
        }
    }
    return(x)
}

f06_part2 <- function(x, days) {

    fish_tally <- c(0, tabulate(x, nbins = 8)) # Counts of fish by time till new fish

    for(i in 1:days) {

        new_fish = fish_tally[1] # Count of new fish
        fish_tally = c(fish_tally[2:9], 0) # Shift fish to left
        fish_tally[7] = fish_tally[7] + new_fish # Reset current fish to time 6
        fish_tally[9] = fish_tally[9] + new_fish # Add new fish to time 8

    }

    return(sum(fish_tally))

}

# Part 1

solution_part1 <- length(f06_part1(f06_read("Day 6/input.txt"), 80))

# Part 2

solution_part2 <- f06_part2(f06_read("Day 6/input.txt"), 256)

