# Libraries
library(data.table)

# Functions
f07_read <- function(x) {
    readLines(x) |>
        strsplit(",") |>
        unlist() |>
        as.numeric()
}

f07_sum <- function(x) {

    return(x * (x + 1) / 2)

}

 # Part 1

crab_bois <- f07_read("Day 7/input.txt")

b_loc <- sum(abs(crab_bois - min(crab_bois)))
for(i in min(crab_bois):max(crab_bois)) {

    n_loc <- sum(abs(crab_bois - i))
    
    if(b_loc > n_loc) {
        b_loc = n_loc
        b_loc_i = i
    } 

}

solution <- b_loc

 # Part 1

crab_bois <- f07_read("Day 7/input.txt")

b_loc <- sum(f07_sum(abs(crab_bois - min(crab_bois)))) 
for(i in min(crab_bois):max(crab_bois)) {

    n_loc <- sum(sapply(abs(crab_bois - i), f07_sum))
    
    if(b_loc > n_loc) {
        b_loc = n_loc
        b_loc_i = i
    } 

}

solution <- b_loc

