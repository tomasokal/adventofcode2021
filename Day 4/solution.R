# Libraries
library(data.table)
library(greenbrown)

# Load data
input <- readLines("Day 4/input.txt") 

# Part 1

output <- split(input, data.table::rleid(input))
selections <- as.vector(unlist(output[1]))
selections <- gsub('"', '', selections)

selections <- strsplit(output[[1]], ",")[[1]]

bingo_boards <- output[-1]
bingo_boards <- bingo_boards[-(seq(1, length(bingo_boards), 6))]

create_bingo_board <- function(x) {

    x = unlist(x)
    x = trimws(x)
    x = gsub("  ", " ", x)
    x = strsplit(x, " ")
    x = unlist(x)
    x = matrix(x, ncol = 5, nrow = 1)
    x = data.table::as.data.table(x)
    return(x)

}

bingo_boards <- data.table::rbindlist(lapply(bingo_boards, create_bingo_board))
bingo_boards <- split(bingo_boards, 0:nrow(bingo_boards) %/% 5)
bingo_boards <- bingo_boards[-101]

check_rows <- function(x){

    r1 = length(unique(as.numeric(x[1, ]))) == 1
    r2 = length(unique(as.numeric(x[2, ]))) == 1
    r3 = length(unique(as.numeric(x[3, ]))) == 1
    r4 = length(unique(as.numeric(x[4, ]))) == 1
    r5 = length(unique(as.numeric(x[5, ]))) == 1

    return(r1 || r2 || r3 || r4 || r5)
}

check_cols <- function(x){
    
    c1 = length(unique(as.numeric(x[, 1]))) == 1
    c2 = length(unique(as.numeric(x[, 2]))) == 1
    c3 = length(unique(as.numeric(x[, 3]))) == 1
    c4 = length(unique(as.numeric(x[, 4]))) == 1
    c5 = length(unique(as.numeric(x[, 5]))) == 1

    return(c1 || c2 || c3 || c4 || c5)
}

play_bingo_board <- function(x) {

    x = as.data.frame(x)
    val = NULL

    for(num in seq_along(selections)){

        x[x == selections[num]] <- -99
        if (check_rows(x) || check_cols(x)){
            val = num
            break
        }

    }

    return(val)

}

results_bingo <- lapply(bingo_boards, play_bingo_board)
best_bingo_board <- as.data.frame(bingo_boards[which(results_bingo == Reduce(min, results_bingo))])

best_bingo_board_matrix <- NULL 
final_selection <- NULL

for(num in seq_along(selections)){

        best_bingo_board[best_bingo_board == selections[num]] <- 0
        if (check_rows(best_bingo_board) || check_cols(best_bingo_board)){
            best_bingo_board_matrix = best_bingo_board
            final_selection = as.numeric(selections[num])
            break
        }

    }

solution <- sum(as.numeric(unique(as.vector(as.matrix(best_bingo_board_matrix))))) * final_selection

# Part 2

output <- split(input, data.table::rleid(input))
selections <- as.vector(unlist(output[1]))
selections <- gsub('"', '', selections)

selections <- strsplit(output[[1]], ",")[[1]]

bingo_boards <- output[-1]
bingo_boards <- bingo_boards[-(seq(1, length(bingo_boards), 6))]

create_bingo_board <- function(x) {

    x = unlist(x)
    x = trimws(x)
    x = gsub("  ", " ", x)
    x = strsplit(x, " ")
    x = unlist(x)
    x = matrix(x, ncol = 5, nrow = 1)
    x = data.table::as.data.table(x)
    return(x)

}

bingo_boards <- data.table::rbindlist(lapply(bingo_boards, create_bingo_board))
bingo_boards <- split(bingo_boards, 0:nrow(bingo_boards) %/% 5)
bingo_boards <- bingo_boards[-101]

check_rows <- function(x){

    r1 = length(unique(as.numeric(x[1, ]))) == 1
    r2 = length(unique(as.numeric(x[2, ]))) == 1
    r3 = length(unique(as.numeric(x[3, ]))) == 1
    r4 = length(unique(as.numeric(x[4, ]))) == 1
    r5 = length(unique(as.numeric(x[5, ]))) == 1

    return(r1 || r2 || r3 || r4 || r5)
}

check_cols <- function(x){
    
    c1 = length(unique(as.numeric(x[, 1]))) == 1
    c2 = length(unique(as.numeric(x[, 2]))) == 1
    c3 = length(unique(as.numeric(x[, 3]))) == 1
    c4 = length(unique(as.numeric(x[, 4]))) == 1
    c5 = length(unique(as.numeric(x[, 5]))) == 1

    return(c1 || c2 || c3 || c4 || c5)
}

play_bingo_board <- function(x) {

    x = as.data.frame(x)
    val = NULL

    for(num in seq_along(selections)){

        x[x == selections[num]] <- -99
        if (check_rows(x) || check_cols(x)){
            val = num
            break
        }

    }

    return(val)

}

results_bingo <- lapply(bingo_boards, play_bingo_board)
worst_bingo_board <- as.data.frame(bingo_boards[which(results_bingo == Reduce(max, results_bingo))])

worst_bingo_board_matrix <- NULL 
final_selection <- NULL

for(num in seq_along(selections)){

        worst_bingo_board[worst_bingo_board == selections[num]] <- 0
        if (check_rows(worst_bingo_board) || check_cols(worst_bingo_board)){
            worst_bingo_board_matrix = worst_bingo_board
            final_selection = as.numeric(selections[num])
            break
        }

    }

solution <- sum(as.numeric(unique(as.vector(as.matrix(worst_bingo_board_matrix))))) * final_selection