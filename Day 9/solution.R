# Libraries
library(data.table)

# Functions

f09_read <- function(x) {
    readLines(x) |>
        as.data.frame()
}

f09_minimum <- function(x) {

    which((x - data.table::shift(x, 1) < 0 | is.na(x - data.table::shift(x, 1))) & (x - data.table::shift(x, 1, type = 'lead') < 0 | is.na(x - data.table::shift(x, 1, type = 'lead'))))

}

# Part 1

solution <- setDT(f09_read('Day 9/input.txt'))[, paste0("var", 1:100) := tstrsplit(`readLines(x)`, "")][, `readLines(x)` := NULL]
solution <- sapply(solution, as.numeric)

row_mins <- apply(solution, 1, f09_minimum)
col_mins <- apply(solution, 2, f09_minimum)

mat <- matrix(0, nrow = 100, ncol = 100)

for(i in 1:length(row_mins)) {
    for(j in 1:length(row_mins[[i]])){
        row_min_col = row_mins[[i]][j]
        mat[i, row_min_col] = mat[i, row_min_col] + 1
    }
}

for(i in 1:length(col_mins)) {
    for(j in 1:length(col_mins[[i]])){
        col_min_row = col_mins[[i]][j]
        mat[col_min_row, i] = mat[col_min_row, i] + 1
    }
}

mat_df <- as.data.frame(mat)
mat_df[mat_df == 1] <- 0
mat_df[mat_df == 2] <- TRUE

sum(solution[mat_df == TRUE] + 1)

