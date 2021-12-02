# Libraries
library(data.table)

# Load data
input <- readLines("Day 2/input.txt") 

# Part 1
output = data.table::data.table(direction = sub(" .*", "", input),
                                amount = as.numeric(sub(".* ", "", input))
                                )

var_f = output[direction == 'forward', sum(amount)] # Sum of amount where direction is forward.
var_u = output[direction == 'up', sum(amount)] # Sum of amount where direction is up.
var_d = output[direction == 'down', sum(amount)] # Sum of amount where direction is down.

pos_horizontal = var_f 
pos_depth = var_d - var_u

solution = pos_horizontal * pos_depth

# Part 2

# Part 1
output = data.table::data.table(direction = sub(" .*", "", input),
                                amount = as.numeric(sub(".* ", "", input))
                                )

output$aim_amount = ifelse(output$direction == 'forward', 0, output$amount) # Aim doesn't change when going forward.
output$aim_amount = ifelse(output$direction == 'up', -1 * output$aim_amount, output$aim_amount) # Aim decreases when direction is up.
output$aim = cumsum(output$aim_amount) # Rolling sum of changes in aim.
output$aim_depth = output$amount * output$aim # Increase depth multiplied by change in aim.

var_f = output[direction == 'forward', sum(amount)]
var_d = output[direction == 'forward', sum(aim_depth)]

pos_horizontal = var_f
pos_depth = var_d

solution = pos_horizontal * pos_depth
