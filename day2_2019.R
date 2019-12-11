# Advent of code day 2 2019

library(tidyverse)

# Part 1

# Read data into vector
initial_data <- readLines("input/day2_input.txt")
#my_data <- readLines("input/day2_test.txt")

initial_data <- as.integer(unlist(strsplit(initial_data[1], split=",")))
my_data <- initial_data
# intcode computer function

intcode <- function(my_program, step = 4){
  position_curr <- 1 # Rememember to subtract 1 from the indices later on
  opcode_curr <- my_program[position_curr]
  while (opcode_curr != 99 & position_curr < length(my_program)) {
    param1 = my_program[position_curr+1] + 1
    param2 = my_program[position_curr+2] + 1
    param3 = my_program[position_curr+3] + 1
    if (opcode_curr == 1) {
      my_program[param3] <- my_program[param1] + my_program[param2]
    }
    if (opcode_curr == 2) {
      my_program[param3] <- my_program[param1] * my_program[param2]
    } 
    position_curr = position_curr + step
    opcode_curr <- my_program[position_curr]
  }
  return(my_program)
}

# before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.

my_data[2] <- 12
my_data[3] <- 2

a = intcode(my_data, step = 4)
part1 <- a[1]
part1

# In part 2 we need to replace positions 1 and 2 with various things and test the output
# They can't be greater than the length of the program (minus 1)



  for (i in 0:99) {
    for (j in 0:99) {
      my_data <- initial_data
      my_data[2] <- i
      my_data[3] <- j
      a = intcode(my_data, step = 4)
    }
    if(a[1] == 19690720){
      print(i)
    }
  }




