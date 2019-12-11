# Advent of code day 1 2019

library(tidyverse)

# Part 1

my_data <- read_csv("input/day1_part1_input.txt", col_names = FALSE) %>%
  rename(mass = X1) %>%
  mutate(fuel = floor(mass/3) - 2)

part1 <- sum(my_data$fuel)
part1


# Part 2

ultimate_fuel <- function (my_mass){
  add_mass <- 0
  add_fuel <- floor(my_mass/3) - 2
  if (add_fuel <= 0){
    return(0)
  } else{
    while (add_fuel > 0){
      add_mass <- add_mass + add_fuel
      add_fuel <- floor(add_fuel/3) - 2
    }
    return(add_mass)
  }
}

my_data$total_fuel <- mapply(ultimate_fuel, my_data$mass)

part2 <- sum(my_data$total_fuel)
part2
