#### Preamble ####
# Purpose: Simulates dataset about house price in Nanjing
# Author: Yufei Liu
# Date: 22 Nov 2023
# Contact: florence.liu@mail.utoronto.ca
# License: MIT

#### Data expectations ####
# Total_Price would be positive in range 10 to 7000 with more weights in range 200 to 1000
# Unit_Price would be positive in range 1000 to 100000 with more weights in range 5000 to 70000
# Areas would be positive in range 20 to 1000
# District would be characters with 12 unique values
# Furnished would be characters with 4 unique values
# Bedroom, Living_Room, Total_Floors, Detailed_Floors would be positive integers
# Facing_South would be a dummy variable with 0 representing facing south


#### Workspace setup ####
library(tidyverse)


#### Simulate data ####
set.seed(777) # set a random seed for simulation
n <- 500 # sample size of simulation

### Simulate Total_Price
sim_gamma_total_price <- rgamma(n, scale = 300, shape = 2) # Simulate gamma distribution
sim_total_price <- pmax(10, pmin(7000, sim_gamma_total_price)) # Adjust the simulated values to the desired range
sim_total_price <- as.integer(sim_total_price) # Convert to integers

### Simulate Unit_Price
sim_gamma_unit_price <- rgamma(n, scale = 10000, shape = 2) # Simulate gamma distribution
sim_unit_price <- pmax(1000, pmin(100000, sim_gamma_unit_price)) # Adjust the simulated values to the desired range
sim_unit_price <- as.integer(sim_unit_price) # Convert to integers

### Simulate Area
sim_gamma_area <- rgamma(n, scale = 30, shape = 2) # Simulate gamma distribution
sim_area <- pmax(20, pmin(1000, sim_gamma_area)) # Adjust the simulated values to the desired range

### Simulate District
district <- c('gulou', 'gaochun', 'jiangning', 'jianye', 'jurong', 'lishui', 'liuhe', 'pukou', 'qinhuai', 'qixia', 'xuanwu', 'yuhuatai')
sim_district <- sample(district, n, replace = TRUE)

### Simulate Furnished
furnished <- c("Fully Furnished", "Part Furnished", "Not Furnished", "Other")
sim_furnished <- sample(furnished, n, replace = TRUE)

### Simulate Bedroom
bedroom_num <- c(0:10)
prob <- c(0.1, 0.2, 0.3, 0.2, 0.1, 0.05, 0.02, 0.02, 0.005, 0.005, 0.005)
sim_bedroom <- sample(bedroom_num, n, prob = prob, replace = TRUE)

### Simulate Living_Room
livingroom_num <- c(0:10)
prob_livingroom <- c(0.2, 0.2, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01, 0.001, 0.001, 0.001)
sim_livingroom <- sample(livingroom_num, n, prob = prob_livingroom, replace = TRUE)

### Simulate Total_Floors
total_floor_num <- c(1:60)
sim_total_floor <- sample(total_floor_num, n, replace = TRUE)

### Simulate Detailed_Floor
sim_detailed_floor <- runif(n, max = 60, min = 1)

### Simulate Facing_South
prob_south <- 0.45
sim_facing_south <- as.factor(rbinom(n, size = 1, prob = prob_south))

### Generate simulated dataset
simulated_data <- data.frame(sim_total_price, sim_unit_price, sim_district, 
                             sim_area, sim_furnished, sim_bedroom, 
                             sim_livingroom, sim_total_floor, 
                             sim_detailed_floor, sim_facing_south)


#### Test data ####

### Check distribution via histograms and barplots
hist(simulated_data$sim_total_price)
hist(simulated_data$sim_unit_price)
hist(simulated_data$sim_area)
hist(simulated_data$sim_bedroom)
hist(simulated_data$sim_livingroom)
hist(simulated_data$sim_total_floor)
hist(simulated_data$sim_detailed_floor)
barplot(table(simulated_data$sim_facing_south))
barplot(table(simulated_data$sim_district))
barplot(table(simulated_data$sim_furnished))

### Check variable types
simulated_data$sim_total_price |> class() == "integer"
simulated_data$sim_unit_price |> class() == "integer"
simulated_data$sim_district |> class() == "character"
simulated_data$sim_area |> class() == "numeric"
simulated_data$sim_furnished |> class() == "character"
simulated_data$sim_bedroom |> class() == "integer"
simulated_data$sim_livingroom |> class() == "integer"
simulated_data$sim_total_floor |> class() == "integer"
simulated_data$sim_detailed_floor |> class() == "numeric"
simulated_data$sim_facing_south |> class() == "factor"


### Check the unique values in District and Furnished
simulated_data$sim_district |> unique()
simulated_data$sim_furnished |> unique()


### Check Total price and Area are positive
simulated_data$sim_total_price |> min() > 0
simulated_data$sim_unit_price |> min() > 0
simulated_data$sim_area |> min() > 0


#### Save data ####
write_csv(simulated_data, "output/data/simulated_data.csv")
