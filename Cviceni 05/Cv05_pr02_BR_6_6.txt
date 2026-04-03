# Clear variables and console
rm(list = ls())
cat("\014")

# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesplot)

# (a) 
# Step 1: Define a grid of lambda values
grid_data  <- data.frame(
  lambda_grid = seq(from = 0, to = 8, length = 501))
grid_data |> head()

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data <- grid_data %>% 
  mutate(prior = dgamma(lambda_grid, 20, 5),
   likelihood = dpois(0,lambda_grid) * dpois(1,lambda_grid) * dpois(0,lambda_grid))

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
   mutate(unnormalized = likelihood*prior,
      posterior = unnormalized/sum(unnormalized))

ggplot(grid_data, aes(x = lambda_grid, y = posterior)) + 
  geom_point()

ggplot(grid_data, aes(x = lambda_grid, y = posterior)) + 
  geom_point() + 
  geom_segment(aes(x = lambda_grid, xend = lambda_grid, y = 0, yend = posterior))


# Set the seed
set.seed(1234567)

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)


ggplot(post_sample, aes(x = lambda_grid)) + 
  geom_histogram(aes(y = after_stat(density)), colour = "white", binwidth = 0.1) + 
  stat_function(fun = dgamma, args = list(20+1, 5+3)) + 
  lims(x = c(0, 10))
# after_stat je na preskalovanie