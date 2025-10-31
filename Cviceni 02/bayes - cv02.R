# priklad 2.13

# 2.3.7 Posterior simulation
#https://www.bayesrulesbook.com/chapter-2#cousin-cole

rm(list = ls())
cat("\014")

# libraries
# install.packages("bayesrules")
# install.packages("tidyverse")

library(bayesrules)
library(tidyverse)
library(janitor)
library(dplyr)

# Define possible win probabilities
lactose <- data.frame(pi = c(0.4, 0.5, 0.6, 0.7))

# Define the prior model
prior <- c(0.10, 0.2, 0.44, 0.26)

# Simulate 10000 values of pi from the prior
set.seed(84735)
lactose_sim <- sample_n(lactose, size = 10000, weight = prior, replace = TRUE)


# Simulate 10000 match outcomes
lactose_sim <- lactose_sim %>% 
  mutate(y = rbinom(10000, size = 80, prob = pi))

# Check it out
lactose_sim %>% 
  head(3)
