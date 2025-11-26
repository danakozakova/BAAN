# https://www.bayesrulesbook.com/chapter-9
# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)

plot_normal(mean = 5000, sd = 1000) + 
  labs(x = "beta_0c", y = "pdf")
plot_normal(mean = 100, sd = 40) + 
  labs(x = "beta_1", y = "pdf")
plot_gamma(shape = 1, rate = 0.0008) + 
  labs(x = "sigma", y = "pdf")