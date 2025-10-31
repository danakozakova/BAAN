# Clear variables and console
rm(list = ls())
cat("\014")

# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesplot)

# (a) 
# Step 1: Define a grid of pie values
grid_data  <- data.frame(
  pie_grid = seq(from = 0, to = 1, length = 501))
grid_data |> head()

# Step 2: Evaluate the prior & likelihood at each pie
# 2.a My prior function =  normal, but only from 0 to 1
my_prior <- function(x, mu, sigma) { # v akej hodnote, aka stredna hodnota, aky rozptyl
  dnorm(x, mean = mu, sd = sigma)
}

# mozeme dat aj tu obmedzenie, ze x ma zmysel len od 0 do 1, inak je to nastavene aj v mriezke, lebo ine hodnoty tam nie su
# cize predefinujme
my_prior <- function(x, mu, sigma) { # v akej hodnote, aka stredna hodnota, aky rozptyl
  if (x < 0 || x > 1 ) return (0)
  dnorm(x, mean = mu, sd = sigma)
}

# ale obsah pod hustotou nie je jedna, treba to normalizovat
# cize este predefinujme
my_prior <- function(x, mu, sigma) { # v akej hodnote, aka stredna hodnota, aky rozptyl
  if (x < 0 || x > 1 ) return (0)
  # odseknute casti pod 0 a nad 1 - tato cast chyba do obsahu 1 potom
  p_low  <- pnorm(0, mean = mu, sd = sigma, lower.tail = TRUE)
  p_upper  <- pnorm(0, mean = mu, sd = sigma, lower.tail = TRUE)
  # normalizujeme, t.j. prenasobit 1/obsah bez odseknutych casti
  dnorm(x, mean = mu, sd = sigma)*1/(1-p_low-p_upper)
}

# 2.b My likehood - binomicke
my_like <- function(y, n, pie) {
  dbinom(x = y, size = n, prob = pie)
}

DOKONC...
pie z grid
y = 5
n = 8

grid_data <- grid_data

grid_data <- grid_data %>% 
  mutate(prior = dgamma(lambda_grid, 20, 5),
         likelihood = dpois(0,lambda_grid) * dpois(1,lambda_grid) * dpois(0,lambda_grid))


