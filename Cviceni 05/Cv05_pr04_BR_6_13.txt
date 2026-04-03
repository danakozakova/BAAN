# Clear variables and console
rm(list = ls())
cat("\014")

# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesplot)


# 6.13 ============
# STEP 1: DEFINE the model
bb_model <- "
  data {
    int<lower = 0, upper = 10> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y ~ binomial(10, pi);
    pi ~ beta(3, 8);
  }
"
# (a) STEP 2: SIMULATE the posterior
bb_sim <- stan(model_code = bb_model, data = list(Y = 2), 
               chains = 3, iter = 6000*2, seed = 123456)
# save(bb_sim,file = "sim_cv05_pr04.RData")
# bb_sim se nahraje z predem vytvoreneho objektu - pro ucely ilustrace diagnostik
# load("sim_cv05_pr04.RData")

# (b)
mcmc_trace(bb_sim, pars = "pi", size = 0.1)

# (d)
# Histogram of the Markov chain values
mcmc_hist(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(bb_sim, pars = "pi") + 
  yaxis_text(TRUE) + 
  ylab("density")

# Density plots of individual chains
mcmc_dens_overlay(bb_sim, pars = "pi") + 
  ylab("density")

# Calculate the effective sample size ratio
 neff_ratio(bb_sim, pars = c("pi"))

# autocorrelation
 mcmc_acf(bb_sim, pars = "pi")
 
# R-hat
 rhat(bb_sim, pars = "pi")
 