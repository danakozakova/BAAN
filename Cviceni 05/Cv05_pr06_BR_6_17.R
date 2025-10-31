# Clear variables and console
rm(list = ls())
cat("\014")

# Load packages
library(tidyverse)
library(janitor)
library(rstan)
library(bayesplot)


# 6.17 ============
# STEP 1: DEFINE the model
nn_model <- "
  data {
    real Y[4];
  }
  parameters {
    real mu;
  }
  model {
    Y ~ normal(mu, 1.3^2);
    mu ~ normal(10, 1.2^2);
  }
"
# (a) STEP 2: SIMULATE the posterior
nn_sim <- stan(model_code = nn_model, data = list(Y = c(7.1,8.9,8.4,8.6)), 
               chains = 4, iter = 5000*2, seed = 123456)
# save(nn_sim,file = "sim_cv05_pr06.RData")
# nn_sim se nahraje z predem vytvoreneho objektu - pro ucely ilustrace diagnostik
# load("sim_cv05_pr06.RData")


# (b)
mcmc_trace(nn_sim, pars = "mu", size = 0.1)

# (d)
# Histogram of the Markov chain values
mcmc_hist(nn_sim, pars = "mu") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(nn_sim, pars = "mu") + 
  yaxis_text(TRUE) + 
  ylab("density")

# Density plots of individual chains
mcmc_dens_overlay(nn_sim, pars = "mu") + 
  ylab("density")

# Calculate the effective sample size ratio
 neff_ratio(nn_sim, pars = c("mu"))

# autocorrelation
 mcmc_acf(nn_sim, pars = "mu")
 
# R-hat
 rhat(nn_sim, pars = "mu")