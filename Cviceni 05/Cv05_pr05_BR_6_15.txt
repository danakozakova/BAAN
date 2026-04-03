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
gp_model <- "
  data {
    int<lower = 0> Y[3];
  }
  parameters {
    real<lower = 0> lambda;
  }
  model {
    Y ~ poisson(lambda);
    lambda ~ gamma(20, 5);
  }
"
# (a) STEP 2: SIMULATE the posterior
gp_sim <- stan(model_code = gp_model, data = list(Y = c(0,1,0)), 
               chains = 4, iter = 5000*2, seed = 123456)
# save(gp_sim,file = "sim_cv05_pr05.RData")
# gp_sim se nahraje z predem vytvoreneho objektu - pro ucely ilustrace diagnostik
# load("sim_cv05_pr05.RData")

# (b)
mcmc_trace(gp_sim, pars = "lambda", size = 0.1)

# (d)
# Histogram of the Markov chain values
mcmc_hist(gp_sim, pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(gp_sim, pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("density")

# Density plots of individual chains
mcmc_dens_overlay(gp_sim, pars = "lambda") + 
  ylab("density")

# Calculate the effective sample size ratio
 neff_ratio(gp_sim, pars = c("lambda"))

# autocorrelation
 mcmc_acf(gp_sim, pars = "lambda")
 
# R-hat
 rhat(gp_sim, pars = "lambda")