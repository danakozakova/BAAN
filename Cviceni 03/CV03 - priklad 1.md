# Cv 03 - Priklad 1
# https://docs.google.com/document/d/1afVsfaO8tmnBwlWWLkIX3_QfdK4rdNC4l47lvu3Xot8/edit?tab=t.0#heading=h.pp4dsgvpua7r
# https://www.bayesrulesbook.com/chapter-3#exercises-2

rm(list = ls())
cat("\014")
library(bayesrules)
library(janitor)

# Exercise 3.2 (Tune your Beta prior: Take II) As in Exercise 3.1, tune an appropriate Beta(α,β) 
# prior model for each situation below.
# a. Your friend tells you “I think that I have a 80% chance of getting a full night of sleep tonight, and I am pretty certain.” 
# When pressed further, they put their chances between 70% and 90%.
# E(pi) 
E <- 0.8

# pozriu kapitolu 3.1
# E(pi) = alfa /(alfa + beta)
# rozptyl .. zlozitejsi vzore
# ideme tipovat

alpha <- 40
beta <- (alpha/E) - alpha

plot_beta(alpha, beta, mean = TRUE, mode = TRUE)
summarize_beta(alpha, beta)

# Kontrola 5% a 95% percentilov (90% interval)
lower <- qbeta(0.05, alpha, beta)
upper <- qbeta(0.95, alpha, beta)
print(paste("90% CI: [", round(lower, 3), ",", round(upper, 3), "]"))

## druhy pokus
alpha <- 30
beta <- (alpha/E) - alpha

plot_beta(alpha, beta, mean = TRUE, mode = TRUE)
summarize_beta(alpha, beta)

# Kontrola 5% a 95% percentilov (90% interval)
lower <- qbeta(0.05, alpha, beta)
upper <- qbeta(0.95, alpha, beta)
print(paste("90% CI: [", round(lower, 3), ",", round(upper, 3), "]"))

## uniformne rozdelenie
alpha <- 1
beta <- 1
plot_beta(alpha, beta, mean = TRUE, mode = TRUE)
summarize_beta(alpha, beta)
