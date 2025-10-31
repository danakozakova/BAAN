# Cv 04 - Priklad 3 - exercise 5.5 + exercise 5.6
# https://docs.google.com/document/d/1afVsfaO8tmnBwlWWLkIX3_QfdK4rdNC4l47lvu3Xot8/edit?tab=t.0#heading=h.pp4dsgvpua7r
# https://www.bayesrulesbook.com/chapter-5#exercises-4

rm(list = ls())
cat("\014")
library(bayesrules)
library(janitor)

# Exercise 5.5 (Text messages) Let random variable λ represent the rate of text messages people 
# receive in an hour. At first, you believe that the typical number of messages per hour is 5 with a standard deviation of 0.25 messages.
# a. Tune and plot an appropriate Gamma(s, r) prior model for λ
# b. What is the prior probability that the rate of text messages per hour is larger than 10? Hint: learn about pgamma().

################################################
# Riesenie

# a. Tune and plot an appropriate Gamma(s, r) prior model for λ
# nastavit priorny model
# pozor iba v ramci Poissonovho rozdelenia plati, ze stredna hodnota = rozptylu

# prior model bude Gamma
# v zadaní je predpoklad, že stredna hodnota je 5 a odchýlka je 0,25
# rozptyl = 0.25^2 = 0.0625
# t.j. s/r = 5 a s/r^2 = 0.0625^2
# čiže s = 5*r; s = 0,0625*r^2
# 5*r = 0,0625*r^2
# 80*r = r^2
# r = 80, s = 400

# vseobecne:
# čiže s = E*r; s = Var*r^2
# E*r = Var*r^2
# (E/var) * r = r^2
# r = E/Var, s = E^2/Var
E <- 5
SD <- 0.25
Var <- SD^2

r <- E/Var
s <- E*r

# vizualna kontrola (plot) a vypoctova kontrola nastaveneho prioru
plot_gamma(shape = s, rate = r, mean = TRUE, mode = TRUE)
summarize_gamma(s, r)

# b. Pravdepodobnost v tomto modeli Gamma P(λ > 10)
# What is the prior probability that the rate of text messages per hour is larger than 10? Hint: learn about pgamma()
pgamma(10, shape = s, rate = r, lower.tail = FALSE)
# pravdepodobnost je 9.813974e-56

pgamma(5, shape = s, rate = r, lower.tail = FALSE)
pgamma(5, shape = s, rate = r, lower.tail = TRUE) 
  # nie je symetricky, ale priblizne.. 

################################################
# Exercise 5.6 (Text messages with data) Continuing with Exercise 5.5, you collect data from six friends. 
# They received 7, 3, 8, 9, 10, 12 text messages in the previous hour.
# Plot the resulting likelihood function of λ.Plot the prior pdf, likelihood function, and the posterior pdf of λ.
# Use summarize_gamma_poisson() to calculate descriptive statistics for the prior and the posterior models of λ.
# Comment on how your understanding about λ changed from the prior (in the previous exercise) to the posterior based on the data 
# you collected from your friends.
################################################

# Datovy model
y <- c(7, 3, 8, 9, 10, 12)
## Vierohodnostna funkcia
plot_poisson_likelihood(y = y, lambda_upper_bound = 20) # vierohodnost: pro rozne lambda, aka je pravdepodobnost nasich dat
  # 20 je horne ohranicenie, nejaky odhad

# Aposteriorny model
plot_gamma_poisson(shape = s, rate = r,
                   sum_y = sum(y), n = length(y))

# Porovnanie prior a posterior
summarize_gamma_poisson(shape = s, rate = r,
                        sum_y = sum(y), n = length(y))


# len posteriorny model
plot_gamma(shape = s + sum(y), rate = r + length(y), mean = TRUE, mode = TRUE)
summarize_gamma(s + sum(y), r + length(y))


################################################
# pokracujeme v dalsom uceni

################################################

mod_1 <- summarize_gamma_poisson(shape = s, rate = r,
                        sum_y = sum(y), n = length(y))
y2 <- c(6, 4, 10, 12, 1, 0, 5, 9, 13, 8)

# z povodneho posterior sa stane prior, vytiahnem si jeho shape a rate
mod_1$shape # vrati oba shapes
s_2 <- mod_1$shape[2] #novy shape
r_2 <- mod_1$rate[2] #novy rate

# Porovnanie noveho prior a posterior
plot_gamma_poisson(shape = s_2, rate = r_2,
                   sum_y = sum(y2), n = length(y2))

mod_2 <- summarize_gamma_poisson(shape = s_2, rate = r_2,
                        sum_y = sum(y2), n = length(y2))
mod_2 


###################################
plot_normal_normal()
nazvy pozri https://www.bayesrulesbook.com/chapter-5#exercises-4