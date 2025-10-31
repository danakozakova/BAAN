# Cv 03 - Priklad 4 - exercise 3.13
# https://docs.google.com/document/d/1afVsfaO8tmnBwlWWLkIX3_QfdK4rdNC4l47lvu3Xot8/edit?tab=t.0#heading=h.pp4dsgvpua7r
# https://www.bayesrulesbook.com/chapter-3#exercises-2

rm(list = ls())
cat("\014")
library(bayesrules)
library(janitor)

# Exercise 3.13 (Knowing someone who is transgender) A September 2016 Pew Research survey found that 30% of U.S. adults are aware
# that they know someone who is transgender.It is now the 2020s, and Sylvia believes that the current percent of people who know someone
# who is transgender,π, has increased to somewhere between 35% and 60%.
# a. Identify and plot a Beta model that reflects Sylvia’s prior ideas about π.

# definicie apriorniho modelu
# ako pre priklad 1 - exercise 3.2
# E(pi) 
E <- (0.35 + 0.6)/2

# E(pi) = alfa /(alfa + beta)
# rozptyl .. zlozitejsi vzorec
# ideme tipovat
alpha <- 20
beta <- (alpha/E) - alpha

plot_beta(alpha, beta, mean = TRUE, mode = TRUE)
summarize_beta(alpha, beta)

print(paste("90% CI: [", round(qbeta(0.05, alpha, beta), 3), ",", round(qbeta(0.95, alpha, beta), 3), "]"))

# b. Sylvia wants to update her prior, so she randomly selects 200 US adults and 80 of them are aware that they know someone who is transgender. 
# Specify and plot the posterior model for π

# likehood 
n <- 200
y <- 80

plot_binomial_likelihood(y, n, mle = FALSE)

# c. What is the mean, mode, and standard deviation of the posterior model?
  # Describe how the prior and posterior Beta models compare.
# aposteriorni model
plot_beta_binomial(alpha, beta, y, n, prior = TRUE, likelihood = TRUE, posterior = TRUE)
summarize_beta_binomial(alpha, beta, y, n)

# len na okraj: ked by sme nemali ziadnu prior informaciu --> beta-rozdelenie(1,1)
plot_beta_binomial(1, 1, y, n, prior = TRUE, likelihood = TRUE, posterior = TRUE)
summarize_beta_binomial(1, 1, y, n)
