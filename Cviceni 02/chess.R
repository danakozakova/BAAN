# 2.3 Building a Bayesian model for random variables

# a discrete set of possibilities
pi <- c(0.2, 0.5, 0.8)

# the probability mass function (pmf)  f(⋅)which specifies the prior probability of each possible π value.
prior <- c(0.10, 0.25, 0.65) 

# 2.3.2 The Binomial data model
# Y = the number of the six games in the 1997 re-match that Kasparov wins
# Y is a random variable that can take any value in {0, 1,..., 6}
# Conditional probability model of data Y   f(y/pi) = P(Y = y / pi)
# if the parameter pi has this value, then the probability of winning x matches is ...
# the probability of win doeent change and matches are independent ==> this is Binomial model
