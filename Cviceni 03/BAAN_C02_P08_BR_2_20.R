# https://www.bayesrulesbook.com/chapter-2#exercises-1
# Exercise 2.20 (Cat image recognition) Whether you like it or not, cats have taken over the internet.
# Joining the craze, Zainab has written an algorithm to detect cat images. It correctly identifies 80% of cat images as cats, 
# but falsely identifies 50% of non-cat images as cats. Zainab tests her algorithm with a new set of images, 8% of which are cats. 
# What’s the probability that an image is actually a cat if the algorithm identifies it as a cat? 
#   Answer this question by simulating data for 10,000 images.


# Clear variables and console
rm(list = ls())
cat("\014")

# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Define possible articles
cat_img <- data.frame(type = c("no cat", "cat"))

# Define the prior model
prior <- c(0.92, 0.08) 
    # prior - 8% su macky, 92% nie su macky, to je info o populaci

# Simulate 10000 images. 
set.seed(123456)
cat_img_sim <- sample_n(cat_img, size = 10000, 
                        weight = prior, replace = TRUE)

ggplot(cat_img_sim, aes(x = type)) + 
  geom_bar()

cat_img_sim %>% 
  tabyl(type) %>% 
  adorn_totals("row")

cat_img_sim <- cat_img_sim %>% 
  mutate(data_model = case_when(type == "cat" ~ 0.8,
                                type == "no cat" ~ 0.5))

glimpse(cat_img_sim)


# Define whether the algorithm identifies a cat
data <- c("no", "yes")

# Simulate correctness of algorithm 
set.seed(456)
cat_img_sim <- cat_img_sim %>%
  group_by(1:n()) %>% 
  mutate(correct = sample(data, size = 1, 
                          prob = c(1 - data_model, data_model)))

cat_img_sim %>% 
  tabyl(correct, type) %>% 
  adorn_totals(c("col","row"))


ggplot(cat_img_sim, aes(x = type, fill = correct)) + 
  geom_bar(position = "fill")
ggplot(cat_img_sim, aes(x = type)) + 
  geom_bar()

# posterior approximation
cat_img_sim %>% 
  filter(correct == "yes") %>% 
  tabyl(type) %>% 
  adorn_totals("row")

ggplot(cat_img_sim, aes(x = type)) + 
  geom_bar() + 
  facet_wrap(~ correct)