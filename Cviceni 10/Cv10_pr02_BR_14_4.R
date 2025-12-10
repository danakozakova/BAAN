# Clear variables and console
 rm(list = ls())
 cat("\014")
 
# Load packages
 library(bayesrules)
 library(rstanarm)
 library(bayesplot)
 library(tidyverse)
 library(tidybayes)
 library(broom.mixed)
 library(janitor)
 library(e1071)
 
 
# 14.4 exclamation points ========
 data(fake_news)
 fake <- fake_news
 
 fake %>% 
   tabyl(type, title_has_excl) %>% 
   adorn_totals(c("row", "col"))
 
 
 ggplot(fake, 
        aes(fill = title_has_excl, x = type)) + 
   geom_bar(position = "fill") +
   theme_classic()
 
 naive_model <- naiveBayes(type ~ title_has_excl, data = fake)
 our_article <- data.frame(title_has_excl = FALSE)
 predict(naive_model, newdata = our_article, type = "raw")
 predict(naive_model, newdata = our_article)
 
# 14.5 title_length ========
 # ked budeme mat spojitu premennu
 ggplot(fake, aes(x = title_words, fill = type)) + 
   geom_density(alpha = 0.7) + 
   geom_vline(xintercept = 15, linetype = "dashed")
 
 # bayesovska aproximacia normalnym rozdelenim
 # Calculate sample mean and sd for each Y group
 fake %>% 
   group_by(type) %>% 
   summarize(mean = mean(title_words, na.rm = TRUE), 
             sd = sd(title_words, na.rm = TRUE))

 ## vystup je 
 # A tibble: 2 × 3
 # type   mean    sd
 # <fct> <dbl> <dbl>
 #1 fake   12.3  3.74
 # 2 real   10.4  3.20
 
 # t.j. nase aproximace, graf jenom pro nas - pro vizualizaci:
 ggplot(fake, aes(x = title_words, color = type)) + 
   stat_function(fun = dnorm, args = list(mean = 12.3, sd = 3.74), 
                 aes(color = "fake")) +
   stat_function(fun = dnorm, args = list(mean = 10.4, sd = 3.20),
                 aes(color = "real")) +
     geom_vline(xintercept = 15, linetype = "dashed")
 

# 14.6 title_length and negative sentiment ========
 naive_model <- naiveBayes(type ~ title_words, data = fake)
 naive_model # objekt modelu
 
 our_article <- data.frame(title_words = 15)
 our_article
 
 predict(naive_model, newdata = our_article, type = "raw")
 predict(naive_model, newdata = our_article)
 
# 14.7 three predictors ========  
 
 naive_model2 <- naiveBayes(type ~ title_words + negative + title_has_excl, data = fake)
 naive_model2
 
 our_article <- data.frame(title_words = 15,
                           negative = 6,
                           title_has_excl = FALSE)
 
 predict(naive_model2, newdata = our_article, type = "raw")
 predict(naive_model2, newdata = our_article)
 
# 14.8 model selection ========  
# mame len dva modely, da sa urobit presne podla zadania - tie 4 modely 
 
 fake <- fake %>% mutate(
   class1 = predict(naive_model, newdata = .), # klasifikace podle 1. modelu
   class2 = predict(naive_model2, newdata = .) # klasifikace podle 2. modelu
 )
 
 # vybrat nahodne pozorovania
 set.seed(84735)
 fake %>%
   sample_n(10) %>%
   select(title_words, negative, title_has_excl, type, class1, class2)
 
 # Confusion matrix for naive_model_1
 fake %>% # v riadku su skutocne
   tabyl(type, class1) %>% 
   adorn_percentages("row") %>% 
   adorn_pct_formatting(digits = 2) %>%
   adorn_ns()
 
 # type        fake        real
 # fake 30.00% (18) 70.00% (42)
 # real 14.44% (13) 85.56% (77)
 # citlivost = 30% 
 #specificita = 85.56%
 
 # Confusion matrix for naive_model_2
 fake %>% # v riadku su skutocne
   tabyl(type, class2) %>% 
   adorn_percentages("row") %>% 
   adorn_pct_formatting(digits = 2) %>%
   adorn_ns()
 
 # type        fake        real
 # fake 48.33% (29) 51.67% (31)
 # real 12.22% (11) 87.78% (79)
 
 # cross-validation pre model1
 set.seed(84735)
 cv_model1 <- naive_classification_summary_cv(
   model = naive_model, data = fake, y = "type", k = 10)

 cv_model1$folds 
 cv_model1$cv

 # cross-validation pre model2
 set.seed(84735)
 cv_model2 <- naive_classification_summary_cv(
   model = naive_model2, data = fake, y = "type", k = 10)
 
 cv_model2$folds 
 cv_model2$cv
 
 # 14.9 Logistic vs. naive ========  
 
 z predch cvicenia ... stan_glm
 pozri aj cross-validaciu