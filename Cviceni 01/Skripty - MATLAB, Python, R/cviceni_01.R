# https://www.bayesrulesbook.com/chapter-2#fnref13 2.1.5
# FAKE vS REAL - EXCLAMATIONS
# Define possible articles
article <- data.frame(type = c("real", "fake"))

# Define the prior model
prior <- c(0.6, 0.4)

# Simulate 10000 articles. 
# set.seed(84735)
article_sim <- sample_n(article, size = 10000, 
                        weight = prior, replace = TRUE)
# Graf
library(ggplot2)
ggplot(data = article_sim,
       mapping = aes(x = type)) +
  geom_bar()

# Tab
# install.packages("janitor")
library(janitor)
article_sim %>% 
  tabyl(type) %>% 
  adorn_totals("row")

article_sim <- article_sim %>% 
  mutate(data_model = case_when(type == "fake" ~ 0.2667,
                                type == "real" ~ 0.0222))

glimpse(article_sim)

# Define whether there are exclamation points
data <- c("no", "yes")

# Simulate exclamation point usage 
# set.seed(3)
article_sim <- article_sim %>%
  group_by(1:n()) %>% 
  mutate(usage = sample(data, size = 1, 
                        prob = c(1 - data_model, data_model)))

article_sim %>% 
  tabyl(usage, type) %>% 
  adorn_totals(c("col","row"))

ggplot(article_sim, aes(x = type, fill = usage)) + 
  geom_bar(position = "fill")
ggplot(article_sim, aes(x = type)) + 
  geom_bar()

article_sim %>% 
  filter(usage == "yes") %>% 
  tabyl(type) %>% 
  adorn_totals("row")

ggplot(article_sim, aes(x = type)) + 
  geom_bar() + 
  facet_wrap(~ usage)
#################################################### SIMULACE Alice - Bob
rm(list = ls())
cat("\f") # prikaz vycisti command window

# 1. Hra Alice a Boba - deklarace promennych a parametru
S <- 1000000 # pocet opakovani cele hry
r <- 0 # pocitadlo stavu 5:3, t.j. pocet her, kde nastal stav 5:3
soucet_pravd <- 0 # soucet P(p/A = 5, B = 3)
E_Bwin <- 0 # stredni hodnota vyhry Boba pri stavu 5:3

# 2. Simulace hry
for (s in 1:S) {
    p <- runif(1) # pocatecni rozdeleni stolu
         # generujeme p ~ U(0,1) ~ apriorna hustota
    # zahrani 8 her
    y <- runif(8) # vektor rozmeru 8x1 s nezavislymi U(0,1)
    # overime stav 5:3 => Alice musi vyhrat prave 5x
    # y < p ... vrati vektor 0/1, kde 1 = vyhrala Alice
    # sum(y<p) ... spocita pocet vyher Alice -> porovname
    #              s 5 sum ... secte prvky vektoru
    if (sum(y < p) == 5) { # ci Alica vyhrala prave 5-krat
        r <- r + 1 # pocitadlo stavu 5:3 se posune o jedna
        # scitame pravdepodobnosti vyhry Boba
        # (z predchoziho behu pri stavu 5:3)
        soucet_pravd <- soucet_pravd + (1 - p)^3
    }
}


# Vypocet a prezentace pozadovanych statistik
# Stredni (ocekavana) hodnota pravdepodobnosti vyhry Boba
# pri stavu 5:3
E_Bwin <- soucet_pravd/r
# Stredni (ocekavana) hodnota pravdepodobnosti vyhry Alice
# pri stavu 5:3
E_Awin <- 1 - E_Bwin

# Jednoduchy vypis vysledku na obrazovku
print('Prumerna pravdepodobnost vyhry Boba pri stavu 5:3')
print(E_Bwin)
print('Prumerna pravdepodobnost vyhry Alice pri stavu 5:3')
print(E_Awin)
print('Ferovy podil sanci A:B pro rozdeleni vyhry za stavu 5:3')
print(E_Awin/E_Bwin)
print('Pocet stavu 5:3')
print(r)
print('Relativni zastoupeni poctu stavu 5:3 na celkovem poctu simulaci')
print(r/S)

# Graficke zobrazeni vysledku
plot(1, E_Bwin, xlim = range(c(0,3)), ylim = range(c(0,1)), pch = 8, col = "red")
points(2, E_Awin, pch = 3, col = "green")

