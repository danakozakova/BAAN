# Priklad 3
# https://is.muni.cz/auth/el/econ/podzim2025/MPE_BAAN/um/podklady_ke_cvicenim/cviceni_06/BAAN_2025_Cviceni_06.pdf?predmet=1695283 
# Clear variables and console
 rm(list = ls())
 cat("\014")
 
 # Load packages you'll need for this chapter
 library(tidyverse)

# Nastaveni simulace (RWMH)
 # pocet generovanych vzorkov 
 S <- 10000 + 1 # (velke N) ta 1 je pociatocne pozorovanie, nemusi byt
 # pocet burn-in vzorkov
 s0 <- 5000 + 1
 # ponechame pre dalsiu analyzu
 s1 <- S - s0

 # theta - pre ukladanie vzoriek
 theta <- rep(0, S)

 # pociatocne nastavenie behu
 theta[1] <- 10
 theta |> head(5) 

 # Kandidatska (proposal) hustota
 # N(theta(s-1), c^2)
 c <- 5
 # pocitadlo akceptovanych vzoriek
 count <- 0 

 # Random Walk chain M-H algoritmus ====================================== 
 for (s in 2:S) {
  # a) generujeme kandidata
   theta_ast <- rnorm(1, mean = theta[s-1], sd = c)
  # b) pocitanie akceptacnej pravdepodobnosti
   alpha <- min(1, 
                exp(-1/2*abs(theta_ast))/exp(-1/2*abs(theta[s-1]))
                )
  # c) akceptacia alebo zamietnutie kandidata
   if (alpha > runif(1)){
     theta[s] <- theta_ast
     count <- count + 1
   } else {
     theta[s] <- theta[s-1]
   }
 }
 
 # Prezentace vysledku ==================================================
 # vyhodenie prvych s0 + 1 vyberu (samples)
 theta <- theta[(s0 + 1) : s]
 # graficke zobrazenie konvergencie (trace plot)
 plot(theta, type = "l")
 
 # ked napriklad len kazdy 100.
 plot(theta[seq(from = 1, to = s1, by = 100)], type = "l")

# pocitanie momentov
E_theta <- mean(theta)
D_theta <- mean(theta^2) - E_theta^2

# priemerna miera akceptacie
avg_count <- count/(S-1)

# histogram
hist(theta, 50, xlim = c(-20, 20)) # reprezentacia aposteriorneho Laplaceovho rozdelenia

#################################
# zmenit c na 0.05
# Kandidatska (proposal) hustota
# N(theta(s-1), c^2)
c <- 0.05
# pocitadlo akceptovanych vzoriek
count <- 0 

# Random Walk chain M-H algoritmus ====================================== 
for (s in 2:S) {
  # a) generujeme kandidata
  theta_ast <- rnorm(1, mean = theta[s-1], sd = c)
  # b) pocitanie akceptacnej pravdepodobnosti
  alpha <- min(1, 
               exp(-1/2*abs(theta_ast))/exp(-1/2*abs(theta[s-1]))
  )
  # c) akceptacia alebo zamietnutie kandidata
  if (alpha > runif(1)){
    theta[s] <- theta_ast
    count <- count + 1
  } else {
    theta[s] <- theta[s-1]
  }
}

# Prezentace vysledku ==================================================
# vyhodenie prvych s0 + 1 vyberu (samples)
theta <- theta[(s0 + 1) : s]
# graficke zobrazenie konvergencie (trace plot)
plot(theta, type = "l")

# ked napriklad len kazdy 100.
plot(theta[seq(from = 1, to = s1, by = 100)], type = "l")

# pocitanie momentov
E_theta <- mean(theta)
D_theta <- mean(theta^2) - E_theta^2

# priemerna miera akceptacie
avg_count <- count/(S-1)

# histogram
hist(theta, 50, xlim = c(-20, 20)) # reprezentacia aposteriorneho Laplaceovho rozdelenia

###########
# treba skusit aj c = 100, potom prestreluje rozptyl
