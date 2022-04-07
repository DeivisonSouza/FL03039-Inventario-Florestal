###########################################################
# Combinação simples no R
###########################################################

x <- letters[1:10]
y <- 1:100

choose(10, 2)
z <- t(combn(1:10, 2))

###########################################################
# Simulando dados normais p/ DAP e H
###########################################################

# Simulação de dados p/ Mogno Africano (Khaya Ivorensis) sob espaçamento
# 5m x 5m (400 mudas/hectare). Admitir-se-á um volume aproximado de 153m3/ha
# na idade de 15 anos.
# link: https://mudasnobres.com.br/cultivo-de-mogno-africano-para-2019-pode-ser-planejado-agora/

# (((pi*(26)^2)/40000)*10*0.7)

# Carrega pacotes...
library(purrr)
library(tidyr)
library(dplyr)

# Parâmetros da simulação...
n <- 10
d_mean <- 26
d_sd <- 3
h_mean <- 10
h_sd <- 1
arv_parc <- 1:400

# Simula diâmetros
set.seed(100)
simul_d <- map_dfr(arv_parc, ~ setNames(rnorm(n, mean = d_mean, sd = d_sd),
                             stringr::str_c("P", 1:10))) %>%
  pivot_longer(everything(), names_to = "Parc", values_to = "d") %>%
  mutate(across(where(is_character), forcats::as_factor)) %>%
  arrange(Parc, desc(d))

# Simula alturas
set.seed(100)
simul_h <- map_dfr(arv_parc, ~ setNames(rnorm(n, mean = h_mean, sd = h_sd),
                                   stringr::str_c("P", 1:10))) %>%
  pivot_longer(everything(), names_to = "Parc", values_to = "h") %>%
  mutate(across(where(is_character), forcats::as_factor)) %>%
  arrange(Parc, desc(h))

# Combina dados...
data <- bind_cols(simul_d, simul_h[c("h")])

# Calcula volume...
data <- data %>%
  mutate(v = ((pi*(d^2))/40000)*h*0.7)

# Média, desvio e total por Parcela
data %>%
  group_by(Parc) %>%
  summarise(across(.cols = where(is.numeric),
                   .fns = list(media=mean, desv=sd, soma=sum),
                   na.rm = TRUE,
                   .names = "{.col}.{.fn}"
  )
  )

# Simulando dados normais com variação da média e desvio padrão p/ cada parcela...
###################################################

df <- data.frame(
  mean_d = runif(n = 10, min = 25, max = 27),
  sd_d = runif(n = 10, min = 1, max = 2),
  mean_h = runif(n = 10, min = 9, max = 10),
  sd_h = runif(n = 10, min = .5, max = 1)
)

# Simula diâmetros
set.seed(100)
simul_d1 <- map_dfr(arv_parc, ~ setNames(rnorm(n, mean = df$mean_d, sd = df$sd_d),
                                        stringr::str_c("P", 1:10))) %>%
  pivot_longer(everything(), names_to = "Parc", values_to = "d") %>%
  mutate(across(where(is_character), forcats::as_factor)) %>%
  arrange(Parc, desc(d))

# Simula alturas
set.seed(100)
simul_h1 <- map_dfr(arv_parc, ~ setNames(rnorm(n, mean = df$mean_h, sd = df$sd_h),
                                        stringr::str_c("P", 1:10))) %>%
  pivot_longer(everything(), names_to = "Parc", values_to = "h") %>%
  mutate(across(where(is_character), forcats::as_factor)) %>%
  arrange(Parc, desc(h))

# Combina dados...
data <- bind_cols(simul_d1, simul_h1[c("h")])

# Calcula volume...
data <- data %>%
  mutate(v = ((pi*(d^2))/40000)*h*0.7)

# Média, desvio e total por Parcela
data %>%
  group_by(Parc) %>%
  summarise(across(.cols = where(is.numeric),
                   .fns = list(media=mean, desv=sd, soma=sum),
                   na.rm = TRUE,
                   .names = "{.col}.{.fn}"
  )
  )
