# Construção de intervalos de confiança a partir de dados de diferentes amostras

# Carrega pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Gera dados aleatórios com distribuição normal e cria um matriz
m <- 100; n <- 20; mu <- 140; sigma <- 4

set.seed(25)
x <- rnorm(n = m*n, mean = mu, sd = sigma) # (2000 dados)
mat <- matrix(data = x, ncol = m,
              # dimnames = list(NULL,
              #                 stringr::str_c("A", 1:100)
              #                 )
              )

# Converte dados para o formato longo

data <- mat %>%
  as_tibble() %>%
  pivot_longer(everything(),
               names_to = "Amostra",
               values_to = "v") %>%
  mutate(Amostra = stringr::str_remove(Amostra, "[V]")) %>%
  mutate(across(where(is.character), as.numeric))

# Calcula média e desvio padrão p/ cada amostra

df <- data %>%
  group_by(Amostra) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = list(media = mean, dp = sd),
      na.rm = TRUE,
      .names = "{.fn}"
      )
    )

# Calcula Intervalos de Confiança (ICs) p/ cada amostra

t_crit <- qt(p = 1-(1-.95)/2, df = n-1) # (alfa = 0,05; df = 19)

df <- df %>%
  mutate(lcl = media - t_crit*dp/sqrt(n),
         ucl = media + t_crit*dp/sqrt(n),
         cover = case_when(lcl < mu & ucl > mu ~ paste("Contém μ"),
                           TRUE  ~ paste("Não contém μ"))
         )

# Visualiza os ICs para as 100 amostras

df %>%
  ggplot() +
  geom_errorbar(aes(x = Amostra,
                    ymin = lcl,
                    ymax = ucl,
                    color = ifelse(cover == "Contém μ", 'black', 'red')
                    ),
                alpha = 0.9,
                size = .4,
                width = .9
                ) +
  scale_color_identity(guide = "legend",
                       labels = c("Contém μ", "Não contém μ")) +
  geom_point(aes(x = Amostra, y = media),
             size = 1,
             color = "blue",
             fill = alpha("orange", 0.1),
             alpha = 0.5,
             shape = 21,
             stroke = .7) +
  geom_hline(yintercept = mu, color = "green2") +
  theme_base() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 14),
        legend.position = c(.24, .95),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.margin = margin(c(1, 5, 5, 5)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Intervalos de Confiança (95%)",
       subtitle = "(100 amostras aleatórias de mesmo tamanho)",
       x = "Número da amostra",
       y = expression(paste(mu, ' = ?'))) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10),
                     limits = c(0, 102))

ggsave("IC-Plot.png", path = "Slides/fig/class3",
       dpi = 600, width = 16, height = 12,
       units = "cm")

# Visualiza os pontos de dados amostrais usado para calcular os ICs

data %>%
  ggplot(aes(x=Amostra, y = v, group = Amostra)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = mu) +
  theme_base()

