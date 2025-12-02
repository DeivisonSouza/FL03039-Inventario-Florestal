## -----------------------------------------------------------
## Simulação de dados de IF por Amostragem de Pinus taeda em
## parcelas de 600 m² com espaçamento 3 x 2 m (100 árvores/parcela).
## A ideia é simular dados de diâmetro e alturas de árvores, visando
## encontrar os volumes por parcelas do estudo de caso em Amostragem
## Aleatória Simples (AAS) (Sanquetta et al., 2023, pg. 124). Para
## calcular os volumes individuais foi admitido o fator de forma 0,45.
##
## SANQUETTA, C.R.; CORTE, A.P.D.; RODRIGUES, A. L.; WATZLAWICK, L.F.
## Inventários florestais: planejamento e execução. 4ª ed. Curitiba, PR.
## 2023. 406p.
## -----------------------------------------------------------

## --------------------------------
## Instalar Pacotes
## --------------------------------
# install.packages("writexl")

## --------------------------------
## Carregar Pacotes
## --------------------------------
library(writexl)

## -----------------------------
## 1. Parâmetros básicos
## -----------------------------
set.seed(123)

area_parcela <- 600        # m²
esp_x <- 3                 # m
esp_y <- 2                 # m
area_por_arvore <- esp_x * esp_y   # 6 m²/árvore
n_arvores_parcela <- round(area_parcela / area_por_arvore)  # ≈ 100 árvores

fator_forma <- 0.45        # Pinus taeda

vol_alvo <- c(
  20.85, 19.47, 24.13, 24.34,
  25.13, 22.37, 22.51, 19.78,
  25.05, 28.84, 23.70, 24.78,
  22.58, 23.70, 36.16, 17.83
)                         # Sanquetta et al., 2023, pg. 124

## -----------------------------
## 2. Função para simular 1 parcela
## -----------------------------
simula_parcela <- function(id_parcela, V_target,
                           n_arv = n_arvores_parcela,
                           ff = fator_forma) {

  # DAP (cm)
  dap_cm <- rnorm(n_arv, mean = 20, sd = 4)
  dap_cm <- pmax(dap_cm, 8)
  dap_cm <- pmin(dap_cm, 40)

  # Altura (m)
  h_m <- 1.3 + 3 * log(dap_cm)

  # Volume inicial (m³)
  d_m <- dap_cm / 100
  vol_i <- ff * (pi * (d_m^2) / 4) * h_m

  # Ajuste para volume alvo
  V_sim <- sum(vol_i)
  k <- V_target / V_sim
  h_m_aj <- h_m * k

  # Volume final ajustado
  vol_i_aj <- ff * (pi * (d_m^2) / 4) * h_m_aj

  dados_parcela <- data.frame(
    parcela = id_parcela,
    arvore  = 1:n_arv,
    dap_cm  = dap_cm,
    h_m     = h_m_aj,
    vol_m3  = vol_i_aj
  )

  return(dados_parcela)
}

## -----------------------------
## 3. Gerar dados para as 16 parcelas
## -----------------------------
lista_parcelas <- lapply(
  X = 1:16,
  FUN = function(i) {
    simula_parcela(
      id_parcela = i,
      V_target   = vol_alvo[i]
    )
  }
)

dados_inventario <- do.call(rbind, lista_parcelas)

## -----------------------------
## 4. Arredondamentos:
##    - DAP e altura → 2 casas
##    - Volume individual → 4 casas
## -----------------------------
dados_inventario_fmt <- within(dados_inventario, {
  dap_cm <- round(dap_cm, 2)
  h_m    <- round(h_m, 2)
  vol_m3 <- round(vol_m3, 4)
})

## -----------------------------
## 5. Conferência dos volumes por parcela (2 casas)
## -----------------------------
vol_parcelas <- aggregate(vol_m3 ~ parcela, data = dados_inventario, sum)
vol_parcelas$vol_parcela_2dec <- round(vol_parcelas$vol_m3, 2)
vol_parcelas

## -----------------------------
## 6. Salvar em .xlsx
## -----------------------------
write_xlsx(dados_inventario_fmt,
           path = "Slides/data/Pinus_simulado.xlsx")
