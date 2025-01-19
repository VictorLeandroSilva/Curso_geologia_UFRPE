# 
dir_01 = "C:/Users/victo/OneDrive/Livros legais/Adobe"
setwd(dir_01)
dir()
getwd()

# Carregar pacotes necessários
library(tidyverse)
library(ggplot2)
library(palmerpenguins)
library(car)
library(sjPlot)
library(gridExtra)
library(lmtest)

# --- tipos de distribuição -----------------------

# Definindo parâmetros
set.seed(123)  # Para reprodutibilidade

# 1. Distribuição Normal
n_normal <- 1000
media <- 0
desvio_padrao <- 1
dados_normal <- rnorm(n_normal, mean = media, sd = desvio_padrao) #função de numero normais

# 2. Distribuição Poisson
n_poisson <- 1000
lambda <- 5
dados_poisson <- rpois(n_poisson, lambda)

# 3. Distribuição Binomial
n_binomial <- 1000
tamanho <- 10
probabilidade <- 0.5
dados_binomial <- rbinom(n_binomial, size = tamanho, prob = probabilidade)

# 4. Distribuição Exponencial
n_exponencial <- 1000
taxa <- 1
dados_exponencial <- rexp(n_exponencial, rate = taxa)

# Criando um data frame para facilitar a plotagem
dados <- data.frame(
  valor = c(dados_normal, dados_poisson, dados_binomial, dados_exponencial),
  tipo = factor(rep(c("Normal", "Poisson", "Binomial", "Exponencial"), 
                    times = c(n_normal, n_poisson, n_binomial, n_exponencial)))
)

# Plotando os dados
ggplot(dados, aes(x = valor, fill = tipo)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ tipo, scales = "free") +
  labs(title = "Distribuições Aleatórias",
       x = "Valor",
       y = "Frequência") +
  theme_minimal()
#--------------------------------------------------
# regraassão linear simples
#data
df <- palmerpenguins::penguins
df2 <- tidyr::drop_na(df)

## Cabeçalho dos dados
head(df2)
## estrutra completa
glimpse(df2)
# transformar em fator
df2$year <- as.factor(df2$year)
df2$bill_length_mm <- as.numeric(df2$bill_length_mm)

## regressão simples
modelo_regressao <- lm(body_mass_g ~ bill_length_mm, 
                       data = df2)

## Verificar as premissas do teste
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_regressao)
dev.off() # volta a configuração dos gráficos para o formato padrão 
modelo_regressao
modelo_regressao$coefficients 
modelo_regressao$residuals 
modelo_regressao$fitted.values 

## Resultados usando a função anova
anova(modelo_regressao)
## Resultados usando a função summary
summary(modelo_regressao)

## Gráfico
ggplot(data = df2, aes(x = bill_length_mm, y = body_mass_g)) + 
  labs(x = "bill_length_mm", 
       y = "body_mass_g") +
  geom_point(size = 4, shape = 21, fill = "darkorange", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_minimal() +
  theme(legend.position = "none")

#regressao linear multipla 

glimpse(df2)

## Regressão múltipla
modelo_regressao_mul <- lm(body_mass_g ~ bill_length_mm + 
                             flipper_length_mm, 
                           data = df2)

# Multicolinearidade
car::vif(modelo_regressao_mul)

## Normalidade e homogeneidade das variâncias
plot_grid(plot_model(modelo_regressao_mul , type = "diag"))

## regressão múltipla
summary(modelo_regressao_mul)

## Likelihood-ratio test (LRT)
lrtest(modelo_regressao_mul, modelo_regressao)
