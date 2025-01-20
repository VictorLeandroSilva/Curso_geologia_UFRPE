# Script para fazer análises utilizando o glm. Os dados e o código foram feitos a partir #
# do livro Análises ecológicas no R -> https://analises-ecologicas.com/cap8 #

# Selecionar a pasta de trabalho do seu computador ####
setwd("C:/Users/Boldo/OneDrive/Documentos/Gabriel/minicurso_r_geologia")

# Pacotes ####
#install.packages("tidyverse") # apagar a # se já não tiver instalado esse pacote
#install.packages(car)
install.packages("devtools") # permite baixar pacotes fora do cran
devtools::install_github("paternogbc/ecodados") # instalar pacote fora do cran
install.packages("DHARMa") # pacote de diagnose dos modelos
install.packages("performance") # pacote para calcular o r2
install.packages("MASS") # pacote do glm com distribuicao binomial negativa
install.packages("glmmTMB") # pacote do glm com distribuicao beta
#install.packages("patchwork") # pacote para "unir" figuras
install.packages("emmeans") # pacote para analisar o efeito de interacao entre as variaveis
library(car)
#library(patchwork)
library(tidyverse) 
library(svglite)
library(ecodados)
library(DHARMa)
library(performance)
library(MASS)
library(glmmTMB)
library(emmeans)

# "Pegar" os dados do pacote ecodados (dados para exemplificar as análises) ####
frag_data <- ecodados::fragmentos
fish_data <- ecodados::fish

# "Olhar" os dados (verificando a classe das variáveis/colunas) ####
glimpse(frag_data)
glimpse(fish_data)

# Modelo linear (recaptulação aula passada) ####
## Pigmento vermelho influencia no pigmento preto (exemplo sem muita lógica) ####
mod_pig <- lm(Darkness ~ Redness, data = fish_data)
residos_mod <- residuals(mod_pig) # pegar os residuos do modelo
shapiro.test(residos_mod) # testar a normalidade com shapiro (outra maneira que n visual)
summary(mod_pig) # resultado do modelo (como valor de p, slope/estimate, r2)

# Modelos lineares generalizados (quando os residuos n tem distribuição normal) ####
## Para dados de contagem (poisson) ####
mod_poisson <- glm(Riqueza_obs ~ dfrag, 
              family = poisson(link = "log"),
             data = frag_data)

## Diagnose dos pressupostos do modelo ####
diagnose_model <- simulateResiduals(fittedModel = 
                                      mod_poisson, 
                                    plot = TRUE) # modelo apresenta problema de superdispersão

## Interpretacao dos resultados (não podemos confiar, por conta da superdispersão) ####
summary(mod_poisson)
r2(mod_poisson)

## Para dados de contagem (binomial negativa, pode resolver o problema de superdispersão) ####
mod_binomial_neg <- glm.nb(Riqueza_obs ~ dfrag, data = frag_data)

diagnose_model2 <- simulateResiduals(fittedModel = mod_binomial_neg, plot = TRUE)

## Interpretacao dos resultados (podemos confiar, por n apresentar problemas)
# de outlier, normalidade, superdispersão
summary(mod_binomial_neg)
r2(mod_binomial_neg)

## Para dados continuos de proporção (distribuição beta) ####
# Obs: Necessário divir a variável resposta por 100, para variar entre 0 e 1 (pré requisito)
mod_beta <- glmmTMB(Darkness/100 ~ Treatment * 
                  Time + (1|Animal), # sinal * indica interação entre as variáveis preditoras
                family = beta_family, # (1|Animal) forma de incluir um fator/variavel aleatório
                data = fish_data)

## Diagnose do modelo ####
diagnose_mod_beta <- simulateResiduals(fittedModel = 
                                     mod_beta, 
                                   plot = TRUE)

## Interpretação dos resultados ####
Anova(mod_beta) # só indica que existe efeito individual e de interação das variaveis preditoras
pairs(emmeans(mod_beta, ~ Treatment|Time)) # indica "aonde" o efeito acontece
