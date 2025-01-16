# Selecionar a pasta de trabalho do seu computador ####
setwd("C:/Users/Boldo/OneDrive/Documentos/Gabriel/minicurso_r_geologia")

# Pacotes ####
install.packages("tidyverse") # funcão p/ instalar o pacote
install.packages("palmerpenguins")
install.packages("svglite")
library(tidyverse) # funcão p/ ativar o pacote tidyverse
library(palmerpenguins)
library(svglite)

# Dados ####
peng <- palmerpenguins::penguins # pegar o dado do pacote palmerpenguins

# Exemplo de como criar um dataframe no R ####
exem <- data.frame(x = peng$bill_depth_mm, # cifrão seleciona uma coluna específica
                   y = peng$flipper_length_mm) 

# Manipulação de planilhas/dataframe ####
peng2 <- peng %>% # simbolo chamado de pipe, "linka" o que está na esquerda como primeiro argumento da função a direita 
  dplyr::mutate(nova_coluna = NA) %>% # função para criar uma nova coluna
  dplyr::filter(species %in% c("Adelie", "Gentoo")) # função para filtrar uma coluna por um valor específico

peng2.1 <- peng %>% 
  dplyr::mutate(nova_coluna = NA) %>% 
  dplyr::filter(species == "Adelie")

peng2.2 <- peng %>% 
  dplyr::mutate(nova_coluna = NA) %>% 
  dplyr::filter(body_mass_g > 4000)

# Agrupar dados por algum fator e "sumarizar" alguma informação (ou valor) ####
peng3 <- peng %>% 
  dplyr::group_by(species) %>% # função para agrupar por fator 
  dplyr::summarise(media_dos_penguins = 
                     mean(body_mass_g, na.rm = T),
                   desvio_penguin = sd(body_mass_g, 
                                       na.rm = T)) # função para sumarizar

# Figura - Histograma ####
dist_norm <- data.frame(xuxa = rnorm(10000)) # dado com uma distrib. normal

grafico_hist <- dist_norm %>%  
  ggplot(aes(x = xuxa)) + # 1 camada (lembra de adicionar o sinal de "+")
  geom_histogram() # 2 camada

# Figura - Gráfico de Barra ####
grafico_barra <- peng3 %>% 
  ggplot(aes(x = species, 
             y = media_dos_penguins)) +
  geom_bar(stat = "identity")

# Figura - Gráfico de boxplot ####
grafico_boxplot <- peng %>% 
  ggplot(aes(x = species, 
             y = flipper_length_mm, 
             fill = species)) +
  scale_fill_manual(values = c("#7fc97f", # função para escolher as cores
                               "#beaed4",
                               "#fdc086")) +
  geom_boxplot() + 
  geom_jitter() +  # função para botar os "pontinhos" no boxplot
  theme_bw() +
  theme(legend.position = "top") # função para escolher a posição da legenda

# Figura - Gráfico de dispersão ####
grafico_dispersao <- peng %>% 
  ggplot(aes(x = bill_length_mm, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point(size = 2, alpha = 0.5) + # "size" tamanho do ponto, "alpha" é a transparência 
  geom_smooth(method = "lm", se = F) + # reta/ajuste do modelo
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Comprimento do bico (mm)") + # mudar nome do eixo x 
  ylab("Profundidade do bico (mm)")  # mudar nome do eixo y

# Jeito para salvar figuras em alta qualidade ####  
ggsave("jeito_certo.svg", plot = grafico_dispersao,
       dpi = 600, height = 10, width = 8) # pode escolher outros formatos (tipo png)
