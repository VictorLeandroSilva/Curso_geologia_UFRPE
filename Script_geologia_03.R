# 
dir_01 = "C:/Users/victo/OneDrive/Livros legais/alvaro"
setwd(dir_01)
dir()
getwd()

# Carregar pacotes necessários
library(tidyverse)
library(ggplot2)
library(palmerpenguins)
library(vegan)
library(pvclust)
library(ecodados)

# Definindo parâmetros
set.seed(123)  # Para reprodutibilidade

#dados
df <- palmerpenguins::penguins
glimpse(df)

# ---- manipulando diretorio
getwd() # saber qual deiteorio eu estou
dir.create("Paste_teste") # cria uma pasta no diretorio
setwd("Paste_teste") # entra na pasta do diretorio
getwd()
setwd("..") # volta uma pasta
setwd(dir_01)
# ---------------------

head(df)
df2 <- drop_na(df)
df3 <- df2[,2:6] # selecionar colunas em sequencia
df4 <- df2[,c(1,5,7)] # selecionar conlunas fora de sequencia

# nome nas colunas
df3 <- df3[,-1] #retirar primeira coluna
row.names(df3) <- paste0("site",sample(1:333)) # colonar nomes nas linhas

df5 <- df3[1:25,] # pegar as primeiras 25 linhas da tabela
row.names(df5) <- paste0("site",sample(1:25))

# matriz de distencia
dist_m <- vegan::vegdist(df5, method = "euclidean")
#?vegdist
dist_m

## Agrupamento com a função hclust e o método UPGMA
dendro <- hclust(d = dist_m, method = "average")

## Visualizar os resultados
plot(dendro, main = "Dendrograma", 
     ylab = "Similaridade (índice de Horn)",
     xlab="", sub="")

# PCA
## Manter somentes dados contínuos que pretende aplicar a PCA
penguins <- drop_na(penguins)
penguins_trait <- penguins[,3:6]

## Agora, veja o mesmo cálculo se fizer a padronização (scale.unit da função PCA)
penguins_pad <- decostand(x = penguins_trait, 
                          method = "standardize")

library(FactoMineR)
## PCA
pca.p <- PCA(X = penguins_trait, scale.unit = TRUE, 
             graph = FALSE)
pca.p

## Visualização da porcentagem de explicação de cada eixo
# nota: é necessário ficar atento ao valor máximo do eixo 1 da análise para determinar o valor do ylim (neste caso, colocamos que o eixo varia de 0 a 70).
fviz_screeplot(pca.p, addlabels = TRUE, ylim = c(0, 70), main = "", 
               xlab = "Dimensões",
               ylab = "Porcentagem de variância explicada") 

## Outros valores importantes
var_env <- get_pca_var(pca.p)
## Escores (posição) das variáveis em cada eixo
var_env$coord 
## Contribuição (%) das variáveis para cada eixo
var_env$contrib
## Loadings - correlação das variáveis com os eixos
var_env$cor 
## Qualidade da representação da variável. Esse valor é obtido multiplicado var_env$coord por var_env$coord
var_env$cos2
## Escores (posição) das localidades ("site scores") em cada eixo 
ind_env <- get_pca_ind(pca.p)

## Variáveis mais importantes para o Eixo 1
dimdesc(pca.p)$Dim.1 
## Variáveis mais importantes para o Eixo 2
dimdesc(pca.p)$Dim.2 

fviz_pca_biplot(X = pca.p, 
                geom.ind = "point", 
                fill.ind = penguins$species, 
                col.ind = "black",
                alpha.ind = 0.7,
                pointshape = 21, 
                pointsize = 4,
                palette = c("darkorange", "darkorchid", "cyan4"),
                col.var = "black",
                invisible = "quali",
                title = NULL) +
  labs(x = "PC1 (68.63%)", y = "PC2 (19.45%)") + 
  xlim(c(-4, 5)) +
  ylim(c(-3, 3)) +
  tema_livro()

# Diferentemente da PCA, a Análises de Coordenadas Principais 
# (Principal Coordinate Analysis - PCoA) é uma análise de ordenação 
# irrestrita que aceita dados de diferentes tipos, como contínuos, 
# categóricos, ordinais, binários, entre outros. Assim, a PCoA é 
# aplicada para casos em que a distância euclidiana não é aplicada 
# (como na PCA)

#pacote adicional
library(ape)

# pegar os dados dentro do pacote vegan
data("mite")
data("mite.env")

## Padronização dos dados com Hellinger
mite.hel <- decostand(x = mite, method = "hellinger") 

## Cálculo da matriz de distância com método Bray-Curtis
sps.dis <- vegdist(x = mite.hel, method = "bray") 

## PCoA
pcoa.sps <- pcoa(D = sps.dis, correction = "cailliez")

## Porcentagem de explicação do Eixo 1
100 * (pcoa.sps$values[, 1]/pcoa.sps$trace)[1]

## Porcentagem de explicação dos Eixo 2
100 * (pcoa.sps$values[, 1]/pcoa.sps$trace)[2]

## Porcentagem de explicação acumulada dos dois primeiros eixos 
sum(100 * (pcoa.sps$values[, 1]/pcoa.sps$trace)[1:2])

## Selecionar os dois primeiros eixos
eixos <- pcoa.sps$vectors[, 1:2]

## Juntar com algum dado categórico de interesse para fazer a figura
pcoa.dat <- data.frame(topografia = mite.env$Topo, eixos)

### Gráfico biplot da PCoA
ggplot(pcoa.dat, aes(x = Axis.1, y = Axis.2, fill = topografia, 
                     color = topografia, shape = topografia)) +
  geom_point(size = 4, alpha = 0.7) + 
  scale_shape_manual(values = c(21, 22)) + 
  scale_color_manual(values = c("black", "black")) + 
  scale_fill_manual(values = c("darkorange", "cyan4")) + 
  labs(x = "PCO 1 (49.11%)", y = "PCO 2 (14.30%)") + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) +
  tema_livro()




