# Diretorio
setwd(choose.dir())
dir() # arquivoss no diretorio
getwd() #exirbir o caminho do diretorio
setwd("C:/Users/victo/OneDrive/Livros legais/alvaro")
dir()

# instlando pacotes
install.packages("ggplot2")
# chamar o pacote
library(ggplot2)
library(tidyverse)

# objetos
a = 2
A = 5
b = a + A 
b

# acessando help
?ggplot # help da funçao
??ggplot2 # help do pacote

# citaçao do pacote
citation("ggplot2")

# ----------------------------------------------
# tipos de dados
# Caracter
car <- 'a'
car
string <- 'casa'
string
# numerico
inteiro <- 0
inteiro
float <- 0.5955
float

#logico
verdadeiro <- TRUE
falso <- FALSE

# Estrutras de dados
#list muito semelante vetor
lista <- list()
lista <- c(1,2,3,4,5) # concatenar
lista
lista_car <- c('a','d','f')
lista_car
vetor <- vector()
matriz <- matrix(nrow = 4, ncol = 4)
?matrix
matriz[2,3] <- 1.5 # adicionar ou modificar valor
View(matriz)
dataFrame <- data.frame()
?data.frame
dataFrame[1,1] <- 'casa'
dataFrame[1,2] <- 'Endereço'
dataFrame[2,1] <- 'Amarela'
dataFrame[2,2] <- 'Olinda'
colnames(dataFrame) <- c("Coluna1","coluna2")

# ---------------------------------------------------
df <- 50
gf = 100

tf <- df - gf
tf
#
sum(lista) # funçao soma
# ------------------------------------------------------
dir()
dados <- read.csv("Bothrops_Erythromelas_clean_lim_oppc.csv", sep = c(";",",","."," "))
dados2 <- read_csv("Bothrops_Erythromelas_clean_lim_oppc.csv")
dados3 <- read.table("occurrence.txt")
dados4 <- read_table("occurrence.txt")





