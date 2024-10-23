########################################
# Exemplo de aula - Base de dados Câncer
########################################

########################
# Bibliotecas
########################
library(dplyr)
library(readr)

########################
# Carregamento dos dados
########################
Dados_cancer <- read_csv("C:/Users/andre/Downloads/Exemplo 1 - Dados_cancer.csv")

View(Exemplo_1_Dados_cancer)

#######################
# Explorando e preparando os dados
#######################
# Retirando a coluna "id" e "..33"
Dados_cancer <- Dados_cancer[c(-1,-33)]

# Tabela de frequência para os rótulos
table(Dados_cancer$diagnosis)

#codificando a variável como fator
Dados_cancer$diagnosis <- factor(Dados_cancer$diagnosis) 

# Criando uma função que normaliza
normaliza <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

# Normalizando os dados numéricos
Dados_cancer_norm <- as.data.frame(lapply(Dados_cancer[2:31],normaliza))

# Criando datasets de treino e testes (80-20)
Dados_cancer_treino <-Dados_cancer_norm[1:469,]
Dados_cancer_teste <- Dados_cancer_norm[470:568,]

# Separando rótulos
Dados_cancer_treino_rótulos <-Dados_cancer[1:469,1]$diagnosis
Dados_cancer_teste_rótulos <- Dados_cancer[470:568,1]$diagnosis

################################
# Aplicação do k-NN
################################
install.packages("class")
install.packages("caret")
library(class)
library(caret)

predicoes <- knn(train= Dados_cancer_treino,
                 test = Dados_cancer_teste,
                 cl = Dados_cancer_treino_rótulos,
                 k = 3)

confusionMatrix(Dados_cancer_teste_rótulos, predicoes)

###################
#Pós intervalo
##################

# Há um erro na plotagem do K

acuracia <- c()

for(k in 1:30){

predicoes <- knn(train = Dados_cancer_treino,
                 test = Dados_cancer_teste,
                 cl = Dados_cancer_teste_rótulos,
                 k = k)

matriz <- confusionMatrix(Dados_cancer_teste_rótulos, predicoes)

acuracia <- c(acuracia, matriz[['overall']]['Accuracy'])

}

plot(1:30, 1 - acuracia, type = 'l')

# Melhor Knn

predicoes <- knn(train= Dados_cancer_treino,
                 test = Dados_cancer_teste,
                 cl = Dados_cancer_treino_rótulos,
                 k = 6)

confusionMatrix(Dados_cancer_teste_rótulos, predicoes)



