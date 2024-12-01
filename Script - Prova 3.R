#############################################################
# "Prova" 3 - Modelos Econômicos Quantitativos 
# Alunos: André Cunha e Rayane Cardoso - (19/11 até 10/12)
#############################################################

# instalação de Pacotes
install.packages("MASS")
install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")

# Carregamento de pacotes
library(readr)        # Leitura de arquivos
library(ggplot2)      # Gráficos
library(dplyr)        # Manipulação de dados
library(class)        # kNN
library(caret)        # Métricas e validação cruzada
library(MASS)         # Naive Bayes
library(rpart)        # Árvores de decisão
library(rattle)       # Visualização de árvores
library(rpart.plot)   # Plotagem de árvores

# Carregando Data source
dados <- read_csv("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Provas/Prova 3/Dataset-minas terrestres.csv")
View(dados)

###################################################################
# 1 – Comece analisando os tipos de minas nesse conjunto de dados.
###################################################################

# a) Faça uma tabela de frequências e um gráfico de barras para os dados da coluna M.

table(dados$M) # Tabela

# gráfico de barras
ggplot(dados, aes(x = factor(M, labels = c("Não", "Sim")))) +
  geom_bar(fill = "purple", color = "black") +
  labs(
    title = "Detecção de minas terrestres",
    x = "Detecção",
    y = "Quantidade"
  ) +
  theme_classic()

#########################################
#2 – Fixe uma semente aleatória. Depois:
#########################################

set.seed(1234)

# a) Separe os dados em uma amostra de treino e outra de teste, aleatórias (80-20).

sorteio <- sample(1:137,109) # Escolha das observações aleatóriamente

dados_treino <- dados[sorteio,]

dados_teste <- dados[-sorteio,]

# b) Faça uma tabela de frequências para a variável M para a 
  # amostra de treino e outra para a amostra de teste. 

table(dados_treino$M)

table(dados_teste$M)

########################
# 3 – Para o kNN:
########################

# Normalizando os dados - 
  # É importante pois o algoritmo calcula a distância entre os
  # pontos para determinar quais são os vizinhos mais próximos.

normaliza <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

# Excluindo a variável M
dados_normalizados <- as.data.frame(lapply(dados[, -ncol(dados)], normaliza))

# Adicionando a variável alvo de volta
dados_normalizados$M <- dados$M 

# Separando as amostras conforme a seleção aleatória feita na questão anterior
dados_treino_norm <- dados_normalizados[sorteio, ]
dados_teste_norm <- dados_normalizados[-sorteio, ]

# a) Testar diferentes valores de k e calcular acurácia
acuracias <- c()
ks <- 1:20 # Vetor para testar os valores de k de 1 a 20

for (k in ks) {
  pred <- knn(
    train = dados_treino_norm[, -ncol(dados_treino_norm)],  # Dados de treino (sem a variável M)
    test = dados_teste_norm[, -ncol(dados_teste_norm)],    # Dados de teste (sem a variável M)
    cl = dados_treino_norm$M,                             # Classes reais do treino
    k = k                                                 # Número de vizinhos
  )
  
  # Calculando a acurácia para o valor de k
  acuracia <- mean(pred == dados_teste_norm$M)
  acuracias <- c(acuracias, acuracia)
}

# Gráfico ocm número de vizinhos versus a acurácia
plot(ks, acuracias, type = "b", # conecta os pontos com linhas e também exibe os marcadores de cada ponto
     col = "purple", pch = 19, # Define o formato e tamanho dos marcadores nos pontos do gráfico.
     xlab = "Número de vizinhos (k)", 
     ylab = "Acurácia", 
     main = "Acurácia para diferentes valores de k")

# b) Escolha do melhor k (concatenando e exibindo texto e valores no console)
  # Essas modificações foram feitas para facilitar o meu trabalho na leitura dos dados
melhor_k <- ks[which.max(acuracias)]
cat("Melhor valor de k:", melhor_k, "\n")
cat("Acurácia correspondente:", max(acuracias), "\n")

# c) Aplicar o modelo com o melhor k e gerar a matriz de confusão

#Fazendo a predição com o melhor número de vizinhos
pred_final <- knn(
  train = dados_treino_norm[, -ncol(dados_treino_norm)], 
  test = dados_teste_norm[, -ncol(dados_teste_norm)], 
  cl = dados_treino_norm$M, 
  k = melhor_k
)

# Aplicando a matriz de confusão para o melhor número de vizinhos
matriz_confusao_knn <- confusionMatrix(
  data = factor(pred_final),          # Predições do modelo
  reference = factor(dados_teste_norm$M), # Valores reais (rótulos verdadeiros)
  positive = "1"                      # Especifica a classe positiva 
)

print(matriz_confusao_knn)

##########################
# 4 – Para o Naive Bayes:
##########################
library(e1071) # Para o Naive Bayes

# a) Ajustando o modelo com a amostra treino
modelo_nb <- naiveBayes(M ~ ., data = dados_treino)

# b.1) Fazendo predições com a amostra teste
pred_nb <- predict(modelo_nb, newdata = dados_teste)

# b.2) Exibindo a matriz de confusão
matriz_confusao_nb <- confusionMatrix(
  data = factor(pred_nb),   # Predições do NB
  reference = factor(dados_teste$M),  # Rótulos reais
  positive = "1"   # Especifica a classe positiva
  )

print(matriz_confusao_nb)

#############################
# Representando gráficamente (letra A)
#############################

# Transformando a matriz de confusão em um data frame a partir da conversão de matriz -> tabela
df_heatmap <- as.data.frame(as.table(matriz_confusao_nb))

# Corrigindo o nome das colunas
colnames(df_heatmap) <- c("Real", "Predito", "Freq")  

# Revisando se os níveis de "Real" e "Predito" são os mesmos (0 e 1)
df_heatmap$Real <- factor(df_heatmap$Real, levels = unique(c(df_heatmap$Real, df_heatmap$Predito)))
df_heatmap$Predito <- factor(df_heatmap$Predito, levels = unique(c(df_heatmap$Real, df_heatmap$Predito)))

# Gerando o gráfico "heatmap"
ggplot(df_heatmap, aes(x = Real, y = Predito, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "lavender", high = "purple4") +
  labs(
    title = "Heatmap - Matriz de Confusão",
    x = "Valores Reais",
    y = "Predições",
    fill = "Frequência"
  ) +
  theme_classic()

# c) Determinando a acurácia do método Naive Bayes

# Acessando apenas a matriz de confusão (tabela) dentro do objeto
matriz_confusao_nb <- confusionMatrix(
  data = factor(pred_nb),
  reference = factor(dados_teste$M),
  positive = "1"
)$table

# Calculando a acurácia
acuracia_nb <- sum(diag(matriz_confusao_nb)) / sum(matriz_confusao_nb)
cat("Acurácia do Naive Bayes:",acuracia_nb, "ou", round(acuracia_nb * 100, 2), "%\n")

#############################
# Fim da entrega PARCIAL - 1
#############################

################################
# 5 – Para a Árvore de Decisão:
################################

# a) Ajustando o modelo com a amostra de treino
modelo_arvore <- rpart(M ~ ., data = dados_treino, method = "class")

# b.1) Fazendo predições com a amostra de teste
pred_arvore <- predict(modelo_arvore, newdata = dados_teste, type = "class")

# b.2) Exibindo a matriz de confusão
matriz_confusao_arvore <- confusionMatrix(
  data = factor(pred_arvore),         # Predições da árvore
  reference = factor(dados_teste$M),  # Rótulos reais
  positive = "1"                      # Especifica a classe positiva
)

print(matriz_confusao_arvore)

# c) Determinando a acurácia do método Árvore de Decisão

# c.1) Acessando apenas a matriz de confusão (tabela) dentro do objeto
matriz_confusao_arvore <- confusionMatrix(
  data = factor(pred_arvore),
  reference = factor(dados_teste$M),
  positive = "1"
)$table

# c.2) Calculando a acurácia - Automatizando a leitura da matriz de confusão
acuracia_arvore <- sum(diag(matriz_confusao_arvore)) / sum(matriz_confusao_arvore)
cat("Acurácia da Árvore de Decisão:", acuracia_arvore, "ou", round(acuracia_arvore * 100, 2), "%\n")

# d.1) Determinando a importância das variáveis no modelo ajustado
importancia_variaveis <- modelo_arvore$variable.importance
cat("\nImportância das variáveis:\n")
print(importancia_variaveis)

# d.2) Gráfico da importância das variáveis
barplot(importancia_variaveis,
        main = "Importância das Variáveis",
        col = "purple2",
        las = 2, 
        horiz = FALSE,
        xlab = "Variáveis", ylab = "Importância")

# e) Visualização da árvore de decisão
# e.1) 1º forma de plotar a árvore de decisão (pacote rattle)
fancyRpartPlot(modelo_arvore)

# e.2) 2ª forma de plotar a árvore de forma mais detalhada (pacote rpart.plot)
rpart.plot(
  modelo_arvore, 
  type = 2,               # Tipo de plot (2 = texto nos nós)
  extra = 104,            # Mostra porcentagens e contagens
  fallen.leaves = TRUE,   # Coloca os nós terminais no mesmo nível
  shadow.col = "gray",    # Adiciona sombra para profundidade
  box.palette = "BuGn",   # Paleta de cores para os nós
  main = "Árvore de Decisão - Visualização Detalhada"
)

#############################
# Fim da entrega PARCIAL - 2
#############################