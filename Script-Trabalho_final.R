############################################################################################
# Trabalho final - Modelos Econômicos
# Tema: Predição do valor de Aluguéis em São Paulo, Brazil
    # Grupo: André Cunha, Luana Spoladori, Rayane Cardoso, Tainá Zumerle  e Vitória Luiza. 
############################################################################################

# Bibliotecas novas
install.packages("FNN")

# Bibliotecas necessárias
library(readr) # Para leitura dos dados
library(FNN) # Para o knn regressão
library(caret) # Para métricas e matriz de confusão
library(dplyr) # Para manipulação de dados
library(e1071)  # Para o Naive Bayes
library(ggplot2)  # Para visualização


# Carregando Data source
data <- read_csv("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Trabalho final/data.csv")
View(data)

###################################
# Método K-nearest neighbors (KNN)
###################################

# 1. Primeiro, precisamos converter as variáveis categóricas em numéricas
# (O Knn só funciona com variáveis numéricas)
data$type <- as.numeric(factor(data$type))
data$district <- as.numeric(factor(data$district))
data$address <- as.numeric(factor(data$address))

  # 2. Função p/ normalizar os dados
normaliza <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

  # 3. Normalizando as variaveis explicativas
data_normalizados <- as.data.frame(
  lapply(data[, c("address", "district","area", "bedrooms", "garage", "type")],
         normaliza))

# 4. Adicionando a variável alvo de volta após normalizar
data_normalizados$rent <- data$rent 

# 5. Separando as amostras aleatoriamente na proporção (80-20)
set.seed(7377) # Semente de reprodutibilidade

sorteio <- sample(
  1:nrow(data_normalizados), 
  0.8*nrow(data_normalizados)) # automatizei as contas que eu faria no console

data_treino_norm <- data_normalizados[sorteio, ] # 80%
data_teste_norm <- data_normalizados[-sorteio, ] # 20%

# 6. Testando diferentes valores de k e calculando o erro médio absoluto (MAE)
ks <- 1:20 # Números de vizinhos que serão testados
maes <- c() # Vetor p/ armazenar os erros

for (k in ks) {
  pred <- knn.reg(
    train = data_treino_norm[, -ncol(data_treino_norm)], # Dados de treino
    test = data_teste_norm[, -ncol(data_teste_norm)],   # Dados de teste
    y = data_treino_norm$rent,                         # Variável alvo do treino
    k = k                                              # Número de vizinhos
  )$pred
  
  # 6.1 Calculando a acurácia para o valor de K
  mae <- mean(abs(pred - data_teste_norm$rent))
  maes <- c(maes, mae)
}

# 7. Gráfico com número de vizinhos versus acurácia
plot(ks, maes, type = "b", 
     col = "green2", pch = 19, 
     xlab = "Número de vizinhos (k)", 
     ylab = "Erro Médio Absoluto (MAE)", 
     main = "Erro Médio Absoluto para diferentes valores de k")

# 8. Escolha do melhor k
melhor_k <- ks[which.min(maes)]
cat("Melhor valor de k:", melhor_k, "\n")
cat("Erro Médio Absoluto correspondente:R$", min(maes), "\n")

###################################
# Método Naive Bayes
###################################

# ATENÇÂO!
  # De um modelo para outro, limpe o workspace! "Global Environment"
  rm(list = ls())  # Remove todos os objetos
  gc()             # Libera memória


# Carregue os dados originais novamente 
data <- read_csv("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Trabalho final/data.csv")

# 1. pré-processamento dos dados
data <- data %>% select(-total)
data$type <- as.factor(data$type)
data$district <- as.factor(data$district)
data$address <- as.factor(data$address)
data$rent <- as.factor(data$rent)

# 2. Separando as amostras a proporção treino-teste (80-20)
set.seed(7377)

sorteio <- sample(1:nrow(data), size = 0.8 * nrow(data))

dados_treino <- data[sorteio,]
dados_teste <- data[-sorteio,]

# 3. Ajustando o modelo Naive Bayes
modelo_nb <- naiveBayes(rent ~ ., data = dados_treino)

# 4. Fazendo predições na amostra teste
pred_nb <- predict(modelo_nb, newdata = dados_teste)

# 5. Construindo a Matriz de Confusão
matriz_confusao_nb <- confusionMatrix(
  data = pred_nb,
  reference =dados_teste$rent) # comparando com valores reais

print(matriz_confusao_nb)

######################################################
# ATENÇÃO! - A MATRIZ DE CONFUSÃO NÃO carregou NO MEU PC
                      #Ass: André
#######################################################

#-------------------------------------------------------#
# Erro: Diferença na qntd de levels - Corrigindo com IA
#-------------------------------------------------------#

# Comparar os níveis de ambas as variáveis
levels_pred <- levels(factor(pred_nb))
levels_real <- levels(factor(dados_teste$rent))

cat("Níveis nas predições:", levels_pred, "\n")
cat("Níveis nos dados reais:", levels_real, "\n")

# Verificar valores que não estão correspondendo
extra_niveis_pred <- setdiff(levels_pred, levels_real)
extra_niveis_real <- setdiff(levels_real, levels_pred)

cat("Níveis extras em pred_nb:", extra_niveis_pred, "\n")
cat("Níveis extras em rent:", extra_niveis_real, "\n")

# Ajustar os níveis de pred_nb para que coincidam com os de rent
pred_nb <- factor(pred_nb, levels = levels(factor(dados_teste$rent)))

# Gerar a matriz de confusão com níveis corrigidos
matriz_confusao_nb <- confusionMatrix(
  data = factor(pred_nb),        # Predições corrigidas
  reference = factor(dados_teste$rent),  # Rótulos reais
  positive = "1"                 # Classe positiva, se aplicável
)

print(matriz_confusao_nb)

#----------------------------------------------------------#

# 6. Visualização da matriz de confusão com heatmap

  # 6.1 Transformando a matriz em data frame
  df_heatmap <- as.data.frame(as.table(matriz_confusao_nb$table))
  
  # 6.2 Ajustando os nomes das colunas
  colnames(df_heatmap) <- c("Real", "Predito", "Freq")

  # 6.3 Gerando o heatmap
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

################################################
  # POUPE SEU TEMPO E VEJA O RESULTADO DO MÉTODO
################################################
# 7. Calculando a acurácia do modelo
acuracia_nb <- sum(diag(matriz_confusao_nb$table)) / sum(matriz_confusao_nb$table)
cat("Acurácia do Naive Bayes:", acuracia_nb, "ou", round(acuracia_nb * 100, 2), "%\n")

########################################
# REGRESSÂO LINEAR
########################################
  # ATENÇÂO!
# De um modelo para outro, limpe o workspace! "Global Environment"
rm(list = ls())  # Remove todos os objetos
gc()             # Libera memória


# Carregue os dados originais novamente 
dados <- read_csv("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Trabalho final/data.csv")

# 1. pré-processamento dos dados
dados <- dados %>%
  select(-total) %>%              # Remover a variável 'total'
  mutate(
    type = as.factor(type),
    district = as.factor(district),
    address = as.factor(address),
    rent = as.numeric(rent)       # Converter 'rent' para numérico
  )

# 2. Separando as amostras a proporção treino-teste (80-20)
# 2.1 Semente de reprodutibilidade
set.seed(7377)

# 2.2 Seleção de valores aleatórios
sorteio <- sample(1:nrow(dados), size = 0.8 * nrow(dados))

# 2.3 Cria as amostras
dados_treino <- dados[sorteio, ]
dados_teste <- dados[-sorteio, ]

# 3. Ajustando modelo de Regressão Linear Múltipla
modelo_rg <- lm(rent ~., data = dados_treino)

# 4. Exibindo o modelo
summary(modelo_rg)

######################################################
# ATENÇÃO! - O MODELO NÃO carregou NO MEU PC
#Ass: André
#######################################################

# 5. Fazendo predições no conjunto de teste
pred_rent <- predict(modelo_rg, newdata = dados_teste)

# 6. Calculando o erro médio absoluto (MAE) e outras métricas
mae <- mean(abs(pred_rent - dados_teste$rent))
cat("Erro Médio Absoluto (MAE):", mae, "\n")

# 7. Visualizando resultados reais vs preditos
ggplot(data.frame(real = dados_teste$rent, predito = pred_rent), aes(x = real, y = predito)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Valores Reais vs Preditos",
    x = "Valores Reais (rent)",
    y = "Valores Preditos (rent)"
  ) +
  theme_classic()

# 8. Exibindo a relação entre predições e valores reais
correlacao <- cor(dados_teste$rent, pred_rent)
cat("Correlação entre valores reais e preditos:", correlacao, "\n")

##################################
#OPEN AI
##################################

# 2. Pré-processamento dos dados
dados <- dados %>%
  select(-total) %>%              # Remover a variável 'total'
  mutate(
    type = as.factor(type),
    district = as.factor(district),
    address = as.factor(address),
    rent = as.numeric(rent)       # Converter 'rent' para numérico
  )

# 3. Separar as amostras em treino (80%) e teste (20%)
set.seed(7377)  # Semente para reprodutibilidade
sorteio <- sample(1:nrow(dados), size = 0.8 * nrow(dados))
dados_treino <- dados[sorteio, ]
dados_teste <- dados[-sorteio, ]

# 4. Ajustar o modelo de Regressão Linear
modelo_rg <- lm(rent ~ ., data = dados_treino)

# Exibir o resumo do modelo
summary(modelo_rg)

# 5. Fazer predições no conjunto de teste
pred_rent <- predict(modelo_rg, newdata = dados_teste)

# 6. Calcular métricas de erro
mae <- mean(abs(pred_rent - dados_teste$rent))  # Erro Médio Absoluto
cat("Erro Médio Absoluto (MAE):", mae, "\n")

# 7. Visualizar os resultados
# Gráfico de valores reais vs preditos
resultado <- data.frame(real = dados_teste$rent, predito = pred_rent)
ggplot(resultado, aes(x = real, y = predito)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Valores Reais vs Preditos",
    x = "Valores Reais (rent)",
    y = "Valores Preditos (rent)"
  ) +
  theme_classic()

# Exibir a correlação entre os valores reais e preditos
correlacao <- cor(dados_teste$rent, pred_rent)
cat("Correlação entre valores reais e preditos:", correlacao, "\n")

