###############################################
# Prova 2 - Modelos Ecocômicos e Quantitativos
# Tema: Análise de séries temporais 
# Autor: André Cunha e Rayane Cardoso
###############################################


# Carregando bibliotecas
library(dplyr)
library(forecast)
library(lubridate)
library(Metrics)
library(tsibble)
library(tsibbledata)
library(TTR)
library(fpp2)

# Questao 1
#Gráfico de linha
autoplot(Dados)

#Decomposição da série
Dados %>% decompose() %>% autoplot()

##########################################

# Questao 2
#Divisão dos dados
Dados_treino <- window(Dados, end = c(2004,12))
Dados_teste <- window(Dados, start = c(2005,1))

##########################################

# Questao 3 - Naives
prev_naive_teste <- naive(Dados_teste)

#Vendo os valores previstos:
prev_naive_teste

#Calculando o erro:
mape(prev_naive_teste$fitted[2:102], Dados_teste[2:102])*100

#Mostrando o gráfico
autoplot(prev_naive_teste)

#### Previsor Naive Sazonal
prev_snaive_teste <- snaive(Dados_teste)
prev_snaive_teste

#Calculando o erro:
mape(prev_snaive_teste$fitted[13:102], Dados_teste[13:102])*100

#Mostrando o gráfico
autoplot(prev_snaive_teste)

##########################################

# Questao 4 - Previsor Médias Móveis
# Testando as janela (escolher a janela com menor mape):

# Vetor para armazenar os valores de MAPE
Vetor <- numeric() # vazio
janelas <- 2:12

# Calcule o MAPE para cada janela e armazene no vetor mapes
for (janela in janelas) {
  SMA_treino <- SMA(Dados_treino, janela)
  mape_val <- mape(Dados_treino[janela:384], SMA_treino[janela:384]) * 100
  Vetor <- c(mapes, mape_val)
}

# Plotando os MAPEs
plot(janelas, mapes, type = "b", pch = 19, col = "blue",
     xlab = "Janelas", ylab = "MAPE (%)", main = "MAPE por Janela")

#Previsões na amostra de teste:
janela <- 2
prev_SMA<-SMA(Dados_teste, janela)
mape(Dados_teste[janela:102], prev_SMA[janela:102])*100

##########################################

# Questao 5 - Naives
# Treino | Encontrando alfa
aes_treino <- ses(Dados_treino)
summary(aes_treino)
alfa_aes <- 0.9324

#Previsões na amostra de teste
prev_aes_teste <- ses(Dados_teste, alpha = alfa_aes)
prev_aes_teste

#Erro
mape(prev_aes_teste$fitted, Dados_teste)*100

#Gráfico
autoplot(prev_aes_teste)

##########################################

# Questao 6 - Holt
holt_treino <- holt(Dados_treino)
summary(holt_treino)
alfa_holt <- 0.0499
beta_holt <- 1e-04

#Previsão de Holt
prev_holt_teste <- holt(Dados_teste, alpha = alfa_holt, beta = beta_holt)

#Erro
mape(prev_holt_teste$fitted,Dados_teste)*100

#Gráfico
autoplot(prev_holt_teste)

##########################################

# Questao 7 Método Holt-Winters
#Encontrando os valores de alfa, beta e gama
hw_treino <- hw(Dados_treino)
summary(hw_treino)
alfa_hw <- 0.2458
beta_hw <- 4e-04
gama_hm <- 0.332

#Previsões da amostra de teste
prev_hw_teste <- hw(Dados_teste, alpha = alfa_hw, beta = beta_hw, gamma = gama_hm)

#Erro
mape(prev_hw_teste$fitted, Dados_teste)*100

#Gráfico
autoplot(prev_hw_teste)

#Vendo as previsões
prev_hw_teste

##########################################

# Questao 9
# Fazer previsões para os próximos instantes
n_periodos_previstos <- 12
prev_hw_futuro <- forecast(hw(Dados, alpha = alfa_hw, beta = beta_hw, gamma = gamma_hw), h =
                             n_periodos_previstos)
# Visualizar previsões
print(prev_hw_futuro)

# Gráfico das previsões
autoplot(prev_hw_futuro) +
  ggtitle("Previsões usando o Método Holt-Winters") +
  xlab("Tempo") +
  ylab("Valores Previsto") +
  theme_minimal()
