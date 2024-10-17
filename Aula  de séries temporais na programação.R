#############################################################
# Previsão de séries temporais -  Comparando os 6 previsores

  #Autor: André Cunha - Enpro 4
############################################################

#############
# Pacotes
#############
  
  # Instalação
install.packages("forecast")
install.packages("lubridate")
install.packages("Metrics")
install.packages("tsibble")
install.packages("tsibbledata")
install.packages("TTR")
install.packages("fpp2")

  # Carregando
library(dplyr)
library(forecast)
library(lubridate)
library(Metrics)
library(tsibble)
library(tsibbledata)
library(TTR)
library(fpp2)

########################################################
# 1º Passo - Carregamwento e análise dos dados
########################################################

# Carregamento
Vendas_cerveja <- ausbeer

# Exibe
print(Vendas_cerveja)

# Gráfico de linhas
autoplot(Vendas_cerveja)

# Decompondo a série  
Vendas_cerveja %>% decompose() %>% autoplot()

########################################################
# 2º Passo - Método e modelos de previsão
########################################################

# Divisão dos dados
cerveja_treino <- window(Vendas_cerveja, end = c(1999,4))
cerveja_teste <- window(Vendas_cerveja, start = c(2000,1))

#########################
### Previsor 1: Naive
#########################
prev_naive_teste <- naive(cerveja_teste)

# Imprime os valores previstos
prev_naive_teste

# Calculando Erro
mape(prev_naive_teste$fitted[2:42], cerveja_teste[2:42])*100

# Gráfico
autoplot(prev_naive_teste)

#############################
### Previsor 2: Naive Sazonal
#############################
prev_snaive_teste <- snaive(cerveja_teste)

# Imprime os valores previstos
prev_snaive_teste

# Calculando Erro
mape(prev_snaive_teste$fitted[5:42], cerveja_teste[5:42])*100

# Gráfico
autoplot(prev_snaive_teste)

#############################
### Previsor 3: Média Móvel
#############################

# Escolha da janela de tempo 
  # Lembre-se de escolher a janela com o menor mape
janela <- 2

# Cálculo do previsor na amostra treino
media_movel_treino <-SMA(cerveja_treino,janela)

# Cálculo do Erro na amostra treino
mape(cerveja_treino[janela:176], media_movel_treino[janela:176])*100

# Cálculo do Previsor na amostra teste
prev_media_movel <- SMA(cerveja_teste,janela)

# Cállo do Erro  
mape(cerveja_teste[janela:42],prev_media_movel[janela:42])*100

# O gráfico desse previsor é complicado

#############################
### Previsor 4: Amortecimento exponencial simples
#############################

# Treino (Encontrando o alfa)
aes_treino <- ses(cerveja_treino)

# Imprime o previsor
aes_treino %>% summary()

#Alfa encontrado:
alfa_aes<- 0.1562

# previsões na amostra teste 
prev_aes_teste <- ses(cerveja_teste, alpha = alfa_aes)

# Cálculo do Erro  
mape(prev_aes_teste$fitted, cerveja_teste)*100

# Gráfico
autoplot(prev_aes_teste)

#############################
### Previsor 5: Método de Holt (tendência)
#############################

# Encontrando alfa e beta
holt_treino <- holt(cerveja_treino)

# imprime o previsor
holt_treino %>% summary()

# alfa encontrado
alfa_holt <- 0.0643

# Beta encontrado
beta_holt <- 0.0249

# Previsões na amostra teste
prev_holt_teste <- holt(cerveja_teste, alpha = alfa_holt,
                        beta = beta_holt)
# Cálculo do erro
mape(prev_holt_teste$fitted, cerveja_teste)*100

# Gráfico 
autoplot(prev_holt_teste)

#############################
### Previsor 6: Método de Holt-Winters (tendência e sazonalidade)
#############################

# Previsor na amostra treino
hw_treino <- hw(cerveja_treino)

# Imprime os valores previstos
hw_treino %>% summary()

# alfa
alfa_hw <- 0.2284

# beta
beta_hw <- 0.0375

# gama
gama_hw <- 0.2031

# Cálculo do previsor na amostra teste
prev_hw_teste <- hw(cerveja_teste, alpha = alfa_hw,
                    beta = beta_hw, gamma = gama_hw)

# Cálculo do erro
mape(prev_hw_teste$fitted, cerveja_teste)*100

# Gráfico
autoplot(prev_hw_teste)

# Imprime os valores previstos
prev_hw_teste

##########################
####     FIM     #########
##########################