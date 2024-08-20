### Atividade 6 - Regressão Linear Múltipla - dataset:NYC
  #Autor: André Cunha

# Bibliotecas
library(corrplot)
library(dplyr)
library(ggplot2)

# importe a base de dados
library(readr)
nyc <- read_csv("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Atividades - Enviadas/Atividade 6 - Regressão Linear Múltipla - dataset_NYC/nyc.csv")
View(nyc)

# Retire as colunas indesejadas
nyc_ajustado <- nyc[,-c(1,2)]

### a) Faça uma matriz de correlações 
Matriz <- cor(nyc_ajustado) # Cria a matriz

# Exibe a matriiz
Matriz %>% summary()

# Exibe graficamente a matriz (inclusive com o "calor" de cada variável)
corrplot(Matriz, method = "number")

# As variáveis food(3º), decor(1º) e service(2º) parecem ter uma boa correlação com Price. 

#### b) Construa um diagrama de dispersão para cada uma dessas variáveis:

  #b.1) FOOD -> essa variável indica uma correlação linear positiva
ggplot(data = nyc_ajustado, aes( x = Food, y = Price)) +
  geom_point() +
  theme_classic() +
  xlab("Food") +
  ylab("Price") +
  geom_smooth(method = "lm", se = FALSE)

  # Coeficiente de correlação de pearson - DECOR
  cor(nyc_ajustado$Food, nyc_ajustado$Price)

  #b.2) Service -> essa variável indica uma correlação linear positiva
ggplot(data = nyc_ajustado, aes( x = Service, y = Price)) +
  geom_point() +
  theme_classic() +
  xlab("Service") +
  ylab("Price") +
  geom_smooth(method = "lm", se = FALSE)
  
  # Coeficiente de correlação de pearson - SERVCICE
  cor(nyc_ajustado$Service, nyc_ajustado$Price)

  #b.3) Decor -> essa variável indica uma correlação linear positiva
ggplot(data = nyc_ajustado, aes( x = Decor, y = Price)) +
  geom_point() +
  theme_classic() +
  xlab("Decor") +
  ylab("Price") +
  geom_smooth(method = "lm", se = FALSE)

  # Coeficiente de correlação de pearson - DECOR
  cor(nyc_ajustado$Decor, nyc_ajustado$Price)
  
  ###c) Ajuste um modelo de regressão linear simples para cada uma das variáveis 
  
### c.1) AJuste de modelo - Food
modelo_food <- lm(formula = Price ~ Food, data = nyc_ajustado)
  
  modelo_food %>% summary() # R-square = 39.32%
  
### c.2) AJuste de modelo - Service
modelo_Service <- lm(formula = Price ~ Service, data = nyc_ajustado)

  modelo_Service %>% summary() # R-square = 41.11%

### c.3) AJuste de modelo - Decor
modelo_Decor <- lm(formula = Price ~ Decor, data = nyc_ajustado)

  modelo_Decor %>% summary() # R-square = 52.47%
  
#### d) Compare os preços dos restaurantes a Leste e a Oeste
  
  # boxplot para análise da média de preço entre as localidades
  boxplot(nyc$Price ~ nyc$East,
          xlab = "Leste ou Oeste",
          ylab = "Preço",
          main = "Boxplot")
  
  # Neste caso, precisaremos separar nossa base de dados entre a condição de localidade
  # para identificar medidas descritivas corretas, uma vez que as mesmas em relação
  # ao grupo total seriam muito genéricas.
  
# data frame - east == 1
nyc_1 <- nyc %>% filter(East == 1) 

  #Média
  mean(nyc_1$Price)

  # Desv-pad
  sd(nyc_1$Price)
  
  #Mínimo
  min(nyc_1$Price)
  
  #Máximo
  max(nyc_1$Price)
  
  #Mediana
  median(nyc_1$Price)
  
  
# data frame - east == 0
nyc_0 <- nyc %>% filter(East == 0) 
  
  #Média
  mean(nyc_0$Price)
  
  #Desv-pad
  sd(nyc_0$Price)

  #Mínimo
  min(nyc_0$Price)

  #Máximo
  max(nyc_0$Price)
  
  #Mediana
  median(nyc_0$Price)

### e) ajuste modelos de regressão separados para Leste e Oeste.
  
  # e.1) Modelo - East=0
  modelo_nyc0 <- lm(formula = Price ~ East, data = nyc_0)
  
  modelo_nyc0 %>% summary() # imprime o modelo no console 
  
#  e.2) Modelo - East=1 
  modelo_nyc1 <- lm(formula = Price ~ East, data = nyc_1)
  
  modelo_nyc1 %>% summary()  # imprime o modelo no console 
  
### f)Agora construa um modelo com o Preço como variável de resposta e
    #começando com todas as demais (exceto o nome do restaurante) como variáveis explicativas.
  
modelo_geral <- lm(formula = Price ~ Food+Decor+Service+East, data = nyc)

modelo_geral %>% summary()

### g) Faça a análise dos resíduos do modelo
# Gráfico dos resíduos contra os valores de x (price)
plot(nyc$Price, rstandard(modelo_geral),
     xlab = "Price", ylab = "Resíduos",
     main = "Gráfico de resíduos contra price")
abline(0,0)

  # HISTOGRAMA para verificar a média dos erros por valor de X (t):
  hist(modelo_geral$residuals,
       main = "Histograma da média de resíduos",
       xlab = "Resíduo",
       ylab = "Frenquencia do resíduo")

