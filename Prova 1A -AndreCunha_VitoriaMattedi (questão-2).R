## Modelos Econômicos e Quantitativos – Avaliação 1

# Autor: André Cunha e Vitória Mattedi

#############
#Questão 2: Base de dados cerveja
#############

######### Pacotes #########
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
###########################

# Letra a) ############################################
cerveja <- read_csv("C:/Users/andre/Downloads/cerveja.csv")
View(cerveja)

# Letra b) ############################################

Matriz <- cor(cerveja)
corrplot(Matriz, method = "number")

ggplot(data = cerveja, aes(x = temp_media, y = consumo)) +
  geom_point(size = 4) +
  xlab("Temperatura média") +
  ylab("Consumo") + 
  theme_classic()

ggplot(data = cerveja, aes(x = temp_min, y = consumo)) +
  geom_point(size = 4) +
  xlab("Temperatura mínima") +
  ylab("Consumo") + 
  theme_classic()

ggplot(data = cerveja, aes(x = temp_max, y = consumo)) +
  geom_point(size = 4) +
  xlab("Temperatura máxima") +
  ylab("Consumo") + 
  theme_classic()

ggplot(data = cerveja, aes(x = precipitacao, y = consumo)) +
  geom_point(size = 4) +
  xlab("precipitação") +
  ylab("Consumo") + 
  theme_classic()

ggplot(data = cerveja, aes(x = final_de_semana , y = consumo)) +
  geom_point(size = 4) +
  xlab("Final de semana") +
  ylab("Consumo") + 
  theme_classic()

#Letra C) ############################################


modeloT <- lm(formula =  consumo ~ temp_max, data = cerveja)
summary(modeloT) 

#Letra d) ############################################

modelo2 <- lm(formula =  consumo ~. , data = cerveja)
summary(modelo2) 

modelo3 <- lm(formula =  consumo ~ temp_media + final_de_semana + precipitacao , data = cerveja)
summary(modelo3) 

#Letra e) ############################################
hist(modelo3$residuals,
     main = "Histograma de resíduos contra x (t)",
     xlab = "Resíduo",
     ylab = "Frenquencia do resíduo")

plot(cerveja$consumo, modelo3$residuals)
abline(0,0)

#Letra f) ############################################


novo_dado <- data.frame( temp_media = 20, temp_min = 16, temp_max = 24, precipitacao = 0, final_de_semana = 1)

predict(modelo3, novo_dado)

####################
####    FIM
####################