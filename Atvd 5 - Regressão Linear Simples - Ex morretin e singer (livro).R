#  Execício 6.7 do livro Morettin e Singer
  # Análise de regressão linear simples

  # Autor: André Cunha

# Bibliotecas
library(ggplot2)
library(corrplot)
library(dplyr)

#################################
### QUestão 1
################################

# Declarando a base de dados

dados1 <- data.frame(volume = c(656, 692, 588, 799, 766, 800, 693, 602, 737, 921, 923, 945, 816, 584, 642, 970), 
                     Peso = c(630, 745, 690, 890, 825, 960, 835, 570, 705, 955, 990, 725, 840, 640, 740, 945))

#### i) Proponha um modelo de regressão linear simples para "Dados1" e interprete os parâmetros
modelo_ajustado <- lm(formula = Peso ~ volume, data = dados1)

# Exibe o modelo
modelo_ajustado %>% summary()

# Peso = 213.2762 + 0.7642 * volume

# Sendo alpha=213.2762 ; beta: 0.7642.
# Logo: a cada 1cm^3 de volume que o orgão tiver, seu peso aumentará em 0.7642.

#### ii) Construa um diagrama de dispersão apropriado:

ggplot(data = dados1, aes( x = volume, y = Peso)) +
  geom_point() +
  theme_classic() +
  xlab("volume") +
  ylab("Peso") +
  ggtitle("Diagrama de Dispersão") +
  geom_smooth(method = "lm", se = FALSE)

  # Este diagrama sugere uma correlação linear positiva entre as variáveis

#### iii) Determine a qualidade do modelo ajustado:

# R-square = 58.11%, ou seja, este modelo é capaz de explicar 58.11% da variação do peso.

# A anáise de homocedasticidade, feita para verificar se o modelo possui bom desempenho
# para todas as faixas de valores, revelou que é possível aceitar a hipótese de homocedasticidade,
# uma vez que os valores do erro se encontram, em sua maioria, perto de 0, exceto alguns outliers.
  
# 1º Verifica-se a média (teste se a média é zero)
  t.test(modelo_ajustado$residuals)
  
  #P-value = 1, ou seja, 
  # P-value>=0.05, não há evidência para rejeitar a hiótese de média zero

  # 2º verifica-se a normalidade dos resíduos
  qqnorm(modelo_ajustado$residuals)
  qqline(modelo_ajustado$residuals)

  shapiro.test(modelo_ajustado$residuals)

  hist(modelo_ajustado$residuals,
      main = "Histograma de resíduos contra x (peso)",
       xlab = "Resíduo",
       ylab = "Frenquencia do resíduo")

  # Vamos verificar a hipóstese de homocedasticidade deste modelo.
  plot(dados1$volume, rstandard(modelo_ajustado),
       xlab = "volume", ylab = "Resíduos",
       main = "Gráfico de resíduos contra X")
  abline(0,0)
  
  plot(modelo_ajustado)
  
### iv) Defina os intervalos de confiança para os parâmetros:

modelo_ajustado %>% confint()

# Considerando uma confiança de 95% nos resultados, é possível predizer que,
# teremos um valor de intercepto dentro do intervalo -72.69 e 499.24 e,
# um valor de volume entre  0.3922543 - 1.136109.
  
  # OU SEJA: o coeficiente "volume" é estatísticamente significativo e,
  # possui um efeito postivo na variável peso

  # Entretanto, o intervalo de intercepto encontrado é uma faixa muito ampla de valores,
  # o que indica incerteza na estimativa desses parâmetros.

# v) Construa uma tabela com o intervalo de confiança  para o peso esperado do órgão,
  # para os valores de volmue = 600,700,800,900 e 1000 e faça a predição desse valores: 

novo_dados1 <- data.frame(volume = c(600,700,800,900,1000)) # novo data frame

predict(modelo_ajustado, novo_dados1, interval = "predict")

# vi) Refaça os itens anteriores considerando um modelo linear simples sem intercepto,
  # Qual dos modelos você acha mais conveniente? justifique.

modelo_ajustado2 <- lm(formula = Peso ~ volume+0, data = dados1)

modelo_ajustado2 %>% summary()

  # Peso = 0 + 0.03003 * volume
# Logo: a cada 1cm^3 de volume que o orgão tiver, seu peso aumentará em 0.03003.

# Este novo modelo é capaz de explicar 98.76% da variação do peso do órgão a partir do volume.
# Portanto, o modelo com o intercpto em 0 possui uma qualidade maior em seus resultados,
# visto que não considera oultliers teóricos, como por exemplo volume=0.

#######################################
###    FIM   ###
#######################################