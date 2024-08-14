####### Exercícios de Aula - dataset: mtcars #######
  #' Autor: André Cunha
#' Nestes execícios, Iremos abordar o tema: Regressão Linear simples.

#' Bibliotecas necessárias:
library(ggplot2)
library(corrplot)

#' Primeiro, iremos Adiciocar a base de dados ao ambiente.
Dados_carro <- mtcars

  #' Em seguida, Entender as variáveis contidas na base de dados.
  ?mtcars

#' 1) Para começar uma análise de regressão linear, é necessário ter uma ideia mínima sobre qual a correlação dos dados escolhidos
#' Para isso, faça um Diagrama de disperção entre duas variáveis, neste caso:  hp(preditora) e mpg(Dependente).

ggplot(data = Dados_carro, aes(x = hp, y = mpg)) +  # Informa qual o data frame e a coluna as quais se localizam os dados.
  geom_point() +  # define o tipo de gráifco, no caso, diagrama de disperção.
  theme_classic() + # Atribui o tema "classic" ao gráfico.
  xlab("hp") +  # nomeia o eixo X.
  ylab("mpg")+  # nomeia o eixo Y.  
  geom_smooth(method = "lm", se = FALSE)  # atribui a reta ao gráfico.

    #' Com base no gráfico, responda O que você acha sobre a associação entre as variáveis escolhidas.

#' 2) Após identificar superficialmente alguma correlação, é necessário identificar o quão forte ela é
#' Para isso, calcule o  Coeficiente de correlação linear de pearson. 

cor(Dados_carro$hp, Dados_carro$mpg)
  
    #' Responda: O valor do coeficiente confirma sua expectativa? (confira a tabela de pearson).

#' 3) Visando predizer dados futuros para validar a correlação das variáveis:
#' Ajuste um modelo de regressão linear simples ( y = alpha + beta * x  ).

Dados_carro_2 <- lm(formula = mpg ~ hp, data = Dados_carro)
                  # lm(): Comando p/ criar um modelo.
                  # (formula = variavel dependente ~ variavel preditora): especifica a formula do modelo.
                  # (data = base de dados): informa que os dados p/ o modelo estão no data frame escolhido.

summary(Dados_carro_2) 
      #' Exibe o resultado no console.

#' 4) Agora responda:

##' 4.1) Qual é a equação do modelo ajustado?
    #' Milhas por galão = 30.09886 - 0.06823 * hp.

##' 4.2) Para cada incremento de uma unidade de hp, quanto aumenta ou diminui o mpg? 
    #' cada 1 mpg aumenta ou diminui 0.06823 horsepowers.

##' 4.3) Qual o valor do coeficiente de determinação? Como você o interpreta?
    #' R-square ou coef. de determinação = 0.6024 ou 60.24%.
    #' o coeficiente de determinação indica que, neste caso,
      #' ele é capaz de explicar relativamente bem a correlação entre as variáveis escolhidas.

#' 5) Faça o gráfico dos resíduos contra valores de x. 

plot(Dados_carro$hp, rstandard(Dados_carro_2), xlab = "hp", ylab = "Resíduos")
abline(0,0)
    #' plot(): cria um gráfico de disperção.
    #' (data.frame$hp): eixo x.
    #' (rstandard(modelo_regressão)): define o eixo y como resíduos padronizados (ajustados em média 0) do modelo.
    #' abline(0,0): define uma reta ao gráfico e determina sua localização.

#' Responda: o que sugerem os resultados para a hipótese de homoscedasticidade dos resíduos (ou erro ajustado)?
    #' No geral, os resíduos demonstram seguir a hipótese de homoscedasticidade, 
    #' entretanto, há alguns poucos dados para os quais os resíduos são altos. Esses dados 
    #' são considerados outliers e devem ser estudados separadamente dos demais.

#' 6) Faça uma predição de mpg para hp=200.
#' Para isso, impute valores em seu data frame para testar a capacidade de predição do seu modelo

novo_dado <- data.frame(hp = 200)
    #' Cria uma nova base de dados, no caso, com uma única linha e coluna, sendo hp = 200.

predict(Dados_carro_2, novo_dado)
    #' Predict(): faz a previsão do novo_dado em relação ao modelo ajustado

#' Por fim, sua análise de regressão linear simples está feita, uma vez que,
#' você conseguiu comprovar uma correlação entre duas variáveis.

######################## FIM ######################### 

