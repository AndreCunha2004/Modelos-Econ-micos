# Exercícios de Regressão Linear Simples - Base de Dados: Poluentes
#Autor: André Cunha

# Bibiliotecas necessárias
library(ggplot2)
library(corrplot)
library(readxl)

# Para começar a atividade, carregue a base de dados:
poluicao <- read_excel("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Atividades - Enviadas/Atividade HomeOfice - Poluição/poluicao.xls")

# Abra a base de dados e entenda as variáveis
View(poluicao)

?poluicao

# Passo 1: Construa um diagrama de dispersão para investigar
# a relação linear entre as variáveis dia (preditora) e monóxido de carbono (variável dependente).

ggplot(data = poluicao, aes(x = dia, y = CO)) + # Adiciona a função ggplot, informa a base de dados e indica os eixos
  geom_point() +  # define o estilo do gráfico como diagrama de dispersão
  theme_classic() + # define o tema do background do gráfico
  xlab("Tempo") + # Título do eixo x
  ylab("Monoxido de carbono") + # Título do eixo y
  geom_smooth(method = "lm", se = FALSE) # Adiciona a reta ao gráfico e não considera os intervalos de confiança

# Passo 2: Calcule o coeficiente de correlação linear de Pearson para as variáveis

cor(poluicao$dia, poluicao$CO)

  #Este coeficiente indica que há uma correlação positiva moderada.

# Passo 3: Ajuste um modelo de regressão linear. 
Modelo_regressao <- lm(formula = CO ~ dia, data = poluicao)

  # Agora, veja o resultado do seu modelo
  summary(Modelo_regressao)
  
# Passo 4: Escreva como fica a equação do modelo. 
  # Monóxido de Carbono = 6.264608 + 0.019827 * dia
  
# Passo 5: Coeficiente de determinação possui uma taxa de explicação de  19.96% (explica pouco o modelo).
  
# Passo 6: Avalie a hipótese de homocedasticidade:
  
plot(poluicao$dia, rstandard(Modelo_regressao),
     xlab = "dia", ylab = "Resíduos",
     main = "Gráfico de resíduos contra X")
  abline(0,0)
  
  # Passo 7: HISTOGRAMA para verificar a média dos erros por valor de X (t):
  
  hist(Modelo_regressao$residuals,
       main = "Histograma de resíduos contra x (t)",
       xlab = "Resíduo",
       ylab = "Frenquencia do resíduo")
  
  # Nesse contexto, os resíduos mostram seguir a hipótese de homocedasticidade
  # Porém, é importante lembrar que outliers devem ser tratados separadamente,
  # estes valores extremos são identificados como há valores a partir de um determinado nível da variável x que os dados se dispersam.
  
# Passo 8: Faça uma predição de mpg para dia=121
  #' Para isso, impute valores em um data frame para testar a capacidade de predição do seu modelo
  
  novo_dad0 <- data.frame(dia = 121)
  
  predict(Modelo_regressao, novo_dad0)
  
    #Conclusao: no dia 121, a concentração de monóxido de carbono dessa região será de 8.663725
  
  #' Por fim, sua análise de regressão linear simples está feita, uma vez que,
  #' você conseguiu comprovar uma correlação entre duas variáveis.
  
  ######################## FIM ######################### 
  