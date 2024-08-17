# Atividade 4 de Regressão Linear Simples - Base de dados "VENTO"
  # Autor: André Cunha

#Bibliotecas necessárias
library(ggplot2)
library(readxl)
library(corrplot)

# Carregue a base de dados:
dados_vento <- read_excel("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Atividades - Enviadas/Atividade 4 - Regressão Linear Simples - Vento/vento.xls")

# Veja e entenda o conjunto de dados

View(dados_vento)

# Passo 1: a fim de tentar identificar uma correlação de maneira superficial, 
# é necessário fazer um diagrama de dispersão e identificar se a mesma existe e qual a sua relevãncia:

ggplot(data = dados_vento, aes( x = t, y = vt)) +
  geom_point() +
  theme_classic() +
  xlab("dias") +
  ylab("velocidade do vento") +
  geom_smooth(method = "lm", se = FALSE)

    # Neste diagrama de dispesão é possível identificar uma correlação negativa dos valores,
    # entretanto, é apenas uma conclusão superficial.

# Passo 2: para determinar se há de fato alguma correlação,
# vamos calcular o coeficiente de correlação de pearson:

cor(dados_vento$t, dados_vento$vt)

  # Ao observar o valor de -0.4808565, é possível afirmar uma correlação negativa moderada.
  # o que confirma a suposição feita no diagrama de dispersão anterior.

# Passo 3: A fim de utilizar essa base de dados para predizer novos valores de VT,
# ajuste um modelo de regressão Linear simples para replicar a correlação para novos valores:

Modelo_ajustado <- lm(formula = vt ~ t, data = dados_vento)

  #Analise o modelo que você ajustou:

  summary(Modelo_ajustado)
  
  # Passo 4: Com esses dados, podemos montar a equação do nosso modelo e predizer alguns valores à mão:
    
  # Velocidade do vento (vt) = 30.0343 - 1.4543 * dia (t)
      
      # A cada 1 que avança na amostra, menor é a velocidade registrada do vento.
    
  # Neste modelo, identificamos os valores como y=aplha + beta*x
  
    # também encontraremos o coef. de determinação, o qual diz quanto esse modelo explica bem a correlação:
  
    # dito como R-square no console,ou seja, este  modelo é capaz de explicare 23.12% da variação do vento.
  
# Passo 5: Verificar a hipóstese de homocedasticidade deste modelo.

plot(dados_vento$t, rstandard(Modelo_ajustado),
     xlab = "dia", ylab = "Resíduos",
     main = "Gráfico de resíduos contra X")
abline(0,0)

  # A anáise de homocedasticidade, feita para verificar se o modelo possui bom desempenho
  # para todas as faixas de valores, revelou que é possível aceitar a hipótese de homocedasticidade,
  # uma vez que os valores do erro se encontram, em sua maioria, perto de 0, exceto alguns outliers.

# Passo 7: HISTOGRAMA para verificar a média dos erros por valor de X (t):

hist(Modelo_ajustado$residuals,
     main = "Histograma de resíduos contra x (t)",
     xlab = "Resíduo",
     ylab = "Frenquencia do resíduo")

  # O histograma demonstra que o modelo está realizando previsões razoáveis,
  # Uma vez que a maior frequência encontrada está na media 0-10 de resíduos.

# Passo 8: Faça uma predição para avaliar a velocidade do vento nos primeiros 15 dias de 1994

novo_dado <- data.frame(t = c(16,17,18,19,20))

predict(Modelo_ajustado, novo_dado)

  # Conclusão: à medida que os dias vão passando no ano de 1974, a velocidade do vento foi reduzindo.
  # exatamente conforme o modelo identificou na amostra e prediz que seja nos 5 dias após os 15 dias iniciais.

# Por fim, essa análise de regressão linear simples está feita, agora que você investigou
# de maneira ampla a possível correlção entre as variáveis, que neste caso, demonstraram haver correlação negativa.