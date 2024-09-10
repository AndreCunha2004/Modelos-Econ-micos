## Modelos Econômicos e Quantitativos – Avaliação 1

    # Autor: André Cunha e Vitória Mattedi

#############
#Questão 1: Base de dados leite
#############

# a) Prepare os dados:

  # a.1) Carregue a planilha
  library(readr)
  leite <- read_csv("C:/Users/andre/Downloads/leite.csv")
  View(leite)
  
  # a.2) Usando o comando glimpse do pacote dplyr, responda: quantas variáveis o R compreende como
  #qualitativas e quantas compreende como quantitativas?
  library(dplyr)
  glimpse(leite)

    # O R compreende apenas 1 variável como qualitativa, sendo "Avaliação", e as restantes 
    # como quantitavas, incluindo aquelas que são binárias.
  
  # a.3) verifique se é necessário converter uma ou mais variáveis para o formato factor.
  leite$gosto <- factor(leite$gosto,
                        levels = c(0,1),
                        labels = c("ruim", "normal"))
  
  leite$odor <- factor(leite$odor,
                       levels = c(0,1),
                       labels = c("ruim", "normal")) 
  
  leite$gordura <- factor(leite$gordura,
                       levels = c(0,1),
                       labels = c("baixo", "alto"))
  
  leite$turbidez <- factor(leite$turbidez,
                       levels = c(0,1),
                       labels = c("baixa", "alta"))
  
# b) Análise exploratória e inferência – variáveis qualitativas 
library(ggplot2)  

  #b.1) Para as variáveis qualitativas, construa tabelas de dupla entrada
  # e gráficos de barras para verificar a associação. 
  
  # Avaliação x Gosto
  table(leite$avaliacao,leite$gosto) #tabela de dupla entrada 
  
  ggplot(leite) +
    aes(x = avaliacao, fill = gosto) +
    geom_bar() +
    scale_fill_hue(direction = 1) +
    theme_minimal() #grafico de barras
  
  # Avaliação x Odor
  table(leite$avaliacao,leite$odor) #tabela de dupla entrada 
  
  ggplot(leite) +
    aes(x = avaliacao, fill = odor) +
    geom_bar() +
    scale_fill_hue(direction = 1) +
    theme_minimal() #grafico de barras

  # Avaliação x Gordura
  table(leite$avaliacao,leite$gordura) #tabela de dupla entrada 
  
  ggplot(leite) +
    aes(x = avaliacao, fill = gordura) +
    geom_bar() +
    scale_fill_hue(direction = 1) +
    theme_minimal() #grafico de barras
  
  # Avaliação x Turbidez
  table(leite$avaliacao,leite$turbidez) #tabela de dupla entrada 
  
  ggplot(leite) +
    aes(x = avaliacao, fill = turbidez) +
    geom_bar() +
    scale_fill_hue(direction = 1) +
    theme_minimal() #grafico de barras
  
  # b.2) Faça testes qui-quadrado para verificar a independência entre as variáveis.
  chisq.test(table(leite$avaliacao, leite$gosto))
  chisq.test(table(leite$avaliacao, leite$odor))
  chisq.test(table(leite$avaliacao, leite$gordura))
  chisq.test(table(leite$avaliacao, leite$turbidez))
  
  # b.3) qual ou quais variáveis qualitativas você diria que mais influenciam a qualidade do leite?
  
    # A turbidez é a variável que mais influencia a avaliação do leite, seguida pela
    # gordura, que influencia menos. Já as outras, apesar de haver a não hipótese de independencia,
    # Não demonstraram influencia forte de acordo com os gráficos e tabelas.
  
# c) Análise exploratória e inferência – variáveis quantitativas 
  library(corrplot)
  
  leite$avaliacao <- factor(leite$avaliacao, levels = c("aceitável", "ruim"), 
                            labels = c(1, 0))
  
  dados_novos <- leite %>% select(pH, temperatura, color)
  
  M <- cor(dados_novos)
  corrplot(M, method = "number")
  
  # c.1) Para as variáveis quantitativas, calcule média, mediana e desvio padrão, 
    # em separado para o grupo dos leites ruins e dos aceitáveis; 
  Avaliacao_ruim <- leite %>% filter(avaliacao == "ruim")  
  Avaliacao_aceitavel <- leite %>% filter(avaliacao == "aceitável")  
  
  # Avaliação ruim
  
  # Média
  mean(Avaliacao_ruim$pH)
  mean(Avaliacao_ruim$temperatura)
  mean(Avaliacao_ruim$color)
  
    # Mediana
  median(Avaliacao_ruim$pH)
  median(Avaliacao_ruim$temperatura)
  median(Avaliacao_ruim$color)
  
  # Desv-pad
  sd(Avaliacao_ruim$pH)  
  sd(Avaliacao_ruim$temperatura)
  sd(Avaliacao_ruim$color)

  # Avaliação aceitável
  
  # Média
  mean(Avaliacao_aceitavel$pH)
  mean(Avaliacao_aceitavel$temperatura)
  mean(Avaliacao_aceitavel$color)
  
  # Mediana
  median(Avaliacao_aceitavel$pH)
  median(Avaliacao_aceitavel$temperatura)
  median(Avaliacao_aceitavel$color)
  
  # Desv-pad
  sd(Avaliacao_aceitavel$pH)  
  sd(Avaliacao_aceitavel$temperatura)
  sd(Avaliacao_aceitavel$color)
  
  # c.2) Faça boxplots para comparar os valores das variáveis qualitativas nos dois grupos;
  
  # Avaliação ruim 
  boxplot(Avaliacao_ruim$pH,
          main = "Boxplot do pH - Avaliação Ruim",
          xlab = "Amostras",
          ylab = "Valores de pH",
          col = "lightcoral")
  
  boxplot(Avaliacao_ruim$temperatura,
          main = "Boxplot da Temperatura - Avaliação Ruim",
          xlab = "Amostras",
          ylab = "Valores de temperatura",
          col = "lightcoral")
  
  boxplot(Avaliacao_ruim$color,
          main = "Boxplot da cor - Avaliação Ruim",
          xlab = "Amostras",
          ylab = "Valores de cor",
          col = "lightcoral")
  
  # Avaliação aceitável
  boxplot(Avaliacao_aceitavel$pH,
          main = "Boxplot do pH - Avaliação Aceitável",
          xlab = "Amostras",
          ylab = "Valores de pH",
          col = "lightcoral")
  
  boxplot(Avaliacao_aceitavel$temperatura,
          main = "Boxplot da Temp - Avaliação Aceitável",
          xlab = "Amostras",
          ylab = "Valores de temperatura",
          col = "lightcoral")
  
    boxplot(Avaliacao_aceitavel$color,
            main = "Boxplot da cor - Avaliação Aceitável",
            xlab = "Amostras",
            ylab = "Valores de cor",
            col = "lightcoral")
    
    #c.3)Faça testes t para diferença de médias nos dois grupos 
    #e verifique se é possível afirmar que a diferença nas médias seja estatisticamente significativa; 
    
    #Avaliação aceitavel
    shapiro.test(Avaliacao_aceitavel$color)
    shapiro.test(Avaliacao_aceitavel$temperatura)
    shapiro.test(Avaliacao_aceitavel$pH)
    
    #Avaliação ruim
    shapiro.test(Avaliacao_ruim$color)
    shapiro.test(Avaliacao_ruim$temperatura)
    shapiro.test(Avaliacao_ruim$pH)
    
# d) Ajuste do modelo de regressão 
 
    leite$gosto <- factor(leite$gosto, levels = c("ruim", "normal"), 
                              labels = c(0, 1))
    leite$odor <- factor(leite$odor, levels = c("ruim", "normal"), 
                              labels = c(0, 1))
    leite$gordura <- factor(leite$gordura, levels = c("baixo", "alto"), 
                              labels = c(0, 1))
    leite$turbidez <- factor(leite$turbidez, levels = c("baixa", "alta"), 
                              labels = c(0, 1))
    
  #d.1) Separe os dados em uma amostra de treino (80%) e um amostra de teste (20%);
    
  indices <- sample(1:1059, 847, replace = FALSE) 
    
  #d.2) Na amostra de treino, ajuste um modelo de regressão logística; 
  treino <- leite[indices,]
  teste <- leite[-indices,]
  
  #d.3) Use o comando summary para analisar o modelo resultante. 
  # Há variáveis que parecem não ser estatisticamente significativas?
  # Se sim, diga quais, remova-as e gere um novo modelo. 
  modelo <- glm(formula = avaliacao ~., family = "binomial", data = treino)
  summary(modelo)
    
  #d.4)Use os comandos coef e exp, diga quais foram os coeficientes do modelo e faça a interpretação. 
  modelo_1 <- glm(formula = avaliacao ~ temperatura + gosto + turbidez + color, family = "binomial",
                  data = treino)
  
  summary(modelo_1)
  
  exp(coef(modelo_1))
  
  #e.1)Faça predições para a qualidade do leite com base nas variáveis 
  #independentes com a base de dados teste.

  predicoes <- predict(modelo, newdata = teste, type = "response")  
  
  # e.2) Faça uma matriz de confusão e diga qual a acurácia do seu modelo. 
  library(caret)
  
  # Ajustando as predições para uma classe com um ponto de corte de 0,5
  predicoes_class <- ifelse(predicoes > 0.5, 1, 0)
  
  confusionMatrix(factor(predicoes_class), factor(teste$avaliacao,
                                                  levels = c(0, 1), labels = c(0, 1)))
  