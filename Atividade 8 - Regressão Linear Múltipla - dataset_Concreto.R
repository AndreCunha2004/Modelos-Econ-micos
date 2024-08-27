# Atividade 8 - dataset: Concreto

  # Autor: André Cunha

  # Objetivo do estudo: 
    # Investigar quais variáveis mais influenciam a resistência à compressão do concreto. 

# Bibliotecas:
library(readr)
library(corrplot)
library(ggplot2)
library(dplyr)

# Importando a base de dados
concrete_data <- read_csv("C:/Users/andre/OneDrive/GRADUAÇÃO_ENPRO/ENPRO 4/Modelos Econômicos Quantitativos/Atividades - Enviadas/Atividade 8 - Regressão Linear Múltipla - dataset_Concreto/concrete_data.csv")
View(concrete_data)

# 1) Matriz de correlação linear
Matriz <- cor(concrete_data)

  # Imprime a matriz
  corrplot(Matriz, method = "number")

# 2)Faça um ranking com os coeficientes de correlação (em módulo), do maior para o menor,
  # para verificar quais variáveis têm correlação linear mais forte com "strength".
  
  # 1º - cement
  # 2º - superplastic
  # 3º - age
  # 4º - water
  # 5º - fineagg  
  # 6º - coarseagg
  # 7º - slag
  # 8º - ash
  
# 3) Para as três variáveis com correlação mais forte,
  # faça o diagrama de dispersão da variável (eixo x) com "strength" (eixo y).
  
  # Gráfico de dispersão: Cement
  ggplot(data = concrete_data, aes( x = cement, y = strength)) +
    geom_point() +
    theme_classic() +
    xlab("Cement") +
    ylab("Strenght") +
    ggtitle("Gráfico de Dispersão") +
    geom_smooth(method = "lm", se = FALSE)
  
  # Gráfico de dispersão: superplastic
  ggplot(data = concrete_data, aes( x = superplastic, y = strength)) +
    geom_point() +
    theme_classic() +
    xlab("Superplastic") +
    ylab("Strenght") +
    ggtitle("Gráfico de Dispersão") +
    geom_smooth(method = "lm", se = FALSE)
  
  # Gráfico de dispersão: age
  ggplot(data = concrete_data, aes( x = age, y = strength)) +
    geom_point() +
    theme_classic() +
    xlab("Age") +
    ylab("Strenght") +
    ggtitle("Gráfico de Dispersão") +
    geom_smooth(method = "lm", se = FALSE)
  
# 4) Construa um modelo de regressão linear incluindo todas as variáveis.
  # Qual a equação do modelo?
  modelo_completo <- lm(formula = strength ~., data = concrete_data)
  
    # Imprime o modelo:
    modelo_completo %>% summary()
    
    # Modelo-ajustado: 
    
    # Strength = cement*0.121+slag*0.104+ash*0.088-water*0.151+superplastic*0.29
    # +coarseagg*0.018086+fineagg*0.020+age*0.114
    
# 5) Analise os resultados (coeficientes do modelo,
    # R² ajustado, significância dos coeficientes, ferramentas de diagnóstico).
    
      #Resposta: Este  modelo foi capaz de explicar 61.25% da variação da resistência do concreto 
    
    qqnorm(modelo_completo$residuals)
    qqline(modelo_completo$residuals)
    
    shapiro.test(modelo_completo$residuals) # p-value = 0.0029
    
    # há elementos para rejeitar a hipótese de normalidade
    
    hist(modelo_completo$residuals,
         main = "Histograma de resíduos contra x (strength)",
         xlab = "Resíduo",
         ylab = "Frenquencia do resíduo")
    

    
# 6) Agora faça um novo modelo, removendo as variáveis que 
    # não são estatisticamente significativas. 
    
    modelo_reduzido2 <- lm(formula = strength ~ cement+superplastic+age,
                           data = concrete_data)
    
    # Imprime o modelo:
    modelo_reduzido2 %>% summary()
    
    # Refaça as análises feitas no item 5).

    qqnorm(modelo_reduzido2$residuals)
    qqline(modelo_reduzido2$residuals)
    
    shapiro.test(modelo_reduzido2$residuals) # há elementos para rejeitar
    
    hist(modelo_reduzido2$residuals,
         main = "Histograma de resíduos contra x (strength)",
         xlab = "Resíduo",
         ylab = "Frenquencia do resíduo")
    
  
# 7) De acordo com seu modelo, quais são as variáveis que mais impactam 
    # a resistência à compressão do concreto?
    
    # resposta: No geral, as 3 variáveis que foram escolhidas influenciam
    #significativamente a resistência mecânica do concreto.
    # logo, são elas: cement, superplastic e age.
    
# 8)  Use o modelo para prever "strength" para um material
    # com as seguintes características:

    # predizendo valores  
novo_dado <- data.frame( slag = 74, ash = 54, cement = 280, age = 45, 
                         water = 182, superplastic = 6, coarseagg = 972, fineagg = 773)

predict(modelo_reduzido2, novo_dado)

# Resposta: 35.44 (unidade de medida de strength)
