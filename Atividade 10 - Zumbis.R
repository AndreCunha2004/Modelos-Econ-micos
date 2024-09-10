### Atividade 10 - Base de dados Zombies ###
  
  # Autor: André Cunha
#Orientador: Guilherme GUilhermino Neto

# Bibliotecas:
library(readr)
library(kmed) 
library(dplyr) 
library(sjPlot) 
library(ggplot2)
library(caret)

# Carregue a base de dados
zumbis <- read_csv("zumbis.csv")
View(zumbis)

#########################
# 1º passo: Renomeie as variáveis do banco de dados
#########################
zumbis$zumbi <- factor(zumbis$zumbi,
                       levels = c(0,1),
                       labels = c("Humano","Zumbi"))

zumbis$sexo <- factor(zumbis$sexo, 
                      levels = c(0,1), 
                      labels = c("Maculino","Feminino"))

zumbis$comida <- factor(zumbis$comida, 
                        levels = c(0,1), 
                        labels = c("Não tem comida","Tem comida"))

zumbis$medicamentos <- factor(zumbis$medicamentos, 
                              levels = c(0,1), 
                              labels = c("Nao tem medicamentos","Tem medicamentos"))

zumbis$ferramentas <- factor(zumbis$ferramentas, 
                             levels = c(0,1), 
                             labels = c("Nao tem ferramentas","Tem ferramentas"))

zumbis$sanitizacao <- factor(zumbis$sanitizacao, 
                             levels = c(0,1), 
                             labels = c("Nao tem sanitizacao","Tem sanitizacao"))

zumbis$roupas <- factor(zumbis$roupas, 
                        levels = c(0,1), 
                        labels = c("Nao tem roupas","Tem roupas"))

zumbis$documentos <- factor(zumbis$documentos, 
                            levels = c(0,1), 
                            labels = c("Nao tem documentos","Tem documentos"))

#########################
# 2º passo: Crie uma variável de galões de água por morador
#########################

zumbis["Galões por morador"] <- zumbis$agua/zumbis$moradores

#########################
# 3º passo: Compare por meio de medidas descritivas as pessoas que sobreviveram e se tornaram
#########################

# zumbi em relação á quantidade de água
pessoas_mortas <- zumbis %>% filter(zumbi == "Zumbi")  
pessoas_vivas <- zumbis %>% filter(zumbi == "Humano") 

# Mean
mean(pessoas_mortas$`Galões por morador`) 
mean(pessoas_vivas$`Galões por morador`) 

# Median 
median(pessoas_mortas$`Galões por morador`) 
median(pessoas_vivas$`Galões por morador`) 

# Minimal 
min(pessoas_mortas$`Galões por morador`) 
min(pessoas_vivas$`Galões por morador`) 

# Max 
max(pessoas_mortas$`Galões por morador`) 
max(pessoas_vivas$`Galões por morador`) 

# Desv-pad
sd(pessoas_mortas$`Galões por morador`) 
sd(pessoas_vivas$`Galões por morador`) 

# gráfico de box-plot (identificando outliers)
boxplot(pessoas_mortas$`Galões por morador`, pessoas_vivas$`Galões por morador`) 

#########################
# 4º passo: faça a mesma análise para a idade das pessoas na amostra
#########################

# Mean
mean(pessoas_mortas$idade) 
mean(pessoas_vivas$idade) 

# Median 
median(pessoas_mortas$idade) 
median(pessoas_vivas$idade) 

# Minimal 
min(pessoas_mortas$idade) 
min(pessoas_vivas$idade) 

# Max 
max(pessoas_mortas$idade) 
max(pessoas_vivas$idade) 

# Desv-pad
sd(pessoas_mortas$idade) 
sd(pessoas_vivas$idade) 

# gráfico de box-plot (identificando outliers)
boxplot(pessoas_mortas$idade, pessoas_vivas$idade) 

#########################
# 5º passo: faça tabelas de dupla entrada e verifique as correlações mais relevantes.
######################### 
 # Zumbi e Sexo 
table(zumbis$zumbi,zumbis$sexo) #tabela de dupla entrada 

ggplot(zumbis) +
  aes(x = zumbi, fill = sexo) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() #grafico de barras

  # Zumbi e area 
table(zumbis$zumbi,zumbis$area_residencia)  #tabela de dupla entrada 

ggplot(zumbis) +
  aes(x = zumbi, fill = area_residencia) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() #grafico de barras

  # Zumbi e comida
table(zumbis$zumbi,zumbis$comida) 

ggplot(zumbis) +
  aes(x = zumbi, fill = comida) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

  # Zumbi e medicamentos
table(zumbis$zumbi, zumbis$medicamentos)

ggplot(zumbis) +
  aes(x = zumbi, fill = medicamentos) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

  # Zumbi e ferramentas
table(zumbis$zumbi, zumbis$ferramentas)

ggplot(zumbis) +
  aes(x = zumbi, fill = ferramentas) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

  # Zumbi e sanitização
table(zumbis$zumbi, zumbis$sanitizacao)

ggplot(zumbis) +
  aes(x = zumbi, fill = sanitizacao) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

  # Zumbi e roupas
table(zumbis$zumbi, zumbis$roupas)

ggplot(zumbis) +
  aes(x = zumbi, fill = roupas) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

  # Zumbi e documentos
table(zumbis$zumbi, zumbis$documentos)

ggplot(zumbis) +
  aes(x = zumbi, fill = documentos) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#########################
# 6º passo: Faça testes qui-quadrados para checar a independência entre a 
# variável “zumbi” e as variáveis qualitativas. 
#########################
chisq.test(table(zumbis$zumbi, zumbis$sexo))
chisq.test(table(zumbis$zumbi, zumbis$area_residencia)) 
chisq.test(table(zumbis$zumbi, zumbis$comida)) 
chisq.test(table(zumbis$zumbi, zumbis$medicamentos)) 
chisq.test(table(zumbis$zumbi, zumbis$ferramentas)) 
chisq.test(table(zumbis$zumbi, zumbis$sanitizacao)) 
chisq.test(table(zumbis$zumbi, zumbis$roupas)) 
chisq.test(table(zumbis$zumbi, zumbis$documentos)) 

#########################
# 7º passo: avalie se a localidade de residência influencia a sobrevivencia.
#########################
 ##Resposta:
    
    ## A fim de contextualizar, ao realizar o teste qui-quadrado, nota-se que, se o p-valor for maior
    # que 0.05, Há evidências para rejeitas a hipóteses, ela será aceita para o contrário.
    # Logo, é possível aceitar a hipótese de correlação com as variáveis: "area-residencial",
    # "comida", "medicamentos" e "Sanitização".

#########################
# 8º passo: Faça testes e ajuste um modelo de regressão logística
#########################
indices <- sample(x = 1:200, 180, replace = FALSE)

treino <- zumbis[indices,]
teste <- zumbis[-indices,]

modelo <- glm(zumbi ~ area_residencia + comida + medicamentos+ sanitizacao, family = 'binomial', data = treino)

summary(modelo)
exp(coef(modelo))


#########################
# 9º passo:s utilize uma matriz de confusão e a acurácia para analisar os resultados
#########################
predicoes <- predict(modelo, newdata = teste, type = "response")
predicoes <- ifelse(predicoes > 0.5, "Zumbi", "Humano")
confusionMatrix(factor(predicoes), factor(teste$zumbi))

#########################
# 10º passo: Verifique se você sobreviveria (o autor no caso) 
#########################
status <- predict(modelo, newdata = data.frame(area_residencia = "Urbana", 
                                               comida = "Tem comida", 
                                               medicamentos = "Tem medicamentos", 
                                               sanitizacao = "Tem sanitizacao"), 
                                               type = "response")

status_final <- ifelse(status > 0.5, "Zumbi", "Humano")   

print(status_final)
#########################
###    FIM   ###
#########################
