install.packages("wooldridge")
install.packages("dplyr")
install.packages("esquisse")
library(dplyr)
library(wooldridge)
library(esquisse)

##############
# Exercício 1:
##############
#Base de Dados
dados_c1 <- wage1

#Dicionário de Dados 
?wage1

# i) Nível de Escolaridade
  # Mean
mean(dados_c1$educ)

  # Min
min(dados_c1$educ)
  # Max
max(dados_c1$educ)

# ii) Salário médio 
mean(dados_c1$wage)
  
  #Trazendo para diário
  mean(dados_c1$wage)*8*24
  
# v) Análise de variabilidade da amostra
table(dados_c1$female)

##############
# Exercício 2:
##############
#Base de Dados
dados_c2 <- bwght

#Dicionário de Dados 
?bwght

# i) numero de mulheres na amostra e quantas fumaram durante uma gestação
table(dados_c2$cigs)
  
Awnser <- 1388-1176
  
# ii) numero medio de cigarros consumidos por dia
mean(dados_c2$cigs)
  
# Rerserva as mulheres as quais consomem cigarro
Dados_F <- dados_c2 %>% filter(cigs > 0)
  
mean(Dados_F$cigs)
  
# v) Renda familiar em dolares

#mean
mean(dados_c2$faminc)

#SD
sd(dados_c2$faminc)

##############
# Exercício 5:
##############
#Base de Dados
dados_c5 <- fertil2

#Dicionário de Dados 
?fertil2

# i) Variavel children

# Mean
mean(dados_c5$children)

# Min
min(dados_c5$children)

# Max
max(dados_c5$children)

# ii) Enegia eletrica em casa
table(dados_c5$electric)/nrow(dados_c5)

# iii) Enegia eletrica em casa e filhos por mulher
  # media de filhos por casa sem energia
  dados_0 <- dados_c5 %>% filter(electric == 0)

  mean(dados_0$children)
  
  # media de filhos por casa com energia
  dados_1 <- dados_c5 %>% filter(electric == 1)
  
  mean(dados_1$children)
  
##############
# Exercício 6:
##############
#Base de Dados
dados_c6 <- countymurders
  
#Dicionário de Dados 
?countymurders
  
#Dados apenas de 1996
novo_c6 <- dados_c6 %>% filter(year == 1996)
  
# i) Quantos condados existem
novo_c6 %>% distinct(countyid) %>% nrow()  

# Quantos com 0 assasinatos  
novo_c6 %>% filter(murders == 0) %>% nrow()
  
# Qual a porcetagem do total 
novo_c6 %>% filter(murders == 0) %>% nrow()/ novo_c6 %>% distinct(countyid) %>% nrow()

# ii) Número máximo de assassinatos
max(novo_c6$murders)

# Número máximo de pessoas sentenciadas à morte
max(novo_c6$execs)

# Número médio de execuções
mean(novo_c6$execs)
  
  # Raciocínio: veirificar quantos condados tem execs maior que zero
  novo_c6 %>% filter(execs > 0) %>% nrow()
 
  # Raciocínio: veirificar quantos condados tem execs igual a zero
  novo_c6 %>% filter(execs == 0) %>% nrow()
  
#iii) Índice de correlação entre as variáveis "murders" e "execs":
cor(dados_c6$murders, dados_c6$execs)

##############
# Exercício 7:
##############
#Base de Dados
dados_c7 <- alcohol

#Dicionário de Dados 
?alcohol

# i) Porcentagem de homens que relataram o abuso de alcool
table(dados_c7$abuse)/nrow(dados_c7)

# taxa de empregabilidade geral da amostra
mean(dados_c7$employ) #desempregado apenas

table(dados_c7$employ)/nrow(dados_c7) # tabela com desempregados e empregados

# ii) Taxa de empregabilidade entre os homens que relatam abuso de alcool
male_abuse <- dados_c7 %>% filter(abuse == 1) 

mean(male_abuse$employ) #desempregado apenas
  
table(dados_c7$abuse, dados_c7$employ)/nrow(dados_c7) # tabela com desempregados e empregados

# iii) Taxa de empregabilidade entre os homens que nao relatam abuso de alcool
male_not_abuse <- dados_c7 %>% filter(abuse == 0)

mean(male_not_abuse$employ)

# iv) correlação entre abuso de alcool e empregabilidade
cor(dados_c7$employ, dados_c7$abuse)
