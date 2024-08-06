# Importação dos dados:
  # Instalação do pacote p/ leitura da base de dados
  install.packages("readr")
  # Acionamento do pacote instalado
  library(readr)

  # Efitivamente faz a leitura da base de dados
  Dados <- read.csv("C:/Users/20231enpro0097/Downloads/olimpiadas.csv")
      # O comando read.csv dá o comando p/ o script
      # É necessário alterar a "\" por "/".
      # Para remover uma base de dados, utilize: rm(Dados)
  
#######################
#Tabelas de Frequencia 
#######################
  
 #Tabelas Simples: 
  
 # Valores Absolutos
  
  table(Dados$Sex)  
  
 # Valores Relativos (%)
  
  table(Dados$Sex)/nrow(Dados)  
  
 # Tabelas de Dupla Entrada:
  
  table(Dados$Sex,Dados$NOC)
  
 # Dados somente do brasil
  
  install.packages("dplyr")
  library(dplyr)
  
  Dados_Brasil <- Dados %>% filter(NOC == "BRA")
  
  table()
  
##########
#GRÁFICOS  
##########
  install.packages("esquisse")  
  library(esquisse)
 # Gráfico de exemplo: Script feito pelo pacote
  
 ggplot(Dados) +
 aes(x = Sex) +
 geom_bar(fill = "#1F4FA8") +
 labs(x = "Sexo", y = "Quantidade") +
 theme_classic()

 # Estatísticas Descritivas para a idade 

 # Idade mínima da amostra
 min(Dados$Age, na.rm = TRUE)

 # Idade máxima da amostra
 max(Dados$Age, na.rm = TRUE)

 # Média 
 mean(Dados$Age, na.rm = TRUE)

 # Desvio-Padrão
 sd(Dados$Age, na.rm = TRUE)
  
 # Gráfico
 
  ggplot(Dados) +
 aes(x = Weight, fill = Sex) +
 geom_histogram(bins = 30L) +
 scale_fill_viridis_d(option = "cividis", 
 direction = 1) +
 theme_classic()

  # Analisando os dados da amostra por sexo
  
 Dados_Femininos <- Dados %>% filter(Sex == "F")
 
 Dados_Masculinos <- Dados %>% filter(Sex == "M")
 
 # Média dos dados separados
 
 mean(Dados_Femininos$Weight, na.rm = TRUE)
 
 mean(Dados_Masculinos$Weight, na.rm = TRUE)
 
 # Média dos dados juntos
 
 Dados %>% group_by(Sex) %>% summarise(media = mean(Dados$Weight, na.rm = TRUE))
 
 
 