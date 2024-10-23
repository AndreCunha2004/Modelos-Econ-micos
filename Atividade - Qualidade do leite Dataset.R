######################################################################################
# Atividade de Machine Learning - Modelo supervisionado - Qualidade do leite Dataset
    ### Autor: André Cunha e Rayane Cardoso
    ### Orientador: Prof. Guilherme Guilhermino Neto 
    ### Engenharia de Produção - Modelos Econômicos e Quantitativos
######################################################################################

##############
# bibliotecas
##############
library(readr)
library(dplyr)



############################################
# 1) Importe o conjunto de dados para o R
############################################

Dados_Qualidade_do_leite <- read_csv("C:/Users/andre/Downloads/Exercício 1 - Qualidade do leite.csv")
View(Dados_Qualidade_do_leite)

###########################################################################
# 2) Quantos exemplos tem o conjunto de dados? E quantos são os atributos?
###########################################################################
 
   # Cada linha da planilha é um exemplo, ou seja,
      # um valor dado de desmontração para o programa avaliar os futuros valores.
  Dados_Qualidade_do_leite %>% nrow()

  # já os atributos são cada coluna que caracterizam o exemplo, exceto a coluna que possui um rótulo.

###########################################################################
# 3) Quantos rótulos são possíveis para o atributo-alvo?
###########################################################################
  
  # Os rótulos são a coluna "Grade", uma vez que caracterizam o exemplo de alguma forma

  # Os possíveis rótulos são: "Low", "Medium" ou "High".

###########################################################################
# 4) No dataset, quantas amostras existem para cada rótulo possível? 
# Você diria que o dataset é desbalanceado?
###########################################################################

  # Quantidade de amostras (ou exemplos) por rótulo:
  table(Dados_Qualidade_do_leite$Grade)

  # A base de dados não está perfeitamente balanceada, porém não há uma diferença 
  # significativa entre os rótulos (como 90% para uma classe e 10% para outra, por exemplo).

###########################################################################
# 5) Codifique a variável de saída como fator.
###########################################################################

  # Codificando a variável de saída como fator
  Dados_Qualidade_do_leite$Grade <- factor(Dados_Qualidade_do_leite$Grade)

###########################################################################
# 6) Faça a normalização dos dados.
###########################################################################

  # Criando uma função que normaliza
  normaliza <- function(x) {
    return((x - min(x))/(max(x) - min(x)))
  }

  # Normalizando os dados
  Dados_norm <- as.data.frame(lapply(Dados_Qualidade_do_leite[1:7], normaliza))

  ###########################################################################
  # 7) Separe os dados em uma amostra de treino e outra de teste 
  # (utilize aproximadamente 80% e 20% dos dados).
  ###########################################################################
  # Amostra com 80%  
  Dados_treino <-Dados_norm[1:847,]
  
  # Amostra com 20%
  Dados_teste <- Dados_norm[848:1059,]

  ### Separando rótulos ###
  Dados_treino_rotulos <- Dados_Qualidade_do_leite[1:847,]$Grade
  Dados_teste_rotulos <- Dados_Qualidade_do_leite[848:1059,]$Grade
  
###########################################################################
# 8) Com os dados separados:
###########################################################################
  
######################
# Aplicação do K-NN #
#################### 

### Bibliotecas
library(class)
library(caret)
library(ggplot2)
library(lattice)


### a) Treine um algoritmo k-NN (na amostra de treino). 
# Utilize um laço de repetição para testar várias quantidades de vizinhos mais próximos
# e guarde as acurácias para cada k em um vetor.

acuracia <- c()
  
for(k in 1:30){
    
predicoes <- knn(train = Dados_treino, 
                 test = Dados_teste,
                  cl = Dados_treino_rotulos,
                  k = k)
    
matriz <- confusionMatrix(Dados_teste_rotulos, predicoes)

acuracia <- c(acuracia, matriz[['overall']]['Accuracy'])

}
#### Explicando:

  # O laço de repetição pega os dados de treino e vai treinando com knn, criando um modelo.
    # Depois esse laço pega esses dados de teste e gera várias predicoes para 
    # compararmos com qual K o modelo acertou mais.

#### b) Exiba graficamente o erro em função do valor de k.

# Imprimindo o gráfico da acurácia de cada K
plot(1:30, 1-acuracia, type = 'l') 
  
### c) Qual valor de k trouxe o melhor resultado? k == 27

###########################################################################
# 9) Usando o valor de k descoberto no item anterior,
 # faça classificações na amostra de teste e construa a matriz de confusão para os dados. 
 # Analise o desempenho de seu algoritmo.
###########################################################################

### Testando K = 27 ###
  
predicoes <- knn(train = Dados_treino, 
                 test = Dados_teste,
                 cl = Dados_treino_rotulos,
                 k=27) 

confusionMatrix(Dados_teste_rotulos,predicoes)
 
# O modelo trouxe uma Acurácia == 82.08% com os exemplos do dataset,
  # isso demonstra que esse modelo está razoavelmente preparado para predizer novos dados. 

######################### FIM DO PROGRAMA #########################