tweets <- Exercício_2_Tweets_Mg[,c("Text", "Classificacao")]

#####################################
#Substituindo o a base de dados do código da aula passada:

# Parte 1 -  Criar um corpus (conjunto de documentos de texto)
tweet_corpus <- VCorpus(VectorSource(tweets$Text))

#################################################################
# Parte 2 -  Limpando o conteúdos das frases
# 1- Transformar o texto para minúsculo
tweet_limpo <- tm_map(tweet_corpus, content_transformer(tolower))

as.character(tweet_corpus[[1]])
as.character(tweet_limpo[[1]])

# 2 - Removendo os números
tweet_limpo <- tm_map(tweet_limpo, removeNumbers)

# 3 - Removendo "palavras vazias" = stopwords 

tweet_limpo <- tm_map(tweet_limpo, removeWords, stopwords())

# 4 - Removendo a pontuação

tweet_limpo <- tm_map(tweet_limpo, removePunctuation)

# 5 - Retirando os Backspaces

tweet_limpo <- tm_map(tweet_limpo, stripWhitespace)

# 6 - Mantendo apenas as palavras radicais

tweet_limpo <- tm_map(tweet_limpo, stemDocument)

#################################################################
# Parte 3 -  tokenização do conteúdo

# 1- criando a matriz de termos do documento (DTM)
tweet_dtm <- DocumentTermMatrix(tweet_limpo)

# 2- Separando os dados em treino e teste (80-20)

sorteio <- sample(1:8199, 6559)

tweet_dtm_treino <- tweet_dtm[sorteio,] 

tweet_dtm_teste <- tweet_dtm[-sorteio,]

# 3 - Determinando os rótulos

tweet_dtm_rotulos_treino <- tweets[sorteio,]$Classificacao 

tweet_dtm_rotulos_teste <- tweets[-sorteio,]$Classificacao


# Visualizando em nuvem de palavras

wordcloud(tweet_limpo, min.freq = 50, random.order = FALSE)

Negativo <- subset(tweets, Classificacao = "Negativo")
wordcloud(Negativo$Text, min.freq = 40, ramdom.order = FALSE, scale = c(3,0.5))

Neutro <- subset(tweets, Classificacao = "Neutro")
wordcloud(Neutro$Text, min.freq = 40, ramdom.order = FALSE, scale = c(3,0.5))

Positivo <- subset(tweets, Classificacao = "Positivo")
wordcloud(Positivo$Text, min.freq = 40, ramdom.order = FALSE, scale = c(3,0.5))

# Deixando apenas palavras com frequencia >= 5

tweet_palavras_freq_treino <- findFreqTerms(tweet_dtm_treino, 5)
tweet_palavras_freq_teste <- findFreqTerms(tweet_dtm_teste, 5)

tweet_dtm_freq_treino <- tweet_dtm_treino[, tweet_palavras_freq_treino]
tweet_dtm_freq_teste <- tweet_dtm_teste[, tweet_palavras_freq_teste]

# Contando as palavras em cada mensagem

converter_contagens <- function(x){
  x <- ifelse(x > 0, "sim", "Não")
}

tweet_treino <- apply(tweet_dtm_freq_treino, MARGIN = 2, converter_contagens)
tweet_teste <- apply(tweet_dtm_freq_teste, MARGIN = 2, converter_contagens)

#################################################################
# Parte 4 -  Classificador

# Treino
tweet_classificador <- naiveBayes(tweet_treino, tweet_dtm_rotulos_treino)

# Treino
tweet_teste_pred <- predict(tweet_classificador, tweet_teste)

#Matriz
confusionMatrix(tweet_teste_pred, factor(tweet_dtm_rotulos_teste))