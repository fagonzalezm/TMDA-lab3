require('maxent')
require('tm')
require('SnowballC')
require('wordcloud')

IMDB <- read.table("./IMDB", header=TRUE, sep=",")
head(IMDB)
attach(IMDB)

################## preprocesamiento

pre_procesamiento <- function(texto) {
  texto = tm_map(texto,content_transformer(removePunctuation))
  
  texto = tm_map(texto,content_transformer(tolower))
  
  texto = tm_map(texto,content_transformer(removeWords),stopwords("english"))
  
  texto = tm_map(texto,stemDocument)
  
  texto = tm_map(texto,stripWhitespace)
  
  texto = tm_map(texto,content_transformer(removeNumbers))
  
  return(texto)
}


corpus = Corpus(VectorSource(Text))

summary(corpus)


corpus = pre_procesamiento(corpus)
matrix = DocumentTermMatrix(corpus)
sparse <- as.compressed.matrix(matrix)

f <- tune.maxent(sparse[1:500], Rating[1:500], nfold = 3, showall = TRUE, verbose = TRUE)
print(f)



modelo <- maxent(sparse[1:500], Rating[1:500], l1_regularizer = 0.0, l2_regularizer = 0.2, use_sgd = FALSE, set_heldout = 0, verbose = TRUE)


resultado <- predict(modelo, sparse[501:1000])
