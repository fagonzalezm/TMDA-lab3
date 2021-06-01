require('maxent')
require('tm')
require('SnowballC')
require('wordcloud')

IMDB <- read.table("./IMDB", header=TRUE, sep=",")
head(IMDB)

############ aproximar ratings##################
IMDB$Rating <- round(IMDB$Rating,1)
attach(IMDB)

############# separar conjunto de entrenamiento y test ######################
smp_size <- floor(0.80 * nrow(IMDB))

set.seed(123)
train_ind <- sample(seq_len(nrow(IMDB)), size = smp_size)

train <- IMDB[train_ind, ]
test <- IMDB[-train_ind, ]

################## preprocesamiento ##################################

pre_procesamiento <- function(texto) {
  texto = tm_map(texto,content_transformer(removePunctuation))
  
  texto = tm_map(texto,content_transformer(tolower))
  
  texto = tm_map(texto,content_transformer(removeWords),stopwords("english"))
  
  texto = tm_map(texto,stemDocument)
  
  texto = tm_map(texto,stripWhitespace)
  
  texto = tm_map(texto,content_transformer(removeNumbers))
  
  return(texto)
}


corpusTrain = Corpus(VectorSource(train$Text))
corpusTest = Corpus(VectorSource(test$Text))


corpusTrain = pre_procesamiento(corpusTrain)
matrixTrain = DocumentTermMatrix(corpusTrain)
sparseTrain <- as.compressed.matrix(matrixTrain)

f <- tune.maxent(sparseTrain, train$Rating, nfold = 3, showall = TRUE, verbose = TRUE)
print(f)

#mejor:
#11 l1 = 0.0, l2 = 0.8, usesgd = 0, heldout = 0

modelo <- maxent(sparseTrain, train$Rating, l1_regularizer = 0.0, l2_regularizer = 0.8, use_sgd = FALSE, set_heldout = 0, verbose = TRUE)

corpusTest = pre_procesamiento(corpusTest)
matrixTest = DocumentTermMatrix(corpusTest)
sparseTest <- as.compressed.matrix(matrixTest)

resultado <- predict(modelo, sparseTest)

recuperadoRelevante = 0
recuperadoNoRelevante = 0
noRecuperadoRelevante = 0
noRecuperadoNoRelevante = 0

for(i in 1:nrow(resultado)){
  row = resultado[i, ]
  label = as.numeric(row[1])
  col <- which(colnames(resultado) == as.character(label))
  
  #caso relevante
  if(label >= 0.7){
    #Recuperado
    if(as.numeric(row[col]) >= 0.7){
      recuperadoRelevante = recuperadoRelevante + 1
    }
    #No recuperado
    else{
      noRecuperadoRelevante = noRecuperadoRelevante + 1
    }
  }
  #caso no relevante
  else{
    #recuperado
    if(as.numeric(row[col]) >= 0.7){
      recuperadoNoRelevante = recuperadoNoRelevante + 1
    }
    #no recuperado
    else{
      noRecuperadoNoRelevante = noRecuperadoNoRelevante + 1
    }
  }
}


