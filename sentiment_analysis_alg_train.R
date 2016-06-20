library(e1071)
library("RTextTools")

full.corpus <- read.csv("D:/DATA SCIENCE DATA/full-corpus.csv",stringsAsFactors = FALSE)

datas<-full.corpus[c("Topic","TweetText","Sentiment")]

sent_lexicon <- read.csv("D:/DATA SCIENCE DATA/sent_lexicon.csv")


get_negSent_count <- function(sentence){
  x<-length(intersect(strsplit(as.character(sentence)," ")[[1]],as.character(sent_lexicon$negative)))
  x<-as.character(x)
  return(x)
}

get_posSent_count <- function(sentence){
  x<-length(intersect(strsplit(as.character(sentence)," ")[[1]],as.character(sent_lexicon$positive)))
  x<-as.character(x)
  return(x)
}

makefeatures <-function(dframe){
  dfr<-c()
  
  for (i in 1:nrow(dframe)){
    
    dfr$Text[i]<-substr(as.character(dframe[i,2]),1,220)
    dfr$pos[i]<-get_posSent_count(dframe[i,2])
    dfr$neg[i]<-get_negSent_count(dframe[i,2])
    dfr$sent<-dframe[,3]
  }
  return (dfr)
}
dft<-makefeatures(datas)
indexes = sample(1:nrow(datas), size=100)
sample
# Split data
test = datas[indexes,]
dim(test)  # 6 11
train = datas[-indexes,]
dim(train) # 26 11

# Configure the training data
#container <- create_container(makefeatures(train), train$Sentiment, trainSize=1:11, virgin=FALSE)

# train a SVM Model
#model <- train_model(container, "SVM", kernel="linear", cost=1)

#datas<-datas[1:1000,]
sampled = sample(1:nrow(datas), size=0.9*nrow(datas))
train <- datas[sampled, ]
test <- datas[-sampled, ]
#da<-datas[1:2000,]


dat<-makefeatures(datas)


trainDocTermMatrix <- create_matrix(train$TweetText, language="english", removeNumbers=TRUE, stemWords=FALSE)
testDocTermMatrix <- create_matrix(test$TweetText, language="english", removeNumbers=TRUE, stemWords=FALSE)

container <- create_container(DocTermMatrix, datas$Sentiment, trainSize=1:150, testSize=151:nrow(datas), virgin=FALSE)
models <- train_models(container, "MAXENT")
results <- classify_models(container, models)

trainmatr <- as.matrix(trainDocTermMatrix)
testmatr <- as.matrix(testDocTermMatrix)

library("maxent")

classif<-maxent(trainmatr[51:nrow(trainmatr),],as.factor(train$Sentiment[101:800]) )
trainPredict<- predict(classif,trainmatr[1:50,])
recall_accuracy(train[1:100,3], trainPredict)

s <- as.character(datas$TweetText[1])





matrix = create_matrix(datas[, 1], language = "english", removeStopwords = FALSE, 
                       removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10, ], as.factor(tweets[1:10, 2]))
predicted = predict(classifier, mat[11:15, ])

