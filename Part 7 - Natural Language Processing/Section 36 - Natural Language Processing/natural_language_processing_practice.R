library(tm)
library(SnowballC)
library(randomForest)
library(caTools)
dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)

corpus = VCorpus(VectorSource(dataset_original$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))

dataset$Liked = dataset_original$Liked
dataset$Liked = factor(dataset$Liked)

split = sample.split(dataset$Liked,  SplitRatio = 0.8)

Training_set = subset(dataset, split == TRUE)
Test_set = subset(dataset, split == FALSE)

Classifier = randomForest(x = Training_set[-692],
                          y = Training_set$Liked,
                          ntree = 10
                          )
predictions = predict(Classifier, newdata = Test_set[-692])

cm = table(Test_set$Liked, predictions)