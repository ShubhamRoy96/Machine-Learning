library(tm)
library(SnowballC)
library(randomForest)
library(caTools)
library(rpart)
library(class)
library(e1071)
Metrics = matrix(ncol = 5)


#Calculate Metrics
CalcPerformance <- function(cm, ClassifierUsed) {
  TN = cm[1, 1]
  FP = cm[1, 2]
  FN = cm[2, 1]
  TP = cm[2, 2]
  
  Accuracy = (TP + TN) / (TP + TN + FP + FN)
  
  Precision = TP / (TP + FP)
  
  Recall = TP / (TP + FN)
  
  F1_Score = 2 * Precision * Recall / (Precision + Recall)
  
  ScoreData = c(Accuracy, Precision, Recall, F1_Score)
  
  
  Metrics = rbind(Metrics, matrix(ScoreData, ncol = 4))
  
}

#Export to File
ExportData <- function(Metricdata) {
  Headers = c("Algorithm","Accuracy", "Precision", "Recall", "F1_Score")
  colnames(Metricdata) <- Headers
  filename = "AlgoMetrics.csv"
  if (file.exists(filename)) {
    file.remove(filename)
  }
  write.csv(Metricdata, file = filename)
}

#Read Data
dataset_original = read.delim('Restaurant_Reviews.tsv',
                              quote = '',
                              stringsAsFactors = FALSE)
#Build Corpus
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

#Random Forest Classification
RandomForestClassifier = randomForest(x = Training_set[-692],
                                      y = Training_set$Liked,
                                      ntree = 10)
RFpredictions = predict(RandomForestClassifier, newdata = Test_set[-692])

RFcm = table(Test_set$Liked, RFpredictions)

#CalcPerformance(RFcm, "Random Forest")
TN = RFcm[1, 1]
FP = RFcm[1, 2]
FN = RFcm[2, 1]
TP = RFcm[2, 2]

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1_Score = 2 * Precision * Recall / (Precision + Recall)

ScoreData = c("Random Forest", Accuracy, Precision, Recall, F1_Score)


Metrics = rbind(Metrics, matrix(ScoreData, nrow = 1))


#Logistic Regression
LRclassifier = glm(formula = Liked ~ .,
                   family = 'binomial',
                   data = Training_set)
LR_prob_pred = predict(LRclassifier, type = 'response', newdata = Test_set)
LR_y_pred = ifelse(LR_prob_pred > 0.5, 1, 0)
LRcm = table(Test_set[, 692], LR_y_pred)

#CalcPerformance(LRcm, "Logistic Regression")

TN = LRcm[1, 1]
FP = LRcm[1, 2]
FN = LRcm[2, 1]
TP = LRcm[2, 2]

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1_Score = 2 * Precision * Recall / (Precision + Recall)

ScoreData = c("Logistic Regression", Accuracy, Precision, Recall, F1_Score)


Metrics = rbind(Metrics, matrix(ScoreData, nrow = 1))


#K-NN
KNN_y_pred = knn(
  train =  Training_set[, -692],
  test =  Test_set[, -692],
  cl = Training_set[, 692],
  k = 5
)
KNNcm = table(Test_set[, 692], KNN_y_pred)

#CalcPerformance(KNNcm, "K - NN")

TN = KNNcm[1, 1]
FP = KNNcm[1, 2]
FN = KNNcm[2, 1]
TP = KNNcm[2, 2]

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1_Score = 2 * Precision * Recall / (Precision + Recall)

ScoreData = c("K - NN", Accuracy, Precision, Recall, F1_Score)


Metrics = rbind(Metrics, matrix(ScoreData, nrow = 1))



#SVM
classifier = svm(
  formula = Liked ~ .,
  data = Training_set,
  type = 'C-classification',
  kernel = 'linear'
)
SVM_y_pred = predict(classifier, newdata = Test_set[, -692])

SVMcm = table(Test_set[, 692], SVM_y_pred)

#CalcPerformance(SVMcm, "SVM")

TN = SVMcm[1, 1]
FP = SVMcm[1, 2]
FN = SVMcm[2, 1]
TP = SVMcm[2, 2]

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1_Score = 2 * Precision * Recall / (Precision + Recall)

ScoreData = c("SVM", Accuracy, Precision, Recall, F1_Score)


Metrics = rbind(Metrics, matrix(ScoreData, nrow = 1))



#Naive Bayes

classifier = naiveBayes(x = Training_set[, -692], y = Training_set$Liked)
NB_y_pred = predict(classifier, newdata = Test_set[, -692])

NBcm = table(Test_set[, 692], NB_y_pred)

#CalcPerformance(NBcm, "Naive Bayes")

TN = NBcm[1, 1]
FP = NBcm[1, 2]
FN = NBcm[2, 1]
TP = NBcm[2, 2]

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1_Score = 2 * Precision * Recall / (Precision + Recall)

ScoreData = c("Naive Bayes", Accuracy, Precision, Recall, F1_Score)


Metrics = rbind(Metrics, matrix(ScoreData, nrow = 1))



#Deceision Tree
classifier = rpart(formula = Liked ~ ., data = Training_set)
DT_y_pred = predict(classifier, newdata = Test_set[, -692], type = 'class')

DTcm = table(Test_set[, 692], DT_y_pred)

#CalcPerformance(DTcm, "Decision Tree")

TN = DTcm[1, 1]
FP = DTcm[1, 2]
FN = DTcm[2, 1]
TP = DTcm[2, 2]

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1_Score = 2 * Precision * Recall / (Precision + Recall)

ScoreData = c("Decision Tree", Accuracy, Precision, Recall, F1_Score)


Metrics = rbind(Metrics, matrix(ScoreData, nrow = 1))



#Exporting Data to CSV
ExportData(Metrics)

