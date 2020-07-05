#Reading input Dataset

dataset = read.csv('Churn_Modelling.csv')
X_set = dataset[4:14]

#Factorising categorical data
X_set$Geography = as.numeric(factor(X_set$Geography))
X_set$Gender = as.numeric(factor(X_set$Gender))

#Splitting into Training and Test set
library(caTools)
split = sample.split(X_set$Exited, SplitRatio = 0.8)
Training_set = subset(X_set, split == TRUE)
Test_set = subset(X_set, split == FALSE)

#Scaling data
Training_set[-11] = scale(Training_set[-11])
Test_set[-11] = scale(Test_set[-11])

#Using h2o library for deep learning
#install.packages('h2o')
library('h2o')

#Initialising h20 server instance
h2o.init()

#Training the neural network
classifier = h2o.deeplearning(y = 'Exited',
                 training_frame = as.h2o(Training_set),
                 activation = 'Rectifier',
                 hidden = c(6,6),
                 epochs = 100)

#Making predictions
prob_pred = h2o.predict(classifier, as.h2o(Test_set[-11]))
y_pred = as.vector(ifelse(prob_pred > 0.5, 1,0))

#Shutting down h20 server instance
h2o.shutdown(prompt = FALSE)

#Calculating Confusion matrix and Accuracy
cm = table(Test_set[,11], y_pred)
accuracy = ((cm[1:1] + cm[2:2])/2000)*100
