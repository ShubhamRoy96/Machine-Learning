dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]
dataset$Purchased = factor(dataset$Purchased)
library(e1071)
library(caTools)
library(ElemStatLearn)
dataset[, -3] = scale(dataset[, -3])
split = sample.split(dataset, SplitRatio = 0.75)
training_set = subset(dataset , subset = split == TRUE)
test_set = subset(dataset , subset = split == FALSE)

classifier = naiveBayes(x = training_set[, -3], y = training_set$Purchased)
y_pred = predict(classifier,newdata = test_set[, -3])

cm = table(test_set[, 3], y_pred)

set = training_set
X1 = seq(from = min(set[,1]) - 1, to = max(set[, 1]) + 1, by = 0.01)
X2 = seq(from = min(set[,2]) - 1, to = max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier,newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Training Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)),  add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1 , 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4','red3'))

set = test_set
X1 = seq(from = min(set[,1]) - 1, to = max(set[, 1]) + 1, by = 0.01)
X2 = seq(from = min(set[,2]) - 1, to = max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier,newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Test Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)),  add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1 , 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4','red3'))