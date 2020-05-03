dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]
dataset[, 1:2] = scale(dataset[, 1:2])
split = sample.split(dataset, SplitRatio = 0.75)
training_set = subset(dataset, subset = split == TRUE)
test_set = subset(dataset, subset = split == FALSE)
y_pred = knn(train =  training_set[, -3],
             test =  test_set[, -3],
             cl = training_set[, 3],
             k = 5)
cm = table(test_set[,3], y_pred)

set = training_set
X1 = seq(from = min(set[,1]) - 1, to = max(set[, 1]) + 1, by = 0.01)
X2 = seq(from = min(set[,2]) - 1, to = max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train =  training_set[, -3],
             test =  grid_set[, -3],
             cl = training_set[, 3],
             k = 5)
plot(set[, -3],
     main = 'K-NN (Training Set)',
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
y_grid = knn(train =  training_set[, -3],
             test =  grid_set[, -3],
             cl = training_set[, 3],
             k = 5)
plot(set[, -3],
     main = 'K-NN (Test Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)),  add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1 , 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4','red3'))