dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]
dataset[1:2] = scale(dataset[1:2])
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
classifier = glm(formula = Purchased ~ ., family = 'binomial', data = training_set)
prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set[,3],y_pred)

set = training_set
X1 = seq(min(set[, 1])-1, max(set[,1])+1, by = 0.01)
X2 = seq(min(set[, 2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age','EstimatedSalary')
prob_pred = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_pred > 0.5, 1, 0)
plot(set[,-3],
     main = 'Logistec Regression (Training Set)', 
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3','tomato'))
points(set, pch = 21 , bg = ifelse(set[, 3] == 1, 'green4','red3'))