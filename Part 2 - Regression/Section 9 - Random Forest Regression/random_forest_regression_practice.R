dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]
x = dataset$Level
y = dataset$Salary
regressor = randomForest(dataset[1], dataset$Salary, ntree = 1000)
y_pred = predict(regressor, data.frame(Level = 6.5))
x_grid = seq(min(x), max(x),0.01)
ggplot()+
  geom_point(aes(x, y), color = 'red')+
  geom_line(aes(x_grid, predict(regressor, data.frame(Level = x_grid))),color = 'blue')+
  ggtitle('Salary vs Level')+
  xlab('Level')+
  ylab('Salary')