dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]
x = dataset$Level
y = dataset$Salary
regressor = rpart(formula = Salary ~ ., data = dataset, control = rpart.control(minsplit = 1))
y_pred = predict(regressor, data.frame(Level = 6.5))
x_grid = seq(min(x), max(x), 0.001)
ggplot()+
  geom_point(aes(x, y), color = "red")+
  geom_line(aes(x_grid, predict(regressor, data.frame(Level = x_grid))), color = "blue")+
  ggtitle("Level vs Salary")+
  xlab("Level")+
  ylab("Salary")