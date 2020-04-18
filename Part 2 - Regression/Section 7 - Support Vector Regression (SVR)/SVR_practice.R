dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]
library(e1071)
regressor = svm(formula = Salary ~ ., data = dataset, type = 'eps-regression')
y_pred = predict(regressor, data.frame(Level = 6.5))
ggplot()+
  geom_point(aes(dataset$Level,dataset$Salary), color = 'red')+
  geom_line(aes(dataset$Level, predict(regressor, newdata = dataset)), color = 'blue')+
  ggtitle("Salary vs Level")+
  xlab("Levels")+
  ylab("Salary")