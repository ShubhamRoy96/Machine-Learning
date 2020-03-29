Salary_data = read.csv('Position_Salaries.csv')
Salary_data = Salary_data[2:3]
lin_reg = lm(formula = Salary ~ .,data = Salary_data)
Salary_data$LevelPol = Salary_data$Level^2
Salary_data$LevelPol2 = Salary_data$Level^3
Salary_data$LevelPol3 = Salary_data$Level^4
Salary_data$LevelPol4 = Salary_data$Level^5
Salary_data$LevelPol5 = Salary_data$Level^6

pol_reg = lm(formula = Salary ~ ., data = Salary_data)

ggplot()+
  geom_point(aes(Salary_data$Level,Salary_data$Salary),color = 'red')+
  geom_line(aes(Salary_data$Level,predict(lin_reg)),color = 'blue')+
  ggtitle("Linear regression Data")+
  xlab("Level")+
  ylab("Salary")


ggplot()+
  geom_point(aes(Salary_data$Level,Salary_data$Salary),color = 'red')+
  geom_line(aes(Salary_data$Level,predict(pol_reg)),color = 'blue')+
  ggtitle("Polynomial regression Data")+
  xlab("Level")+
  ylab("Salary")