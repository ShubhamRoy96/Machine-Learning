Salary_Data = read.csv("Salary_Data.csv")
set.seed(1234)
split = sample.split(Salary_Data$Salary,SplitRatio = 2/3)
training_set = subset(Salary_Data,split==TRUE)
test_set = subset(Salary_Data,split==FALSE)
regressor = lm(formula = Salary ~ YearsExperience,data = training_set)
y_pred = predict(regressor,newdata = test_set)
ggplot()+
  geom_point(aes(x = training_set$YearsExperience,y = training_set$Salary),color = "red")+
  geom_line(aes(x = training_set$YearsExperience,y = predict(regressor,newdata = training_set)),
            color = "blue")+
  ggtitle("Salary VS Years of Experience ( Training set )")+
  xlab("Years of experience")+
  ylab("Salary")
  
  ggplot()+
    geom_point(aes(x = test_set$YearsExperience,y = test_set$Salary),color = "red")+
    geom_line(aes(x = training_set$YearsExperience,y = predict(regressor,newdata = training_set)),
              color = "blue")+
    ggtitle("Salary VS Years of Experience ( Test set )")+
    xlab("Years of experience")+
    ylab("Salary")
  