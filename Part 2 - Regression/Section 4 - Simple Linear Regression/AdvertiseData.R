data = read.csv("Advertising.csv")
ggplot()+
  geom_point(aes(x = data$TV,y = data$sales),color="red")+
  geom_point(aes(x = data$radio,y = data$sales),color="black")
  

split = sample.split(data$sales,SplitRatio = 2/3)
training_set = subset(data,split == TRUE)
test_set = subset(data, split == FALSE)
regressor = lm(formula = sales ~ TV,data = training_set)
sales_pred = predict(regressor,newdata = training_set)

ggplot()+
  geom_point(aes(x = training_set$TV,y = training_set$sales),color = "red")+
  geom_line(aes(x = training_set$TV,y = sales_pred),color = "blue")+
  xlab("TV ads")+
  ylab("Sales")+
  ggtitle("Effect of TV ads on Sales")

ggplot()+
  geom_point(aes(x = test_set$TV,y = test_set$sales),color = "red")+
  geom_line(aes(x = training_set$TV,y = sales_pred),color = "blue")+
  xlab("TV ads ( Test )")+
  ylab("Sales ( Predicted )")+
  ggtitle("Effect of TV ads on Sales ( Predicted )")