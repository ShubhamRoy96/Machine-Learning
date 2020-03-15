data = read.csv("Advertising.csv")
split = sample.split(data$sales,SplitRatio = 2/3)
training_set = subset(data,split == TRUE)
test_set = subset(data, split == FALSE)
regressor1 = lm(formula = sales ~ TV,data = training_set)
regressor2 = lm(formula = sales ~ radio,data = training_set)
sales_pred1 = predict(regressor1,newdata = training_set)
sales_pred2 = predict(regressor2,newdata = training_set)

#ggplot()+
  #geom_point(aes(x = training_set$TV,y = training_set$sales),color = "red")+
  #geom_line(aes(x = training_set$TV,y = sales_pred),color = "blue")+
  #xlab("TV ads")+
  #ylab("Sales")+
  #ggtitle("Effect of TV ads on Sales")

ggplot()+
  geom_point(aes(x = training_set$TV,y = training_set$sales),color = "red")+
  geom_point(aes(x= training_set$radio, y = training_set$sales), color = 'magenta')+
  geom_line(aes(x = training_set$TV,y = sales_pred1),color = "blue")+
  geom_line(aes(training_set$radio,sales_pred2), color = 'black')+
  xlab("TV ads ( Test )")+
  ylab("Sales ( Predicted )")+
  ggtitle("Effect of TV ads on Sales ( Predicted )")
