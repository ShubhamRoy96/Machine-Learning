adv_data = read.csv('Advertising.csv')
radio_data = adv_data$radio
sales = adv_data$sales

ggplot()+
  geom_point(aes(x = radio_data, y = sales),color = 'red' )

radio_data = ifelse(test = is.na(radio_data),ave(x = radio_data,FUN = 
                                                   function(x) mean(x, na.rm = TRUE)),radio_data)
split = sample.split(adv_data$sales,SplitRatio = 2/3)
training_set = subset(adv_data,split==TRUE)
test_set = subset(adv_data,split==FALSE)
regressor = lm(formula = sales ~ radio,data = adv_data)
sales_pred = predict(regressor,newdata = training_set)
ggplot()+
  geom_point(aes(training_set$radio,training_set$sales), color = 'red')+
  geom_line(aes(training_set$radio,sales_pred), color = 'blue')+
  ggtitle("Effect of Radio on Sales")+
  xlab("Radio")+
  ylab("Sales")