startup_data = read.csv('50_Startups.csv')
startup_data$State = factor(x = startup_data$State, levels = c('New York', 'California', 'Florida'), labels = c(1, 2, 3))
splitset = sample.split(Y = startup_data$Profit,SplitRatio = 0.8)
training_Set = subset(x = startup_data, splitset == TRUE)
test_Set = subset(x = startup_data, splitset == FALSE)
regressor = lm(formula = Profit ~ .,data = training_Set)


y_pred = predict(regressor,newdata = test_Set)