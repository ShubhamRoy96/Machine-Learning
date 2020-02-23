data = read.csv('Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/Data.csv')
data$Age = ifelse(is.na(data$Age), ave(data$Age, FUN = function(x) mean(x , na.rm = TRUE)), data$Age)
data$Salary = ifelse(is.na(data$Salary), ave(data$Salary, FUN = function(x) mean(x , na.rm = TRUE)), data$Salary)
#data$Purchased = ifelse(is.na(data$Purchased), ave(data$Purchased, FUN = function(x) mean(x, na.rm = TRUE)), data$Purchased)
View(data)

encoded_data = data

encoded_data$Country = factor(encoded_data$Country, labels = c(1, 2, 3))
encoded_data$Purchased = factor(encoded_data$Purchased, levels(unique(encoded_data$Purchased)),labels = c(0,1))
View(encoded_data)