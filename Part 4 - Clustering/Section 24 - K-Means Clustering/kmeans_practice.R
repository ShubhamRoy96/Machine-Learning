dataset = read.csv('Mall_Customers.csv')
x <- dataset[4:5]
set.seed(6)
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(x, i)$withinss)
}
plot(1:10, wcss, type = "b", main = 'Cluster of clients', xlab = "Number of clusters", ylab = "WCSS")

set.seed(29)
kmeans <- kmeans(x, 5, iter.max = 300, nstart = 10)

library(cluster)
clusplot(x,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = TRUE,
         main = 'Cluster of clients',
         xlab = 'Annual Income',
         ylab = 'Spending Score')