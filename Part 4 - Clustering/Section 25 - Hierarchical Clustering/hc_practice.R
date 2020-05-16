dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

dendrogram = hclust(dist(dataset), method = 'ward.D')
plot(dendrogram,
     main = 'Dendrogram',
     xlab = 'Customers',
     ylab = 'Euclidean Distance')
hc = hclust(dist(dataset), method = 'ward.D')
y_hc = cutree(hc, 5)
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = TRUE,
         main = 'Cluster of clients',
         xlab = 'Annual Income',
         ylab = 'Spending Score')