

iris_new <- iris[,-5]
set.seed(123)

Cluster_km <- kmeans(iris_new,
                     nstart = 15,
                     centers = 3)
table(Cluster_km$cluster,iris[,5])
plot(iris_new$Petal.Width,
     iris_new$Petal.Length,
     col=Cluster_km$cluster)




