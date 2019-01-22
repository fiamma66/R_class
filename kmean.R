

iris_new <- iris[,-5]
set.seed(123)

Cluster_km <- kmeans(iris_new,
                     nstart = 15,
                     centers = 3)
# centers 為群數
# nstart 為重新放置K個中心點的次數
# 建議 >= 10

table(Cluster_km$cluster,iris[,5])
plot(iris_new$Petal.Width,
     iris_new$Petal.Length,
     col=Cluster_km$cluster)

# 最佳化 分群數


WSS_ratio <- rep(NA, times = 10)

# for 回圈
k = 1
for (k in 1:length(WSS_ratio)) 
{
  Cluster_km <- kmeans(iris_new, nstart=15,centers=k)     
  WSS_ratio[k] <- Cluster_km$tot.withinss
}

plot(WSS_ratio, type = "b", main = "陡坡圖")














