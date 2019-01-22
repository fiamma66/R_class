install.packages("ggplot2")
install.packages("fpc")
library(ggplot2)
library(fpc)


# 產出資料 
x1 = seq(0,pi, length.out = 500)
y1 = sin(x1) + 0.2*rnorm(500)
x2 = 2 + seq(0 , pi , length.out = 500)
y2 = cos(x2) +0.2*rnorm(500)

data = data.frame(c(x1,x2), c(y1,y2))

names(data) = c("x" , "y")

qplot(data$x , data$y)

p = ggplot(data, aes(x,y))

# 建置Dbscan模型
DbscanModel = dbscan(data, eps = 0.15, MinPts = 4)
p + geom_point(size = 2.5, aes(colours=factor(DbscanModel$cluster))) +theme(legend.position='top')


KmeansModel <- kmeans(data,centers=2,nstart=10)
p + geom_point(size=2.5,aes(colour=factor(KmeansModel $cluster)))+theme(legend.position='top')
