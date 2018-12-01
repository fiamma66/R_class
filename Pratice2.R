install.packages("sqldf")
library(sqldf)

a<- data.frame(T_name=c("Tony","Jeff","Orozco"),
               age = c(25,24,26))
b<- data.frame(T_name = c("Tony","Jeff","Orozco","Carol"),
               Salary = c(20000,25000,30000,18000))
a
b
c=merge(b,a,by.x = "T_name",by.y = "T_name",all.x = TRUE)
##all.right join , all.x left join
c[1,3]=mean(c[-1,3])
class(sqldf(
  "select Species,count(*) as cnt
  from iris
  group by Species"
)) ##sqldf 回傳data.frame
sqldf(
  "select max([Sepal.Length]),count(*) as cnt
  from iris"
)
data(chickwts)
str(chickwts)
table(chickwts$feed)
splt1 = split(chickwts$weight,chickwts$feed)
chickwts[sample(1:nrow(chickwts),5,replace=F),] 
##sample 隨機抽樣 , replace=F 取後不放回 不重覆抽樣
iris
class(rownames(iris))
colnames(iris)
iris[as.integer(rownames(iris))%%7==0,]
## %%取餘數
subset(iris,iris$Sepal.Length>7)
## 無指定欄位挑選
subset(iris,iris$Sepal.Length>7,select = -Species)
## 去除Species欄位
subset(iris,iris$Sepal.Length>7,select = c(Sepal.Length,Species))




