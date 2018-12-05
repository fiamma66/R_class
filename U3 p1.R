
z<- data.frame(Main = c("牛","豚","牛","牛","牛","豚"),
               sup = c("有","有","沒有","有","沒有","有"),
               drink = c("tea","coffee","coffee","tea","tea","coffee")
               )
z
ftable(z,row.vars = 3,col.vars = "sup")

library(grid)
install.packages("vcd")
library(vcd)
install.packages("plyr")
library(plyr)

df <- data.frame(
  group = c(rep("個人戶",20),rep("企業戶",20)),
  sex = sample(c("M","F"),40,replace = T),
  age = floor(runif(n=40,min = 25, max = 40)),
  bill = floor(runif(n=40,min = 199, max = 2600))
)
ddply(df,.(group,sex),summarize,
      mean_age = round(mean(age),2),
      sd_age =round(sd(age),2),
      sum_bill = sum(bill),
      mean_bill = round(mean(bill),2)
      )

ddply(df,c("group","sex"),nrow)
ddply(df,c("group","sex","age"),nrow)
prop.table(df$bill)
df1 <- data.frame(df$bill,prop=prop.table(df$bill))

prop.table(table(df$age,df$group),margin =1 )
##margin = numm 表格內相加為1
##margin = 1 各列相加為1 , margin = 2 各行相加為1

df1[order(df1$df.bill,decreasing = F),] 


prop.table(ftable(z,row.vars = 1:2,col.vars = "drink"))


data("iris")
attach(iris)
plot(Petal.Length~Petal.Width,col=Species)
##plot(y=x,col=name)
lmTrain <- lm(formula = Petal.Length~Petal.Width,data = iris)
abline(reg=lmTrain$coefficients,col="red",lwd=1)


data("mtcars")
attach(mtcars)
T_cyl<-table(cyl) 
barplot(T_cyl,main="cyl汽缸數次數分配",xlab="汽缸數",
        col = c("red","green","blue"),names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"),
        border = "cyan",horiz = F)

T_cyl2 <- table(am,cyl)
barplot(T_cyl2,main="cyl汽缸數次數分配",xlab="汽缸數",
        col = c("red","green"),names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"),
        border = "cyan",horiz = F,
        legend = rownames(T_cyl2),
        beside = T
        )
##beside T為分組 F為堆疊

p_cyl <- prop.table(T_cyl2,2)
barplot(p_cyl,main="cyl汽缸數次數分配",xlab="汽缸數",
        col = c("red","green"),names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"),
        border = "cyan",horiz = F,
        legend = c("自動","手動"),
        beside = F
)
##機率分配堆疊

library(C50)
data(churn)
attach(churnTrain)
par(mfrow = c(2,2))##2*2電視牆

hist(total_day_minutes,xlab = "白天通話分鐘數",
     main="breaks=11",
     ylab="門號數",
     col="red")##breaks預設11
hist(total_day_minutes,xlab = "白天通話分鐘數",
     main="breaks=2",
     ylab="門號數",
     col="red",breaks = 2)
hist(total_day_minutes,xlab = "白天通話分鐘數",
     main="breaks=20",
     ylab="門號數",
     col="red",breaks = 20)
hist(total_day_minutes,xlab = "白天通話分鐘數",
     main="breaks=7",
     ylab="門號數",
     col="red",breaks = 7)

boxplot(total_day_minutes,horizontal = T,xlab = "TotalDayMinutes",
        col="pink")

dt = data.frame(total_eve_minutes
                ,total_night_minutes,
                total_day_minutes)
boxplot(dt,horizontal = T,xlab="通話分鐘數"
        ,col="green")

par(mfrow = c(1,1))

boxplot(total_eve_minutes~area_code*churn
        ,horizontal = F
        ,xlab = "夜晚通話時間"
        ,col = terrain.colors(3))

pieces <- c(8,8,2,4,2)
pie(pieces,label=c("工作","睡覺",
                   "念書","打電動"
                   ,"聊天"),main="生活")
pct <- round(pieces/sum(pieces)*100)
lbls <- paste(c("工作","睡覺",
                "念書","打電動"
                ,"聊天"),pct,"%",sep=)

pie(pieces,labels = lbls,main = "Life"
    ,cex = 1.5,cex.main=2)
##字體放大


##造假資料
years <- sort(round(runif(10,2000,2010),0))
newbornbaby <- sort(round(runif(10,200,1000),0))
dt <- data.frame(newbornbaby,years)

par(mfrow = c(3,2))
plot(newbornbaby ~ years, data=dt, type='l',col=1)
plot(newbornbaby ~ years , data=dt, type='b',col=2)
plot(newbornbaby ~ years , data=dt, type='c',col=3)
plot(newbornbaby ~ years , data=dt, type='h',col=4)
plot(newbornbaby ~ years , data=dt, type='s',col=5)

##Type變化

install.packages("treemap")
library(treemap)

y <- data.frame(C_ID = c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6","cluster7")
                ,C_RTO = c(0.22,0.3,0.16,0.04,0.19,0.03,0.06))
treemap(y,index=c("C_ID"),
        vSize = "C_RTO",
        vColor = "C_RTO")
x<- read.csv(file.choose())
treemap(x,index=c("縣市")
        ,vSize = "面積"
        ,vColor = "面積")
treemap(x,index=c("縣市","行政區名稱")
        ,vSize = "面積"
        ,vColor = "面積")

library(ggplot2)
install.packages("ggmap")
library(ggmap)
uv <- read.csv(file.choose())
map <- get_map(location = "New Taipei City"
               ,zoom = 13
               ,language = "zh-TW"
               ,maptype = "roadmap")

map <- get_map( location ='New Taipei City' ,zoom = 13,language = "zh-TW" , maptype = "roadmap")



data(churn)
data <- churnTrain[,c(-1,-3,-4,-5,-20)]
pca_Traindt <- princomp(data,cor=T)
summary(pca_Traindt)
par(mfrow = c(1,1))
screeplot(pca_Traindt,type="lines")
p <- predict(pca_Traindt)
head(p)
head(p[,c(1:7)])
















