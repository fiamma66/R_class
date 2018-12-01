

install.packages("dplyr")
library(dplyr)
install.packages("C50")
library(C50)
?(churn)
data(churn)
str(churnTrain)
x <- c(55,80,47,25,65,34)
mean(x)
range(x)
median(x)
sd(x)
var(x)
sd(x)^2
max(x)
min(x)
quantile(x)

High <- c(120,134,110,155,121,104,140)
Weight <- sample(20:29,7,replace = T)
cor(High,Weight) #相關係數
plot(High,Weight)

data("airquality")

head(airquality)
cor(airquality[,1:4],use = "pairwise")
pairs(airquality[,1:4])

