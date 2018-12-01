rm(list=ls())

data(cars)
str(cars)

##速度分三類 <12 <15 >=15
x1 <- cars$speed 
new_cars_band = 1*(x1<12)+2*(x1>=12&x1<15)+3*(x1>=15)
new_cars_band

class(label)
##數字標籤轉文字
label = c('慢','中','快','極快')
new_cars_band = label[new_cars_band]
new_cars_band


newcars <- cars
newcars <- within(newcars,
                  {
                    speed_level <- NA ##必須指定初值
                    speed_level[cars$speed<12] <- '慢'
                    speed_level[cars$speed>=15&cars$speed<15] <- '中'
                    speed_level[cars$speed>=15] <- '快'
                  })
head(newcars)


library(ggplot2)
library(foreign)
ex.matrix <- matrix(sample(c(-1, 0, 1), 24, replace = TRUE),
                    nrow = 4,
                    ncol = 6)
row.names(ex.matrix) <- c('A', 'B', 'C', 'D')
colnames(ex.matrix) <- c('P1', 'P2', 'P3', 'P4', 'P5', 'P6')
ex.mult <- ex.matrix %*% t(ex.matrix)
ex.dist <- dist(ex.mult)
class(ex.dist)
ex.mds <- cmdscale(ex.dist)
plot(ex.mds,type = "n")
text(ex.mds, c('A', 'B', 'C', 'D'))









