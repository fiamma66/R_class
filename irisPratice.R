source("trycatch1.r" , echo=TRUE)
getwd()

rm(list=ls())
source("trycatch2.r",echo=TRUE)

data <- c(1,2,3,6,3)
scale(data,center = T,scale=T)
install.packages("mice")
library(mice)
install.packages("missForest")
library(missForest)

tmp <- c(2,3,8,NA,4,NA,9,12,NA)
data <- prodNA(iris,noNA = 0.05)
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(data,col=c('navyblue','red'),numbers=TRUE,sortVars=TRUE,
                  labels=names(data),cex.axis=.7,gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

new_data <- data[complete.cases(data),]
summary(new_data)

newdata1 <- data
newdata1_col1 <- mean(newdata1[,1],na.rm = T)
narow1 <- is.na(newdata1[,1])
newdata1[narow1,1] <- newdata1_col1
summary(newdata1[,1])


mice.data <- mice(data,
                  m=3,
                  maxit = 30,
                  method = "cart",
                  seed=178)
summary(complete(mice.data,1))




mice.data <- mice(data,
                  m=3,
                  maxit = 20,
                  method = "rf",
                  seed = 188)
newdata2 <- complete(mice.data,2)
summary(newdata2)

