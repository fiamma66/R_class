rm(list = ls())
library(C50)
library(tree)
library(rpart)
data(churn)
data <- churnTrain[,c(-1,-3,-4,-5,-20)]

head(data,5)


pca_Traindt <- princomp(data , cor=T)
summary(pca_Traindt)

p <- predict(pca_Traindt)
p<-p[,c(1:7)]
nrow(data_train)
nrow(p[,c(1:7)])
data_train = churnTrain[,-3]
churn.tree = rpart(churn ~ p[,c(1:7)],data=data_train)
churn.tree
plot(churn.tree)
text(churn.tree,cex=.8)
y = churnTrain$churn
y_hat = predict(churn.tree,newdata = churnTrain,
                type = "class")
table.train = table(y,y_hat)
cat("Correct Classification Ratio(test)=", sum(diag(table.train))/sum(table.train)*100,"%\n")

