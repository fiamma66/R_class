
install.packages("caret")
library(caret)
rm(list = ls())

#訓練樣本70%, 測試樣本30%
sample_index <- createDataPartition(y=iris$Species,p=0.7,list=F)
iris.train = iris[sample_index,]
iris.test = iris[-sample_index,]

#確認訓練樣本與測試樣本分不一致
par(mfrow=c(1,2))
#讓R的繪圖視窗切割成 1 X 2 的方塊

plot(iris.train$Species)
plot(iris.test$Species)

par(mfrow=c(1,1))
#C50模型訓練
iris.C50tree = C5.0(Species~. , data = iris.train)
summary(iris.C50tree)
plot(iris.C50tree)

#C50模型測試
y = iris$Species[-sample_index]
y_hat = predict(iris.C50tree,iris.test,type = 'class')
table.test = table(y,y_hat)
cat("Total records(train) = ",nrow(iris.test),"\n")
cat("Correct Classification Ratio(test) = ",
    sum(diag(table.test))/sum(table.test)*100,"%\n")


#隨機森林模型訓練
iris.RFtree = randomForest(Species ~ . ,
                           data = iris.train,
                           importane = T,
                           proximity = T,
                           ntree = 80)
#ntree 樹的數量 越多不一定越準
#proximity 是與不是的機率(類別問題)

print(iris.RFtree)

#變數重要性
round(importance(iris.RFtree),2)

#測試樣本的預測正確率
y = iris$Species[-sample_index]
y_hat = predict(iris.RFtree,newdata = iris.test)
table.test = table(y,y_hat)
cat("Correct Classification Ratio(test) = ",
    sum(diag(table.test))/sum(table.test)*100,
    "%\n")

#CART 演算法 電信churn
data(churn)
rm(list = ls())
data_train = churnTrain[,-3] 
#排除 "area_code"欄位

churn.tree = rpart(churn~.,
                   data = data_train)
# 繪製CART決策樹
plot(churn.tree)
text(churn.tree,cex=.8)#cex表字體大小

# 變數重要性
churn.tree$variable.importance

#測試樣本的混淆矩陣(confusion matrix)與預測正確率
y = churnTest$churn
y_hat = predict(churn.tree,
               newdata = churnTest,
               type = "class")
table.test = table(y,y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和

cat("Correct Classification Ratio(test)=",
    sum(diag(table.test))/sum(table.test)*100,
    "%\n")


#ROC
install.packages("ROCR")
library(ROCR)

# 測試樣本評分
y_prob = predict(churn.tree,
                 newdata = churnTest,
                 type = "prob")[,1]
#[,1]取正例預測機率
pred <- prediction(y_prob,
                   labels = churnTest$churn)

# tpr: True Positive Ratio 正確預測正例;
# fpr: False Positive Ration誤判為正例
perf <- performance(pred,"tpr","fpr")
plot(perf)
#type = Line, 2 <- 虛線
points(c(0,1),c(0,1),type = "l",lty=2)

#AUC
perf <- performance(pred,"auc")
(AUC = perf@y.values[[1]])
(Gini = (AUC-0.5)*2)*100

#lift chart
perf <- performance(pred , "lift", "rpp")
plot(perf)


#羅吉斯回歸
data_train = churnTrain[,-3]# 排除 "area_code"欄位
data_train = churnTrain[,-1]# 排除 “state"欄位

data_train$churn = ifelse(data_train$churn == "yes",
                          1,0)
# 羅吉斯回歸預設對 y=1 建模產出推估機率 yes=1 no = 0

#模型訓練
logitM = glm(formula = churn ~ .,
             data = data_train,
             family = binomial(link = "logit"),
             na.action = na.exclude)
summary(logitM)
#訓練樣本的混淆矩陣(confusion matrix)與預測正確率
install.packages("InformationValue")
library(InformationValue)


y = ifelse(churnTest$churn=='yes',1,0)
y_hat = predict(logitM,newdata = churnTest,
                type = "response")
table.test = table(y,y_hat > optimalCutoff(y,y_hat)[1])
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

# ROC Curve
y_prob <- predict(logitM,newdata = churnTest,
                  type = "response")
y_prob <-exp(y_prob)/(1+exp(y_prob)) #odds轉機率
pred <- prediction(y_prob, labels = churnTest$churn)
# tpr: True Positive Ratio 正確預測正例;
# fpr: False Positive Ration誤判為正例
perf <- performance(pred, "tpr", "fpr")
plot(perf)
points(c(0,1),c(0,1),type="l",lty=2) #畫虛線

#AUC
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )

#Gini
( Gini = (AUC-0.5) *2 )*100















