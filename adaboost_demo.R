rm(list = ls())

library(rpart)
library(C50)

data(churn)
# 排除 area_code
data_train = churnTrain[,-3]
churn.tree = rpart(churn~., data = data_train)

y = churnTest$churn
y_hat = predict(churn.tree, newdata=churnTest,type="class")
table.test = table(y,y_hat)

# 計算 ACC
cat("Correct Classification Ratio(test)=",
    sum(diag(table.test))/sum(table.test)*100,"%\n")




install.packages("fastAdaboost")
library(fastAdaboost)
# Adaboost
# 10表示有10個弱分類器
churn_adaboost <- adaboost(churn~., data_train, 10) 
pred <- predict( churn_adaboost,newdata=churnTest)
cat("Correct Classification Ratio(test)=", (1- pred$error)*100,"%\n")

data_train$churn = ifelse(data_train$churn=="yes",1,0)
# GBM的Y變數僅識別 0與1


churn_GBM = gbm(churn~.,
                data=data_train,
                distribution = "bernoulli",
                n.trees = 3000,
                interaction.depth = 4,
                shrinkage = 0.01,
                bag.fraction = 0.5,
                cv.folds = 5) #  交叉驗證組數
# GBM作者建議shrinkage參數設在0.01 ~ 0.001之間

best.iter = gbm.perf(churn_GBM, method = "cv")

# 使用 最佳迭代
churn_GBM = gbm(churn~.,
                data=data_train,
                distribution = "bernoulli",
                n.trees = best.iter,
                interaction.depth = 4,
                shrinkage = 0.01,
                bag.fraction = 0.5,
                cv.folds = 5)

summary(churn_GBM)


# 評分模型
data.test = churnTest
data.test$churn = ifelse(data.test$churn=="yes",1,0)

pred = predict(churn_GBM, newdata = data.test,
               n.trees = best.iter)


# 繪製 ROC 圖
install.packages("pROC")
library(pROC)
library(stats)

churn.roc = roc(data.test$churn,pred)
plot(churn.roc)

# 產出的結果 是連續機率
# 若要評分模型 仍要使用混淆矩陣
# 用 coords 切割最佳臨界值 來判斷 0 與 1 -> no & yes

coords(churn.roc,"best")
churn.predict.class = ifelse(pred > coords(churn.roc,"best")["threshold"],"yes","no")

table(data.test$churn,churn.predict.class)

table.test = table(data.test$churn,churn.predict.class)

cat("Correct Classification Ratio(test)=",
    sum(diag(table.test))/sum(table.test)*100,"%\n")













