#cart分類回歸數演算
library(rpart)




setwd("E:\\Rwork/")
getwd()
rm(list=ls())
babyData = read.csv(file.choose())
babyData1 = read.table("E:\\Rwork/files/babies.csv",header = T,sep = ",")
typeof(babyData)

head(babyData,10)
#排除NA值
babyData = na.exclude(babyData)

#訓練樣本70%與測試樣本30%
n = 0.3*nrow(babyData)
#抽樣30%的筆數
test.index = sample(1:nrow(babyData),n) 
#訓練即為刪去測試樣本 [row刪去測試,clumn全要(即為自變數)]
Train = babyData[-test.index,]
Test = babyData[test.index,]

#確認訓練樣本與測試樣本分不一致
par(mfrow=c(1,2)) #畫布要 1X2
hist(Train$bwt)#直方圖
hist(Test$bwt)
#cart 演算 並建模
baby.tree = rpart(bwt~.,data=Train)
par(mfrow=c(1,1))
plot(baby.tree)
text(baby.tree,cex=.8)

#vaiable importance
baby.tree$variable.importance

# 數值變數預測效果評估: MAPE(Mean Absolute Percentage Error)
# 絕對百分比誤差MAPE的公式為:
# 各個樣本的(實際值-預測值)/實際值取絕對值後的平均
# 預測效果為 GOOD: MAPE <10%
# 預測效果為 OK : 10% <= MAPE <20%
# 預測效果為 BAD : MAPE >=20%
# MAPE of train and test group
y = babyData$bwt[-test.index]
#predict中 模型使用baby.tree 即為CART分類樹演算法
y_hat = predict(baby.tree,newdata = Train, type = "vector")
train.MAPE = mean(abs(y - y_hat)/y)
cat("MAPE(train) = ",train.MAPE*100,"%")

y = babyData$bwt[test.index]
y_hat = predict(baby.tree,newdata = Test, type = "vector")
train.MAPE = mean(abs(y - y_hat)/y)
cat("MAPE(train) = ",train.MAPE*100,"%")










