
babyData = read.table(file.choose(),sep = ",",header=T,row.names = NULL)

install.packages('gbm')
library(gbm)

babyData= na.exclude(babyData)
n = 0.3*nrow(babyData)
test.index = sample(1:nrow(babyData),n)

Train = babyData[-test.index,]
Test = babyData[test.index,]

set.seed(123)
bwt_GBM = gbm(bwt~.,data = Train,
              distribution = "gaussian",
              n.trees = 5000,
              interaction.depth = 4,
              shrinkage = 0.001,
              bag.fraction = 0.5)
# distribution：損失函數 
# n.trees：迭代次數 
# interaction.depth：決策樹深度
# shrinkage: learning rate 避免過度訓練
# bag.fraction 後續模型訓練的抽樣比率


summary(bwt_GBM)
plot(bwt_GBM, i="gestation")

y=babyData$bwt[test.index]

y_hat = predict(bwt_GBM, newdata=Test,n.trees = 5000)
test.MAPLE = mean(abs(y-y_hat)/y)

cat("MAPE(TEST)=",test.MAPLE*100,"%\n")

best.iter = gbm.perf(bwt_GBM,method = "cv")

