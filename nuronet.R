
install.packages("neuralnet")
install.packages("nnet")

library(nnet)
library(neuralnet)
rm(list = ls())
data(iris)

# One-hot Encoding
head(class.ind(iris$Species))
data <- cbind(iris,class.ind(iris$Species))

# 產生建模與測試樣本 7:3
n = 0.3*nrow(data)
test.index = sample(1:nrow(data),n)
Train = data[-test.index,]
Test = data[test.index,]

# 建模
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
BNP = neuralnet(formula = formula.bpn,
                hidden = c(5,3,2),
                data = Train,
                learningrate = 0.01,
                threshold = 0.01,
                stepmax = 5e5)
# 最大的ieration數 = 500000(5*10^5)
plot(BNP)

# 預測
cf = compute(BNP,Test[,1:4])
Ypred = as.data.frame(round(cf$net.result)) #預測結果

# 建立一個新欄位，叫做Species
Ypred$Species <- ""
# 把預測結果轉回Species的型態
# 根據虛擬變數
for(i in 1:nrow(Ypred)){
  if(Ypred[i, 1]==1){ Ypred[i, "Species"] <- "setosa"}
  if(Ypred[i, 2]==1){ Ypred[i, "Species"] <- "versicolor"}
  if(Ypred[i, 3]==1){ Ypred[i, "Species"] <- "virginica"}
  }
Ypred
table(Test$Species,Ypred$Species)

# 混淆矩陣 
cat("準確度：",sum(diag(table(Test$Species,Ypred$Species))) /
          sum(table(Test$Species,Ypred$Species)) *100,"%")






