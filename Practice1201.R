
x <- c(3,3,4,3,6,8,8,9)#藥劑量

y <- c(22,25,18,20,16,9,12,5)#感冒痊癒天數

New_x <- data.frame(x=11) #預測當x=5時的痊癒天數

# 建立一個線性迴歸模型
Train <- data.frame(x,y)


lmTrain <- lm(formula = y ~ x, data = Train)
predicted <- predict(lmTrain , newdata = New_x)
summary(lmTrain )
predicted


plot(y~x , main = "依藥劑量預測痊癒天數", xlab = "藥劑量", ylab = "感冒痊癒天數", family = "STHeiti")
points(x = New_x, y = predicted, col="green", cex = 2, pch = 16)
warnings()

abline(reg = lmTrain$coefficients, col = "red", lwd = 1) 
# 絕對百分比誤差MAPE的公式為:
  # 各個樣本的(實際值-預測值)/實際值取絕對值後的平均


y_hat=predict(lmTrain , newdata = data.frame(x=x))
train.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(train)=",train.MAPE*100,"%\n")

rm(list=ls())

# 自行產生藥劑量、平均每日睡眠時間與感冒痊癒天數資料
x1 <- c(3,3,4,3,6,8,8,9) #藥劑量
x2 <- c(3,1,6,4,9,10,8,11) #平均每日睡眠時數
y <- c(22,25,18,20,16,9,12,5) #感冒痊癒天數

#新患者資料
New_x1 <- 5 #預測當x=5時的痊癒天數
New_x2 <- 7 #每日睡眠時數
New_data <- data.frame(x1=5 , x2=7) 
#預測資料 變數名稱必須相同

Train <- data.frame(y,x1,x2)
#data.frame 中 無排列順序問題
lmTrain <- lm(formula = y ~.,data = Train)
#回歸模型中 才設定誰是應變數與自變數
predicted <- predict(lmTrain , newdata = New_data)
summary(lmTrain)











