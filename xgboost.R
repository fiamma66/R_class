setwd("E:/Rwork")
install.packages("xgboost")
library(xgboost)
library(C50)
library(dplyr)
data(churn)

data_train = churnTrain[,-3]
data_test = churnTest[,-3]

dataTrain_matrix = Matrix::sparse.model.matrix(churn~.-1,
                                               data = data_train)

#-1 是去掉流水號

output_vector_train = churnTrain[,'churn']=="yes"

train_matrix = xgb.DMatrix(data = as.matrix(dataTrain_matrix),
                           label = output_vector_train)

dataTest_matrix = Matrix::sparse.model.matrix(churn ~ .-1, 
                                              data = data_test)

output_vector_test = churnTest[,'churn']=="yes"
test_matrix = xgb.DMatrix(data = as.matrix(dataTest_matrix),
                          label = output_vector_test)

# 模型參數設定
nc = 2 # 變數其實就是2 是與否
# 預測變數Y有幾類
params = list("objective" = "multi:softprob", 
              #結果包含預測機率與預測類別
              "eval_metric" = "mlogloss",
              "num_class" = nc
              )
watchlist = list(train = train_matrix, test = test_matrix)

bst_model = xgb.train(params = params,
                      data = train_matrix,
                      nrounds = 100,
                      watchlist = watchlist,
                      eta = 0.3,
                      # Learning Rate, low -> more robust to overfitting
                      max.depth = 5,
                      seed = 123)

evalue_log = bst_model$evaluation_log
plot(evalue_log$iter, evalue_log$train_mlogloss,
     col= "blue")
lines(evalue_log$iter, evalue_log$test_mlogloss,
      col="red")

bst_model = xgb.train(params = params,
                      data = train_matrix,
                      nrounds = 18,
                      watchlist = watchlist,
                      eta = 0.3,
                      # Learning Rate, low -> more robust to overfitting
                      max.depth = 5,
                      seed = 123)


var_feature = xgb.importance(colnames(train_matrix),
                             model = bst_model)
print(var_feature)
xgb.plot.importance(var_feature)


# 預測新資料
p = predict(bst_model, newdata = test_matrix)
pred = matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = output_vector_test,
         max_prob = max.col(.,"last")-1)

# 評分
table.test = table(output_vector_test,pred$max_prob)

cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")







