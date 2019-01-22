
install.packages("arules")
library(arules)

# 建置交易資料
transaction_data = data.frame(Bread = c(T,T,F,T,T,F,F),
                              Milk = c(T,F,T,T,T,T,F),
                              Diaper = c(F,T,T,T,T,F,T),
                              Beer = c(F,T,T,T,F,F,T),
                              Coke = c(F,F,T,F,T,T,T))

# 建置關聯規則模型
rule = apriori(transaction_data, 
               parameter = list(supp=0.1,conf=0.8,maxlen=4))
inspect(rule)

summary(rule)


#依照support排序
inspect(head(sort(rule,by="support"),20)) 

#依照confidence排序
inspect(head(sort(rule,by="confidence"),20))

#依照lift排序

inspect(head(sort(rule,by="lift"),20))