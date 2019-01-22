setwd("e:\\Rwork")

x = read.table("files\\san13.csv",header=T , sep=",",fileEncoding = "utf-8")

model_data = data.frame(x$武將
                        ,x$統率,
                        x$武力,
                        x$智力,
                        x$政治)
set.seed(123)
WSS_ratio = rep(NA, times=10)

# model_data[-1] : 去除武將性名

for (k in 1:length(WSS_ratio))
{
  cluster_km = kmeans(model_data[-1],
                      nstart = 15,
                      centers = k)
  WSS_ratio[k] = cluster_km$tot.withinss
  
}

plot(WSS_ratio , type = "b", main = "陡坡圖")
# 分群最佳 看來 = 3


cluster_km = kmeans(model_data[-1], nstart = 15 , centers = 3)

# 將 分群結果 加入資料表 欄位cluster
final_data = data.frame(x,cluster = as.character(cluster_km$cluster))

# 資料清理工作包
install.packages("dplyr")
library(dplyr)

#轉換 dplyr 格式
with_model_data = tbl_df(final_data)


# %>% 將前面結果往後丟 
# pipeline 同樣意思
result <- with_model_data %>%
  dplyr::group_by(cluster) %>%
  summarise(
    count = n(),
    median_統率 = median(統率, na.rm = TRUE),
    median_武力 = median(武力, na.rm = TRUE),
    median_智力 = median(智力, na.rm = TRUE),
    median_政治 = median(政治, na.rm = TRUE),
  )
# 分析各群切割變數

write.table(result, file = "result.csv",
            col.names = T,
            row.names = F,
            sep = ",",
            quote = F)


# 查看群內容
subset(final_data, final_data$cluster==2)[,1:5]

install.packages("vcd")
library(vcd)

#特徵分析
table(final_data$cluster, final_data$槍)
# 1 -> 列相加為1
# 2 -> 行相加為1
100 * prop.table(table(final_data$cluster, final_data$槍),2)









