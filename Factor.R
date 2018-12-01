rm(list=ls())
speed <- c("慢","快","極快","中")
speedFactor <- factor(speed)
speedFactor
speedFactor1 <- factor(speed,ordered = TRUE,levels = c("慢","中","快","極快"))
##自定義level

speedFactor1[4]


speedFactor1[2] <= speedFactor1[1]
##快大於慢 故False
class(speedFactor1)
##查看變數型態 ordered 有排序level







