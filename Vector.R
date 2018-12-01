
x <- rep(TRUE,5)
x

x[1] <- 2
x

x[1] <- "sss"
x

x<- c(0,FALSE,"PLZ")
x   ## 強制轉換 文字>數字>Boolean

x <- c(1,3,5,3,1,2)
y <- c(2,3,5,1,3,5)
x*y
warning()
y <- c(1,5,4)
x <-5
y<-2

x=seq(from=11,to=78,by=7)
x = c(4,1,3,7,2,1,4,1)
names(x) = paste("v",1:length(x),sep="")
x[x==1]##顯示值的所在
which(x==1)##取出(值)的"位置"
unique(x) ##去除重複
sort(x,decreasing = TRUE) 
##排列 decreasing = (default) false 小到大
