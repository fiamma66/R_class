y=data.frame(T_ID=c(3,4,5),
             T_name=c("Tony","Tom","Chris"),
             Birthday = as.Date(c("2017-08-07","2014-05-06","2011-07-01"))
)
y
y$T_ID
y$Birthday
## $呼叫資料框中欄位所有屬性
y$T_name[3]

y[,c(2,3 )]
## 取第二行與第三行,全列都取

z= data.frame(cell_phone=c('0926777757','0921855214','0988755314'))

cbind(y,z)
z
y

k= data.frame(T_ID=6,T_name='Catt',Birthday=as.Date('2009-05-05'))
k
rbind(y,k)
## bind不會更改原資料 只顯示組合結果
y=cbind(y,z)
y
rbind(y,k)## 欄位或列數不符 不會合併

