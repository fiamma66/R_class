#處理編碼
Sys.getlocale()


install.packages("jiebaR")
install.packages("Rcpp")
library(jiebaR)
library(Rcpp)

seg_worker = worker()
segment("這是一段本文喔",seg_worker)
segment("全台,大停電",seg_worker)
seg_worker
segment("ios12.1根本一堆BUG
        超爛,不建議大家更新",seg_worker)
#Packages
install.packages("devtools")
install.packages("tidyRSS")
install.packages("XML")
install.packages("RCurl")
install.packages("plyr")
install.packages("wordcloud")
install.packages("wordcloud2")
library(tidyRSS)
library(XML)
library(jiebaR)
library(stringr)
library(plyr)
library(wordcloud2)
library(wordcloud)
library(RCurl)
#URL RSS2.0版本無法取得
rss_url1 <- 'https://tw.appledaily.com/rss/newcreate/kind/rnews/type/104'
rss_url2 <- 'http://rss.ptt.cc/Gossiping.xml'
rss_url <- 'https://www.mobile01.com/rss/news.xml'

rss <- tidyRSS::tidyfeed(feed = rss_url)




#準備Request
rss_url <- "https://tw.appledaily.com/rss/newcreate/kind/rnews/type/103"
rss <- tidyRSS::tidyfeed(feed=rss_url1)

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.80 Safari/537.36"

myHttpHeader <- c(
  "User-Agent" = ua,
  "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
  "accept-Language" = "zh-TW,zh;q=0.9,en-US;q=0.8,en;q=0.7",
  "accept-encoding" = "gzip, deflate, br",
  "Connection" = "keep-alive",
  "cache-control" = "max-age=0",
  "Accept-Charset" = "UTF8,utf-8;q=0.7,*;q=0.7"
  )

curl_handle <- getCurlHandle()
curlSetOpt(cookiejar = "cookies.txt",
           useragent = ua,
           followlocation = TRUE,
           curl = curl_handle)


#設計爬蟲
data <- list()
i <- 1
for(link in rss$item_link){
  print(link)
 # tryCatch({
  #爬取Curl 為靜態 無法爬取動態javascript 
    html_doc <- htmlParse(getURL(link,
                                 curl = curl_handle),
                          encoding = "UTF-8")
    article_item <- xpathSApply(html_doc,
                                "//*[@id='article']/div[2]/div/main/article/div/div[2]/article/div/p",
                                xmlValue)
    
    #加入判斷 article_item 長度不為0 才去空白
   # article_item <- gsub("\\s+","",article_item) #正規化去除空白
   # article_item <- gsub(" $","",article_item) 
    data[i] <- article_item
    
    
 # },
 # error = function(msg){
 #   message(paste("[Error]",msg,"\n"))
  #  return(NA)
 # }
 # )
  
  i <- i + 1
  t <- sample(1:3,1)
  Sys.sleep(t)
  #休息可在不同地方 越不規則 
  #越不容易被認為機器
  
  
}
data <- unlist(data)
#文字雲

#分詞
cutter = worker(stop_word = "C:/Users/Big\ data/Documents/R/win-library/3.5/jiebaRD/dict/stop_words")
#此段由後往前 # <= 本是運算子 這裡可單獨用在分詞器
seg_words <- cutter <= data

#計算文字出現頻率

table_words <- count(seg_words) 
tail(table_words,20)
#繪圖
wordcloud(table_words[,1],
          table_words[,2],
          random.color = F,
          random.order = F,
          colors = rainbow(10),
          min.freq = 15,
          )
#table_words[,1] 文字
#table_words[,2] 字頻






