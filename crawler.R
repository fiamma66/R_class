ls()
rss_url_new <- 'https://www.mobile01.com/rss/topiclist383.xml'

rss_new <-tidyRSS::tidyfeed(feed = rss_url_new)

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

data_new <- list()
i <- 1
for(link in rss_new$item_link){
  print(link)
  html_doc <- htmlParse(getURL(link,
                               curl = curl_handle),
                        encoding = "UTF-8")
  article_item <- xpathSApply(html_doc,
                              "//*[@id='main-content']/div[5]",
                              xmlValue)
  
  data[i] <- article_item
  
  i <- i + 1
  t <- sample(1:3,1)
  Sys.sleep(t)
  
  
}






