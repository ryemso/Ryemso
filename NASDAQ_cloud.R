library(tidyverse)
library(dplyr)
#install.packages("SnowballC")
#install.packages("tm")
library(SnowballC)
library(wordcloud)
library(tm)
## 출처 : https://e-datanews.tistory.com/155
library(multilinguer)
# install_jdk()
# install.packages(c("hash","tau","Sejong","RSQLite","devtools","bit","rex",
#                   "lazyeval","htmlwifgets","crosstalk","promises","later",
#                  "sessioninfo","xopen","bit64","blob","DBl","memoise","plogr",
#                  "covr","DT","rcmdcheck","rversions"),type="binary")

#install.packages("remotes")
#remotes::install_github('haven-jeon/KoNLP',upgrade = "never",
                       # INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
library(stringr)
library(dplyr)

news_1 <- read.csv("NASDAQ_RSS_IFO_202301.csv")
#str(news_1)
news_1_0 <- subset(news_1,select=c("rgs_dt","news_smy_ifo"))

#news_2 <- read.csv("NASDAQ_RSS_IFO_202302.csv")
#news_3 <- read.csv("NASDAQ_RSS_IFO_202303.csv")
#news_4 <- read.csv("NASDAQ_RSS_IFO_202304.csv")
#news_5 <- read.csv("NASDAQ_RSS_IFO_202305.csv")
#news_6 <- read.csv("NASDAQ_RSS_IFO_202306.csv")
#news_7 <- read.csv("NASDAQ_RSS_IFO_202307.csv")
#news_8 <- read.csv("NASDAQ_RSS_IFO_202308.csv")
#news_1_0_0 <- str_replace_all(news_1, "\\w"," ")
news_1_1 <- as.character(news_1_0)
news_1_2 <- strsplit(news_1_1," ")
#news_1_3 <- (extractNoun(news_1_2))
head(news_1_2)

news_1_nounscount <- table(unlist(news_1_2))
news_1_nounscount_1 <- sort(news_1_nounscount, decreasing = T)
news_1_nounscount_1
#nounscount_2 <- 
head(news_1_nounscount_1)
news_1_nounscount_2 <- news_1_nounscount_1[35:11682]
head(news_1_nounscount_2)
news_1_3 <- unlist(news_1_nounscount_2)
news_1_3
txt <- readLines("D:/workspace/gsubnews.txt") 
cnt_txt <- length(txt)
cnt_txt

for( i in 1:cnt_txt) {
  news_1_3 <-gsub((txt[i]),"",news_1_3)      
} 
news_1_3 
#news_1_3 <- tm_map(news_1_nounscount_1,removePunctuation)
#news_1_3 <- tm_map(news_1_2,removeNumbers)
#inspect(news_1_3)
#news_1_4 <- TermDocumentMatrix(news_1_nounscount_2,control = list(wordLength=c(4,16)))
#news_1_4
#news_1_5 <- as.data.frame(as.matrix(news_1_nounscount_2))
#news_1_5
news_1_6 <- sort(news_1_3,decreasing = T)
news_1_6

news_1_7 <- names(news_1_6)
news_1_7
pal<-brewer.pal(8, "Accent")
#is.integer(news_1_7)
#is.character(news_1_nounscount_2)
#news_1_7 <- as.integer(news_1_7)
#is.integer(news_1_7)
#sum(is.na(news_1_7))
#news_1_nounscount_2 <- as.character(news_1_nounscount_2)
#is.character(news_1_nounscount_2)
wordcloud(news_1_7,freq = news_1_6, min.freq = 300,
          scale =c(8,0.1),colors=pal, random.order = F, rot.per=0.1)

#head(nouns)
wordcount <- table(unlist(df_nounscount)) 
wordcount

$#cloud <- sapply(cloud,extractNoun, USE.NAMES = F)