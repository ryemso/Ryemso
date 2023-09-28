library(tidyverse)
traffics <- read_csv("traffics.csv", col_names=TRUE,
                     locale=locale(encoding="euc-kr"))
glimpse(traffics)
traffics
교통사고_중구 <-  filter(traffics, 자치구 == "중구",
                   유형 == "발생건수")
print(교통사고_중구)
project1 <- ggplot(data = 교통사고_중구)+
  geom_point(mapping = aes(x=월, y=건수)) +
  xlim(1,13)+ylim(0,100)+
  ggtitle("중구 월별 교통사고 발생 건수")
project1
project2 <- ggplot(data = 교통사고_중구)+
  geom_line(mapping = aes(x=월, y=건수)) +
  xlim(1,13)+ylim(0,100)+
  ggtitle("중구 월별 교통사고 발생 건수")
project2
교통사고_5월 <- filter(traffics, 월==5,
                  유형 == "발생건수")
project3 <- ggplot(data = 교통사고_5월, aes(x=자치구, y=건수))+
  geom_bar(stat = "identity", col="blue", fill= "yellow")+
  coord_cartesian()+
  theme(axis.text.x = element_text(angle=90, vjust=0))+
  ggtitle("5월 자치구별 교통사고 발생 건수")
project3
heat <- filter(traffics, 월==5)
heat <- spread(heat, key = '유형',value='건수')
heat <- as.data.frame(heat)
row.names(heat) <- heat$자치구
heat_5 <- heat[,c(3:5)]
heat_5
heat_5_mx <- data.matrix(heat_5)
heatmap(heat_5_mx, Rowv=NA, Colv=NA,
        col=cm.colors(256), scale="column",
        margin=c(5,5),cexCol=1)
heatmap(heat_5_mx,
        col=cm.colors(500), scale="column",
        margin=c(5,5), cexCol=1.5)

교통사고_11월 <- filter(traffics, 월==11)
교통사고_11월_유형 <- spread(교통사고_11월, key= '유형', value='건수')
교통사고_11월_유형 %>%
  ggplot(mapping = aes(x=발생건수, y=부상자수))+
  geom_point()+
  ggtitle("교통사고 발생건수와 부상자수의 관계")
교통사고_11월_유형 %>%
  ggplot(mapping = aes(x=발생건수, y=부상자수))+
  geom_point()+
  ggtitle("교통사고 발생건수와 부상자수의 관계")
  stat_smooth(method="lm", se=FALSE, color="red")


교통사고_11월_유형 %>%
  ggplot(aes(x=발생건수, y=부상자수))+
  geom_point(aes(size=사망자수), shape=16,
             colour="blue", alpha=0.5)+
  ggtitle("교통사고 발생건수와 부상자수 관계 버블차트")

교통사고_강남구 <- filter(traffics, 자치구=="강남구",
                   유형 == "발생건수")
head(교통사고_강남구)
g1 <- subset(교통사고_강남구, 월==3)
g1
project4 <- ggplot(data = 교통사고_강남구, aes(x=월, y=건수))+
  geom_point()+
  geom_line()+
  geom_point(data=g1, size=5)+
  xlim(1,13) + ylim(250, 350) +
  ggtitle("강남구의 월별 교통사고 발생건수")
project4

project5 <- ggplot(data = 교통사고_강남구, aes(x=월, y=건수))+
  geom_line()+
  geom_point(data=g1, shape=15)+
  xlim(1,13) + ylim(250, 350)+
  ggtitle("강남구의 월별 교통사고 발생건수")
project5

project6 <- ggplot(data = 교통사고_강남구, aes(x=월, y=건수))+
  geom_line()+
  geom_point(data=g1, color="red")+
  xlim(1,13) + ylim(250, 350)+
  ggtitle("강남구의 월별 교통사고 발생건수")
project6

부상자수_6월 <- filter(traffics, 월=="6", 유형=="부상자수")
print(부상자수_6월)
project7 <- ggplot(data=부상자수_6월,
                   aes(x=자치구, y=건수,
                       fill=ifeles(자치구=="강남구", "Highlighted", "Normal")))
  geom_bar(stat="identity")+
  coord_cartesian()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.75)) +
  ggtitle("서울 6월 자치구별 교통사고 부상자수")
project7 

project3 <- ggplot(data = 교통사고_5월, aes(x=자치구, y=건수))+
  geom_bar(stat = "identity", col="blue", fill= "yellow")+
  coord_cartesian()+
  theme(axis.text.x = element_text(angle=90, vjust=0))+
  ggtitle("5월 자치구별 교통사고 발생 건수")