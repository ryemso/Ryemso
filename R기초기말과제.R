library(tidyverse)
library(ggplot2)
library(stats)
library(dplyr)
library(readxl)
#군집분석, 다변량회귀분석, k-nn 까지 추후에 직접 테스트 데이터 만들기
salary <- read_excel("salary_data.xlsx")
head(salary)
#summary(salary) #6690개 데이터
salary$gender <- ifelse(salary$Gender =="Male",1,0) #male=1 female=0 na=1
#high school=10 Bachelor=11 Master=12 PhD=13
salary$degree <- ifelse(salary$`Education Level` == "PhD", 13,
                               ifelse(salary$`Education Level` == "Master's",12,
                                      ifelse(salary$`Education Level` == "Bachelor's",11,10)))
head(salary)
str(salary)
#drop_na(salary)

#salary$Salary <- ave(salary,salary$Salary,FUN=function(x) mean(x, na.rm=TRUE))
#salary_source <- c('age','Years of Experience','gender','dgree')
#salary$age<- ave(salary,salary$Age,FUN=function(x) mean(x, na.rm=TRUE))
#salary$exp <- ave(salary,salary$`Years of Experience`,FUN=function(x) mean(x, na.rm=TRUE))

salary$exp <- salary$`Years of Experience`

#salary$degree <- ave(salary,salary$degree,FUN=function(x) mean(x, na.rm=TRUE))

head(salary)
#is.na(salary$gender)
#head(salary)
#정규화 과정
Salary_scale <- scale(salary$Salary)
age_scale <- scale(salary$Age)
degree_scale <- scale(salary$degree)
exp_scale <- scale(salary$exp)


salary_lm <- lm(Salary~Age+exp+gender+degree,data=salary)
salary_lm
summary(salary_lm)

#정규화된 값들
scale_salary_lm1 <- lm(Salary_scale~age_scale+exp_scale+gender+degree_scale,data=salary)
scale_salary_lm1
summary(scale_salary_lm1)

#경력과 학력의 연봉관계
attach(salary)
names(salary)
plot(exp,Salary)
cor.test(exp,Salary)
cor.test(degree,Salary)
cor.test(Age,degree)

#산포도행렬
pairs(salary[c("Age","Salary","exp","degree","gender")])
library(psych)
library(colorspace)
pairs.panels(salary[c("Age","Salary","exp","degree","gender")],
             hist.col=qualitative_hcl(n=1,palette='Dark 3'),
                                       cex.cor=0.5,
                                       stars=TRUE)
#연봉예측모델 만들기
library(class)
library(gmodels)

summary(salary$Salary)
salary$rank <- ifelse(salary$Salary >= 160000,'1st',
                      ifelse(salary$Salary >= 115000,'2nd',
                             ifelse(salary$Salary >= 70000, '3th','4th')))
head(salary)
salary_AI <- select(salary, -2, -3,-4,-5)
head(salary_AI)
table(salary_AI$rank)
#is.na(salary)
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
} 
salary_AI_n <- as.data.frame(lapply(salary_AI[1:5],normalize))
#summary(salary_AI_n)
salary_complete <- salary_AI_n[complete.cases(salary_AI_n), ]
salary_train <- salary_complete[1:4500,]
salary_test <- salary_complete[4501:6690,]
head(salary_AI_n)
salary_train_labels <- salary_train$rank
salary_test_labels <- salary_test$rank
#salary_train_labels

# 결측치 확인
is.na(salary_test_labels)
is.na(salary_train_labels)
# 결측치 제거
salary_train_labels <- salary_train_labels[complete.cases(salary_train_labels)]
salary_train_labels <- as.factor(salary_train_labels)
#is.na(salary_train_labels)

salary_test <- salary_test[complete.cases(salary_test), ]
salary_train <- salary_train[1:length(salary_train_labels), ]


salary_train <- salary_train[complete.cases(salary_train), ]

# 다시 knn 모델 피팅
salary_test_pred <- knn(train = salary_train, test = salary_test, cl = salary_train_labels, k = 20)
salary_test_pred
CrossTable(salary_test_pred, salary_test_pred, prop.chisq = FALSE)

#카파값 확인
library(caret)
confusionMatrix(salary_test_pred, salary_test_pred, positive = "1st")
