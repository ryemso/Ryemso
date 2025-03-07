# 필요한 패키지 로드
library(tidyverse)
library(ggplot2)
library(stats)
library(dplyr)
library(readxl)
library(class)
library(gmodels)
library(caret)

# 1. 데이터 로드
salary <- read_excel("salary_data.xlsx")

# 2. 데이터 전처리 (범주형 변수 변환)
salary$gender <- ifelse(salary$Gender == "Male", 1, 0)  # Male = 1, Female = 0
salary$degree <- ifelse(salary$`Education Level` == "PhD", 13,
                        ifelse(salary$`Education Level` == "Master's", 12,
                               ifelse(salary$`Education Level` == "Bachelor's", 11, 10)))
salary$exp <- salary$`Years of Experience`  # 경험 변수명 변경

# 3. 결측값 처리
salary <- na.omit(salary)  # 모든 NA 값 제거
print(dim(salary))  # 데이터 크기 확인

# 4. 정규화 함수 정의
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 5. 연봉 등급 생성 (Target 변수)
salary$rank <- ifelse(salary$Salary >= 160000, '1st',
                      ifelse(salary$Salary >= 115000, '2nd',
                             ifelse(salary$Salary >= 70000, '3rd', '4th')))
salary$rank <- as.factor(salary$rank)  # 범주형 변수 변환

# 6. 분석에 필요한 변수 선택
salary_AI <- select(salary, Age, exp, gender, degree, Salary, rank)

# 7. 정규화 적용 (X 데이터만)
salary_AI_n <- as.data.frame(lapply(salary_AI[1:5], normalize))

# 8. 데이터셋 분할 (train 70% / test 30%)
set.seed(123)  # 재현성을 위해 설정
train_idx <- sample(1:nrow(salary_AI_n), size = 0.7 * nrow(salary_AI_n))

salary_train <- salary_AI_n[train_idx, ]
salary_test <- salary_AI_n[-train_idx, ]
salary_train_labels <- salary$rank[train_idx]
salary_test_labels <- salary$rank[-train_idx]

# 9. 데이터 크기 확인 및 오류 방지
print(dim(salary_train))
print(dim(salary_test))
print(length(salary_train_labels))
print(length(salary_test_labels))

# train_labels 길이 맞추기
salary_train_labels <- salary_train_labels[1:nrow(salary_train)]

# 10. 최적의 k 찾기
train_control <- trainControl(method = "cv", number = 5)
knn_model <- train(salary_train, salary_train_labels,
                   method = "knn",
                   trControl = train_control,
                   tuneLength = 10)  # k 값 10개 자동 탐색
print(knn_model$bestTune)  # 최적의 k 확인

#  11. k-NN 모델 적용 (최적 k 사용)
best_k <- knn_model$bestTune$k  # 최적 k 값 적용
if (nrow(salary_train) > best_k) {  
  salary_test_pred <- knn(train = salary_train, test = salary_test, cl = salary_train_labels, k = best_k)
  print(salary_test_pred)
  
  # 12. 성능 평가
  CrossTable(salary_test_labels, salary_test_pred, prop.chisq = FALSE)
  
  # 혼동행렬 (Confusion Matrix) 계산
  print(confusionMatrix(salary_test_pred, salary_test_labels))
  
  # 13. 시각화
  # 필요한 패키지 추가
  library(reshape2)
  # 혼동 행렬 시각화 (Heatmap)
  confusion <- table(Actual = salary_test_labels, Predicted = salary_test_pred)
  confusion_melted <- melt(confusion)  # 데이터 변환
  
  ggplot(confusion_melted, aes(x = Actual, y = Predicted, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value), color = "white", size = 5) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
    theme_minimal()
  
  # 연봉 예측을 위한 특징 데이터 시각화
  ggplot(salary, aes(x = exp, y = Salary, color = rank)) +
    geom_point(alpha = 0.7) +
    labs(title = "Years of Experience vs Salary", x = "Years of Experience", y = "Salary") +
    theme_minimal()
  
  ggplot(salary, aes(x = degree, y = Salary, color = rank)) +
    geom_point(alpha = 0.7) +
    labs(title = "Education Level vs Salary", x = "Degree Level", y = "Salary") +
    theme_minimal()
  
  # 연봉 등급별 박스플롯
  ggplot(salary, aes(x = rank, y = Salary, fill = rank)) +
    geom_boxplot() +
    labs(title = "Salary Distribution by Rank", x = "Salary Rank", y = "Salary") +
    theme_minimal()
  
}

