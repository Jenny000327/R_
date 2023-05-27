# install.packages("randomForest")
# install.packages("cluster")

library(cluster)
library(randomForest)
library(dplyr)

# 1. 클러스터링 ----
## 1.1 분석 ----
# 데이터셋 불러오기
#setwd("c:/data/raw")
#dir()

df <- read_csv("C:/data/preprocessed/data_final.csv") %>% as_tibble() %>% select(-...1)
df

df_cluster <- df %>% dplyr::select(MK_NUM,SMK_NUM,DT,IN,OUT,PARKING)

# 적절한 k의 범위 설정
k_values <- 1:8

# 응집도 저장할 변수 초기화
inertias <- numeric(length(k_values))

# 각 k에 대한 응집도 계산
for (k in k_values) {
  kmeans_result <- kmeans(df_cluster, centers = k)
  inertias[k] <- kmeans_result$tot.withinss
}

# 응집도 그래프 그리기
plot(k_values, inertias, type = "b", pch = 20, frame = FALSE, xlab = "Number of clusters (k)", ylab = "Total within-cluster sum of squares")

# 군집 개수 설정
k <- 4

# k = 4 k-means 클러스터링 수행
kmeans_result <- kmeans(df_cluster, centers = k)

# 클러스터 할당 결과 확인
cluster_labels <- kmeans_result$cluster
print(cluster_labels)

# 클러스터 중심 확인
cluster_centers <- kmeans_result$centers
print(cluster_centers)

# 데이터셋에 클러스터 정보 추가
df <- df %>% mutate(cluster = as.factor(cluster_labels))



## 1.2 시각화 ----

# 2. 모델링 ----
# 훈련 데이터와 테스트 데이터로 분할
set.seed(123) # 재현성을 위한 시드 설정

train_indices <- sample(1:nrow(df), nrow(df) * 0.7) # 70%를 훈련 데이터로 사용
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# 랜덤 포레스트 & 회귀분석 모델 훈련
rf1_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)
rf2_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)
rg1_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)
rg2_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)

# 테스트 데이터 예측
predictions_rf <- predict(rf1_model, newdata = test_data)
predictions_rg <- predict(rg1_model, newdata = test_data)

test_data$predictions_rf <- predictions_rf
test_data$predictions_rg <- predictions_rg

# 예측 결과 평가 ---- DT / DT, IN, OUT / DT, IN, OUT, PARKING 
MSE_rf = mean((test_data$predictions_rf - test_data$CLS_RATE)^2)
MSE_rg = mean((test_data$predictions_rg - test_data$CLS_RATE)^2)

100*MSE_rf %>% round(10)
100*MSE_rg %>% round(10)




