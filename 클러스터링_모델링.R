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

#df <- read_csv("C:/data/preprocessed/data_final.csv") %>% as_tibble() %>% select(-...1)
df <- read_csv("/Users/gayeongkim/Desktop/data/prepared/data_final.csv") %>% as_tibble() %>% select(-...1)
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
k <- 5

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
rf1_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + CLS_MK_NUM + FRC_MK_NUM + OP_RATE
                          + DT + IN + OUT + PARKING + cluster, data = df)
rf2_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + CLS_MK_NUM + FRC_MK_NUM + cluster, data = df)
rf3_model <- randomForest(CLS_RATE ~ DT + IN + OUT + PARKING + cluster, data = df)
rf4_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)


rg1_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)
rg2_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + CLS_MK_NUM + FRC_MK_NUM + cluster, data = df)
rg3_model <- lm(CLS_RATE ~ DT + IN + OUT + PARKING + cluster, data = df)
rg4_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)


#그래프 그리기(참값 vs. 예측값)
par(mfrow=c(2,2))

plot(df$CLS_RATE)
lines(rg1_model$fitted.values, col = "red")

plot(df$CLS_RATE)
lines(rg2_model$fitted.values, col = "blue")

plot(df$CLS_RATE)
lines(rg2_model$fitted.values, col = "green")

plot(df$CLS_RATE)
lines(rg2_model$fitted.values, col = "yellow")


# 테스트 데이터 예측
predictions_rf1<- predict(rf1_model, newdata = test_data)
predictions_rf2<- predict(rf2_model, newdata = test_data)
predictions_rf3<- predict(rf3_model, newdata = test_data)
predictions_rf4<- predict(rf4_model, newdata = test_data)

predictions_rg1 <- predict(rg1_model, newdata = test_data)
predictions_rg2<- predict(rg2_model, newdata = test_data)
predictions_rg3<- predict(rg3_model, newdata = test_data)
predictions_rg4<- predict(rg4_model, newdata = test_data)

test_data$predictions_rf1 <- predictions_rf1
test_data$predictions_rf2 <- predictions_rf2
test_data$predictions_rf3 <- predictions_rf3
test_data$predictions_rf4 <- predictions_rf4

test_data$predictions_rg1 <- predictions_rg1
test_data$predictions_rg2 <- predictions_rg2
test_data$predictions_rg3 <- predictions_rg3
test_data$predictions_rg4 <- predictions_rg4

# 예측 결과 평가 ---- DT / DT, IN, OUT / DT, IN, OUT, PARKING 
MSE_rf1 = mean((test_data$predictions_rf1 - test_data$CLS_RATE)^2)
MSE_rf2 = mean((test_data$predictions_rf2 - test_data$CLS_RATE)^2)
MSE_rf3 = mean((test_data$predictions_rf3 - test_data$CLS_RATE)^2)
MSE_rf4 = mean((test_data$predictions_rf4 - test_data$CLS_RATE)^2)

MSE_rg1 = mean((test_data$predictions_rg1 - test_data$CLS_RATE)^2)
MSE_rg2 = mean((test_data$predictions_rg2 - test_data$CLS_RATE)^2)
MSE_rg3 = mean((test_data$predictions_rg3 - test_data$CLS_RATE)^2)
MSE_rg4 = mean((test_data$predictions_rg4 - test_data$CLS_RATE)^2)

100*MSE_rf1 %>% round(10)
100*MSE_rf2%>% round(10)
100*MSE_rf3 %>% round(10)
100*MSE_rf4 %>% round(10)

100*MSE_rg1 %>% round(10)
100*MSE_rg2%>% round(10)
100*MSE_rg3 %>% round(10)
100*MSE_rg4 %>% round(10)

# 라이브러리 이용
library(Metrics)

# rf1
mse(test_data$predictions_rf1,test_data$CLS_RATE)
mae(test_data$predictions_rf1,test_data$CLS_RATE)
mape(test_data$predictions_rf1,test_data$CLS_RATE)
rmse(test_data$predictions_rf1,test_data$CLS_RATE)
# rf2
mse(test_data$predictions_rf2,test_data$CLS_RATE)
mae(test_data$predictions_rf2,test_data$CLS_RATE)
mape(test_data$predictions_rf2,test_data$CLS_RATE)
rmse(test_data$predictions_rf2,test_data$CLS_RATE)
# rf3
mse(test_data$predictions_rf3,test_data$CLS_RATE)
mae(test_data$predictions_rf3,test_data$CLS_RATE)
mape(test_data$predictions_rf3,test_data$CLS_RATE)
rmse(test_data$predictions_rf3,test_data$CLS_RATE)
# rf4
mse(test_data$predictions_rf4,test_data$CLS_RATE)
mae(test_data$predictions_rf4,test_data$CLS_RATE)
mape(test_data$predictions_rf4,test_data$CLS_RATE)
rmse(test_data$predictions_rf4,test_data$CLS_RATE)

# rg1
mse(test_data$predictions_rg1,test_data$CLS_RATE)
mae(test_data$predictions_rg1,test_data$CLS_RATE)
mape(test_data$predictions_rg1,test_data$CLS_RATE)
rmse(test_data$predictions_rg1,test_data$CLS_RATE)
# rg2
mse(test_data$predictions_rg2,test_data$CLS_RATE)
mae(test_data$predictions_rg2,test_data$CLS_RATE)
mape(test_data$predictions_rg2,test_data$CLS_RATE)
rmse(test_data$predictions_rg2,test_data$CLS_RATE)
# rg3
mse(test_data$predictions_rg3,test_data$CLS_RATE)
mae(test_data$predictions_rg3,test_data$CLS_RATE)
mape(test_data$predictions_rg3,test_data$CLS_RATE)
rmse(test_data$predictions_rg3,test_data$CLS_RATE)
# rg4
mse(test_data$predictions_rg4,test_data$CLS_RATE)
mae(test_data$predictions_rg4,test_data$CLS_RATE)
mape(test_data$predictions_rg4,test_data$CLS_RATE)
rmse(test_data$predictions_rg4,test_data$CLS_RATE)
