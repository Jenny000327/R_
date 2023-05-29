# install.packages("randomForest")
# install.packages("cluster")

library(cluster)
library(randomForest)
library(dplyr)
library(readr)

# 1. 클러스터링 ----
## 1.1 분석 ----
# 데이터셋 불러오기
#setwd("c:/data/raw")
#dir()

df <- read_csv("C:/data/preprocessed/data_final.csv") %>% as_tibble() %>% select(-...1)
#df <- read_csv("/Users/gayeongkim/Desktop/data/prepared/data_final.csv") %>% as_tibble() %>% select(-...1)
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
# 1. 독립변수 전부 다(CLS_RATE, CLS_MK_NUM 제외)
rf1_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + OP_RATE
                          + DT + IN + OUT + PARKING + cluster, data = df)
# 2. 마켓 수 변수들만
rf2_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + cluster, data = df)
# 3. 따로 찾은 독립변수들만
rf3_model <- randomForest(CLS_RATE ~ DT + IN + OUT + PARKING + cluster, data = df)
# 4. try
rf4_model <- randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df)


rg1_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + OP_RATE
                + DT + IN + OUT + PARKING + cluster, data = df)
rg2_model <- lm(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + cluster, data = df)
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

# 모델 성능 평가 ; 라이브러리 이용
library(Metrics)

# rf1
rf1_mse <- mse(test_data$predictions_rf1,test_data$CLS_RATE)
rf1_mae <- mae(test_data$predictions_rf1,test_data$CLS_RATE)
rf1_mape <- mape(test_data$predictions_rf1,test_data$CLS_RATE)
rf1_rmse <- rmse(test_data$predictions_rf1,test_data$CLS_RATE)
# rf2
rf2_mse <- mse(test_data$predictions_rf2,test_data$CLS_RATE)
rf2_mae <- mae(test_data$predictions_rf2,test_data$CLS_RATE)
rf2_mape <- mape(test_data$predictions_rf2,test_data$CLS_RATE)
rf2_rmse <- rmse(test_data$predictions_rf2,test_data$CLS_RATE)
# rf3
rf3_mse <- mse(test_data$predictions_rf3,test_data$CLS_RATE)
rf3_mae <- mae(test_data$predictions_rf3,test_data$CLS_RATE)
rf3_mape <- mape(test_data$predictions_rf3,test_data$CLS_RATE)
rf3_rmse <- rmse(test_data$predictions_rf3,test_data$CLS_RATE)
# rf4
rf4_mse <- mse(test_data$predictions_rf4,test_data$CLS_RATE)
rf4_mae <- mae(test_data$predictions_rf4,test_data$CLS_RATE)
rf4_mape <- mape(test_data$predictions_rf4,test_data$CLS_RATE)
rf4_rmse <- rmse(test_data$predictions_rf4,test_data$CLS_RATE)

# rg1
rg1_mse <- mse(test_data$predictions_rg1,test_data$CLS_RATE)
rg1_mae <- mae(test_data$predictions_rg1,test_data$CLS_RATE)
rg1_mape <- mape(test_data$predictions_rg1,test_data$CLS_RATE)
rg1_rmse <- rmse(test_data$predictions_rg1,test_data$CLS_RATE)
# rg2
rg2_mse <- mse(test_data$predictions_rg2,test_data$CLS_RATE)
rg2_mae <- mae(test_data$predictions_rg2,test_data$CLS_RATE)
rg2_mape <- mape(test_data$predictions_rg2,test_data$CLS_RATE)
rg2_rmse <- rmse(test_data$predictions_rg2,test_data$CLS_RATE)
# rg3
rg3_mse <- mse(test_data$predictions_rg3,test_data$CLS_RATE)
rg3_mae <- mae(test_data$predictions_rg3,test_data$CLS_RATE)
rg3_mape <- mape(test_data$predictions_rg3,test_data$CLS_RATE)
rg3_rmse <- rmse(test_data$predictions_rg3,test_data$CLS_RATE)
# rg4
rg4_mse <- mse(test_data$predictions_rg4,test_data$CLS_RATE)
rg4_mae <- mae(test_data$predictions_rg4,test_data$CLS_RATE)
rg4_mape <- mape(test_data$predictions_rg4,test_data$CLS_RATE)
rg4_rmse <- rmse(test_data$predictions_rg4,test_data$CLS_RATE)

# 모델 성능 평가 ; 그래프
par(mfrow=c(2,2))
model_mse=c(rf1_mse, rf2_mse, rf3_mse, rf4_mse, rg1_mse, rg2_mse, rg3_mse, rg4_mse)
model_mae=c(rf1_mae, rf2_mae, rf3_mae, rf4_mae, rg1_mae, rg2_mae, rg3_mae, rg4_mae)
model_mape=c(rf1_mape, rf2_mape, rf3_mape, rf4_mape, rg1_mape, rg2_mape, rg3_mape, rg4_mape)
model_rmse=c(rf1_rmse, rf2_rmse, rf3_rmse, rf4_rmse, rg1_rmse, rg2_rmse, rg3_rmse, rg4_rmse)

#install.packages('colorspace')
library(colorspace)
hcl_palettes(plot = TRUE)
clr <- qualitative_hcl(8, palette = "Set 3")

barplot(model_mse,names=c('rf1_mse', 'rf2_mse', 'rf3_mse', 'rf4_mse', 'rg1_mse', 'rg2_mse', 'rg3_mse', 'rg4_mse'), las=2, col=clr)
title(main="mse")

barplot(model_mae,names=c('rf1_mae', 'rf2_mae', 'rf3_mae', 'rf4_mae', 'rg1_mae', 'rg2_mae', 'rg3_mae', 'rg4_mae'), las=2, col=clr)
title(main="mae")

barplot(model_mape,names=c('rf1_mape', 'rf2_mape', 'rf3_mape', 'rf4_mape', 'rg1_mape', 'rg2_mape', 'rg3_mape', 'rg4_mape'), las=2, col=clr)
title(main="mape")

barplot(model_rmse,names=c('rf1_rmse', 'rf2_rmse', 'rf3_rmse', 'rf4_rmse', 'rg1_rmse', 'rg2_rmse', 'rg3_rmse', 'rg4_rmse'), las=2, col=clr)
title(main="rmse")