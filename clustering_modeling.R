#모델링 코드들 가영이가 한것대로 추가하구 정리만 했오!!
# Libraries
library(cluster)
library(randomForest)
library(dplyr)
library(readr)
library(Metrics)
library(ggplot2)
library(tidyr)
library(tibble)

# Load dataset
df <- read_csv("C:/data/preprocessed/data_final.csv") %>% 
  as_tibble() %>% select(-...1)

#클러스터링할 변수 선택 
#요인에 프랜차이즈 점포수 도 추가 해 봤어요 얘만 빠져있어가지구. 혹시 뺀 이유가 있다면 알려주세요.
df_cluster <- df %>% dplyr::select(MK_NUM,SMK_NUM,FRC_MK_NUM,DT,IN,OUT,PARKING)

# K-means Clustering
k_values <- 1:8
inertias <- numeric(length(k_values))

#k에 대한 응집도 계산
# K-means 클러스터링에서 응집도는 클러스터 내의 데이터 포인트들과 해당 클러스터의 중심(centroid) 간의 거리의 합으로 정의된다. 
# 응집도를 최소화하는 것이 K-means 알고리즘의 목표이며, 클러스터링 결과의 품질을 평가하는 지표 중 하나이다.
# 따라서, K-means 클러스터링에서 응집도는 클러스터링 결과의 품질을 평가하고 클러스터링 알고리즘의 수렴 상태를 확인하는데 중요한 지표.
# 응집도가 낮을수록 클러스터링 결과는 더 좋은 품질을 가지고 있을 가능성이 높다.
for (k in k_values) { 
  kmeans_result <- kmeans(df_cluster, centers = k)
  inertias[k] <- kmeans_result$tot.withinss
}

# 응집도 그래프
plot(k_values, inertias, type = "b", pch = 20, frame = FALSE, xlab = "Number of clusters (k)", ylab = "Total within-cluster sum of squares")

k <- 4 #그래프 결과 클러스터 갯수 4개로 설정
kmeans_result <- kmeans(df_cluster, centers = k)
cluster_labels <- kmeans_result$cluster
cluster_centers <- kmeans_result$centers

print(cluster_labels) # 클러스터 할당 결과 확인
print(cluster_centers) # 클러스터 중심 확인
#클러스터 중심은 K-means 클러스터링 알고리즘에서 각 클러스터의 대표점으로 사용되는 점.
# K-means 알고리즘은 초기에 무작위로 선택된 K개의 중심을 기반으로 클러스터를 형성한다.
#그런 다음, 알고리즘이 반복적으로 수행되면서 각 데이터 포인트를 가장 가까운 중심에 할당하고,
#클러스터의 중심을 다시 계산한다. 중심은 해당 클러스터 내의 모든 데이터 포인트의 평균 위치로 이동하게 된다.

df <- df %>% mutate(cluster = as.factor(cluster_labels))

# 데이터 분할
set.seed(123)#시드고정
train_indices <- sample(1:nrow(df), nrow(df) * 0.7) #데이터 비율을 7:3으로
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# 모델링
models <- list(
  "rf1" = randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + OP_RATE + DT + IN + OUT + PARKING + cluster, data = df), #독립변수 전부 다(CLS_RATE, CLS_MK_NUM 제외)
  "rf2" = randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + cluster, data = df), #마켓 수 변수들만
  "rf3" = randomForest(CLS_RATE ~ DT + IN + OUT + PARKING + cluster, data = df), # 따로 찾은 독립변수들만(점포수 제외)
  "rf4" = randomForest(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df), #다른 조합 시도(프렌차이즈 점포 수 제외)
  
  "rg1" = lm(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + OP_RATE + DT + IN + OUT + PARKING + cluster, data = df), #독립변수 전부 다(CLS_RATE, CLS_MK_NUM 제외)
  "rg2" = lm(CLS_RATE ~ MK_NUM + SMK_NUM + OP_MK_NUM + FRC_MK_NUM + cluster, data = df), #마켓 수 변수들만
  "rg3" = lm(CLS_RATE ~ DT + IN + OUT + PARKING + cluster, data = df), # 따로 찾은 독립변수들만(점포수 제외)
  "rg4" = lm(CLS_RATE ~ MK_NUM + SMK_NUM + DT + IN + OUT + PARKING + cluster, data = df) #다른 조합 시도(프렌차이즈 점포 수 제외)
)

# 예측
for(model in names(models)){
  test_data[paste("predictions", model, sep = "_")] <- predict(models[[model]], newdata = test_data)
}


# 모델 평가
eval_metrics <- list("mse", "mae", "mape", "rmse")
eval_results <- matrix(nrow = length(models), ncol = length(eval_metrics),
                       dimnames = list(names(models), eval_metrics))

for(model in names(models)){
  eval_results[model, "mse"] <- mse(test_data[[paste("predictions", model, sep = "_")]], test_data$CLS_RATE)
  eval_results[model, "mae"] <- mae(test_data[[paste("predictions", model, sep = "_")]], test_data$CLS_RATE)
  eval_results[model, "mape"] <- mape(test_data[[paste("predictions", model, sep = "_")]], test_data$CLS_RATE)
  eval_results[model, "rmse"] <- rmse(test_data[[paste("predictions", model, sep = "_")]], test_data$CLS_RATE)
}

# 데이터 프레임으로
eval_results_df <- as.data.frame(eval_results)

# 예측 결과 
print(eval_results_df)


#위의 4가지 평가 지표에 대한 설명 
# 1. mse (Mean Squared Error): 실제 값과 예측 값의 차이를 제곱해 평균한 값
#
# 2. mae (Mean Absolute Error): 실제 값과 예측 값의 차이를 절대값으로 취한 후 평균한 값
# 이 지표는 예측 오차의 크기를 직관적으로 이해할 수 있다.
# 
# 3.mape (Mean Absolute Percentage Error): 실제 값 대비 예측 오차의 비율을 평균한 값. 실제 값이 0에 가까울 때 큰 오차를 나타냄.
# 
# 4. rmse (Root Mean Squared Error): mse의 제곱근으로, 실제 값과 예측 값의 차이를 제곱해 평균한 후 제곱근을 취한 값, 큰 오차에 대해 더 큰 패널티를 부여하는 특성이 있다.
# 
# 네 가지 지표를 모두 사용하는 이유: 모델의 성능을 다양한 측면에서 평가하고, 그 특성에 따른 이점을 모두 활용하기 위해.
# mse, rmse:  큰 오차에 대해 더 큰 패널티를 부여하므로, 크게 틀린 예측을 줄이는 데 중점을 둔 모델을 선택하는데 도움이 된다.
# mae, mape:  예측 오차의 절대적인 크기나 비율을 직관적으로 보여주므로, 모델의 전반적인 성능을 이해하는 데 도움이 된다.


# 모델에 목적에 따라 중요하게 여기는 평가 지표가 달라짐. 
# 모델의 목적이 큰 폐업률 오차를 최대한 줄이는 것이라면, 큰 오차에 대해 더 많은 패널티를 주는 MSE나 RMSE를 중요하게 여길 수 있다.
# 이런 경우, 특정 동네에서 예상치 못한 큰 폐업률 증가가 발생했을 때, 모델이 그 변화를 잡아내지 못하면 심각한 문제가 될 수 있기 때문이다.
# 
# 모델의 목적이 전반적인 트렌드를 잘 파악하고 일반적으로 폐업률을 잘 예측하는 것이라면, MAE나 MAPE를 중요하게 여길 수 있다.
# 이런 경우, 모든 동네의 폐업률을 비슷한 정도로 잘 예측하는 것이 중요하며, 개별 예측의 오차보다는 전체적인 오차가 더 중요할 수 있다.


#시각화
# 평가 지표 막대그래프로 시각화
eval_results_long <- eval_results_df %>%
  rownames_to_column("model") %>%
  pivot_longer(cols = c("mse", "mae", "mape", "rmse"), 
               names_to = "metric", 
               values_to = "value")


ggplot(eval_results_long, aes(x = model, y = value, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free") +
  theme_minimal() +
  labs(x = "Model", y = "Value", fill = "Model",
       title = "Evaluation Metrics for Each Model",
       subtitle = "Lower is better for all metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 예측 결과 산점도 그래프로 시각화 actual vs. predicted
predictions_long <- test_data %>%
  pivot_longer(cols = starts_with("predictions_"), 
               names_to = "model", 
               values_to = "prediction")



ggplot(predictions_long, aes(x = CLS_RATE, y = prediction)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~model, scales = "free") +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted")



eval_results_df