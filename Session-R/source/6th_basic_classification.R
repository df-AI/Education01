library(dplyr)

#### 1. 데이터 가져오기 ####
if(!file.exists("wdbc.data")) {
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data > wdbc.data') 
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data > wdbc.names')
}

data <- tbl_df(read.table("wdbc.data", 
                          strip.white = TRUE, 
                          sep = ",", 
                          header = FALSE))

feature_names <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dim")

names(data) <- c("id", "class", 
                 paste0("mean_", feature_names), 
                 paste0("se_", feature_names), 
                 paste0("worst_", feature_names))

glimpse(data)

#### 2. 기초통계 ####
summary(data)

#### 3.1. 데이터 전처리, 변수 제거 ####
data <- data %>% dplyr::select(-id)

#### 3.2. class 변수를 인자 변환 ####
data$class <- factor(ifelse(data$class == "B", 0, 1))
glimpse(data)

#### 4. 시각화 ####
library(dplyr)
library(ggplot2)
library(gridExtra)

p1 <- data %>% ggplot(aes(class)) + geom_bar()
p2 <- data %>% ggplot(aes(class, mean_concave_points)) + 
  geom_jitter(col = "gray") + 
  geom_boxplot(alpha = .5)
p3 <- data %>% ggplot(aes(class, mean_radius)) + 
  geom_jitter(col = "gray") + 
  geom_boxplot(alpha = .5)
p4 <- data %>% ggplot(aes(mean_concave_points, mean_radius)) + 
  geom_jitter(col = "gray") + 
  geom_smooth()
grid.arrange(p1, p2, p3, p4)

#### 5. 결측치 및 중복 데이터 ####
sapply(data, function(x) sum(is.na(x)))
data %>% duplicated() %>% sum()

prop.table(table(data$class))

#### 6. 업 샘플링 ####
library(caret)
## upsampling & downsampling
data2 <- upSample(x = data %>% dplyr::select(-class), 
                     y = data$class, 
                     yname = "class")

#### 7. 데이터 분리 ####
## split data : a stratified random split 
set.seed(2018)
inx   <- createDataPartition(data2$class, p = 0.7, list = F)
train <- data2[ inx, ]
test  <- data2[-inx, ]

#### 8. 모델링 - 로지스틱 회귀분석 ####
# 모형
glm.model <- glm(class ~ ., data = train, family = "binomial")

# 예측
predict(glm.model, newdata = data[1:5, ], type = "response")

# 모형 평가
library(ROCR)
y_obs <- as.numeric(as.character(test$class))
yhat_glm <- predict(glm.model, newdata = test, type = "response")
pred_glm <- prediction(yhat_glm, y_obs)
(auc_glm <- performance(pred_glm, "auc")@y.values[[1]])

#### 9. 모델링 - 나무모형 ####
library(rpart)
library(rpart.plot)

# 모형
rpart.model <- rpart(class ~., data = train)

# 그래프
rpart.plot(rpart.model)

# 예측
predict(rpart.model, newdata = data[1:5, ], type = "class")

# 모형 평가
library(ROCR)
y_obs <- as.numeric(as.character(test$class))
yhat_rpart <- predict(rpart.model, newdata = test)[, "1"]
pred_rpart <- prediction(yhat_rpart, y_obs)
(auc_rpart <- performance(pred_rpart, "auc")@y.values[[1]])

#### 10. 모델링 - 랜덤포레스트 ####
library(randomForest)
# 모형
rft.model <- randomForest(class ~., data = train)

# 그래프
plot(rft.model)
varImpPlot(rft.model)

# 예측
predict(rpart.model, newdata = data[1:5, ], type = "prob")[, "1"]

# 모형 평가
library(ROCR)
y_obs <- as.numeric(as.character(test$class))
yhat_rft <- predict(rft.model, newdata = test, type = "prob")[, "1"]
pred_rft <- prediction(yhat_rft, y_obs)
(auc_rft <- performance(pred_rft, "auc")@y.values[[1]])

#### 11. 모델링 - 부스팅 ####

library(gbm)
# 모형
train$class <- as.numeric(as.character(train$class))
gbm.model <- gbm(class ~., data = train, distribution = "bernoulli", n.trees = 20000, cv.folds = 3, verbose = TRUE)

# 그래프
gbm.perf(gbm.model, method = "cv")

# 예측
predict(gbm.model, newdata = data[1:5, ], type = "response")

# 모형 평가
library(ROCR)
y_obs <- as.numeric(as.character(test$class))
yhat_gbm <- predict(gbm.model, newdata = test, type = "response")
pred_gbm <- prediction(yhat_gbm, y_obs)
(auc_gbm <- performance(pred_gbm, "auc")@y.values[[1]])

#### 12. 최종모형 선택 과정 ####
# 정확성 검사
data.frame(method = c("glm", "rpart", "randomforest", "gbm"), 
           auc = c(auc_glm, auc_rpart, auc_rft, auc_gbm))

# ROC Curve
perf_glm <- performance(pred_glm, measure = "tpr", x.measure = "fpr")
perf_rpart <- performance(pred_rpart, measure = "tpr", x.measure = "fpr")
perf_rft <- performance(pred_rft, measure = "tpr", x.measure = "fpr")
perf_gbm <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")

plot(perf_glm, col = "black", main = "ROC Curve")
plot(perf_rpart, add = TRUE, col = "green")
plot(perf_rft, add = TRUE, col = "blue")
plot(perf_gbm, add = TRUE, col = "red")
abline(0,1)
legend("bottomright", inset = .1, 
       legend = c("glm", "rpart", "randomforest", "gbm"), 
       col = c("black", "green", "blue", "red"), lty = 1, lwd = 2)


