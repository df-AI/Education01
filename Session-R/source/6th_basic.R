## caret package 장점
# 약 250개의 모델 작성 문법을 일원화
# 자동으로 병렬 처리
# 문법이 쉬움


## load packages
pkgs <- c("caret", "tidyverse", "doParallel", "DT")
sapply(pkgs, require, character.only = T)


## for parallel processing
detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)


## load data
data("GermanCredit")
loan <- GermanCredit

## missing values, duplicates
sapply(loan, function(x) sum(is.na(x)))
loan %>% duplicated() %>% sum()


## split data : a stratified random split 
set.seed(2018)
inx   <- createDataPartition(loan$Class, p = 0.7, list = F)
train <- loan[ inx, ]
test  <- loan[-inx, ]

## define x, y
my_y <- "Class"
my_x <- setdiff(names(train), my_y)

## 10 fold CV : 왜 해야 하나요? 그리고 언제 해야 하나요?
# Create Custom indices: myFolds
myFolds <- createFolds(train[ , my_y], k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  method  = "repeatedcv",
  number  = 10,
  repeats = 5,
  search  = "grid")

## preprocess data
# BoxCox : 데이터의 왜도 보정
# center & scale : 표준화
# spatialSign : outlier 보정
# corr : 공선성 데이터 제거
# nzv : near zero variance

prep <- c("BoxCox", "center", "scale", "spatialSign", "corr", "nzv")

## logistic regression
set.seed(2018)
modelLookup("glm")

glm.model <- train(Class ~ ., 
                   data = train,
                   method = "glm", 
                   family = "binomial",
                   preProcess = prep, 
                   trControl = myControl)

glm.model

## single tree : rpart
modelLookup("rpart")

rpartGrid <- expand.grid(cp = c(0.001, 0.01, 0.1, 0.5))

set.seed(2018)

rpart.model <- train(Class ~ ., data = train,
             method     = "rpart",
             trControl  = myControl,
             preProcess = prep,
             tuneGrid   = rpartGrid)

rpart.model
ggplot(rpart.model)


## multiple trees : random forest
modelLookup("rf")

rftGrid <- expand.grid(mtry = c(2, 3, 4, 8)) # p/3

set.seed(2018)

rft.model <- train(Class ~ ., data = train,
             method = "rf",
             trControl  = myControl,
             preProcess = prep,
             tuneGrid   = rftGrid,
             ntree      = 500)

rft.model
ggplot(rft.model)

# ada
## gbm
modelLookup("gbm")

gbmGrid <- expand.grid(
  n.trees = c(100, 500, 1000),
  interaction.depth = c(10, 30),  # maximum nodes per tree
  shrinkage = c(0.01, 0.1, 0.5),  # learning rate(0.01 ~ 1)
  n.minobsinnode = c(10, 30, 50)) # 터미널노드에 있는 최소 데이터 수

set.seed(2018)

gbm.model <- train(Class ~ ., data = train,
             method     = "gbm",
             trControl  = myControl,
             preProcess = prep,
             tuneGrid   = gbmGrid,
             verbose    = F)

gbm
ggplot(gbm)

## compare models 
resamps <- resamples(
  list(glm.model = glm.model, 
       rft.model = rft.model, 
       rft.model = rft.model, 
       gbm.model = gbm.model)
)

summary(resamps)

## 그래프 비교
bwplot(resamps, layout = c(3, 1))
dotplot(resamps, layout = c(3, 1))

difValues <- diff(resamps)
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)

## predict test data
pred_rf  <- predict(rft, test)
pred_gbm <- predict(gbm, test)
pred_snn <- predict(snn, test)

## 
