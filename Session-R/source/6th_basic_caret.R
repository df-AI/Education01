## caret package 장점
# 약 250개의 모델 작성 문법을 일원화
# 자동으로 병렬 처리
# 문법이 쉬움

## load packages
pkgs <- c("caret", 
          "tidyverse", 
          "doParallel", 
          "DT", 
          "corrplot")

sapply(pkgs, require, character.only = T)


## for parallel processing
detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)

## load data
url <- "https://assets.datacamp.com/production/repositories/718/datasets/7805fceacfb205470c0e8800d4ffc37c6944b30c/loans.csv"

loans <- read_csv(url) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(outcome = factor(default, levels = c(0, 1), labels = c("repaid", "default"))) %>% 
  select(-default, -keep, -rand)

glimpse(loans)

## missing values, duplicates
sapply(loans, function(x) sum(is.na(x)))
loans %>% duplicated() %>% sum()

## upsampling & downsampling
loans2 <- downSample(x = loans %>% select(-outcome), 
           y = loans$outcome, 
           yname = "outcome")

## split data : a stratified random split 
set.seed(2018)
# loans3 <- sample_n(loans2, 1000)
inx   <- createDataPartition(loans2$outcome, p = 0.7, list = F)
train <- loans2[ inx, ]
test  <- loans2[-inx, ]

#### sample test data ####
good_credit <- data.frame(
  loan_amount        = "LOW", 
  emp_length         = "10+ years", 
  home_ownership     = "MORTGAGE", 
  income             = "HIGH", 
  loan_purpose       = "major_purchase", 
  debt_to_income     = "AVERAGE", 
  credit_score       = "HIGH", 
  recent_inquiry     = "NO", 
  delinquent         = "NEVER", 
  credit_accounts    = "MANY", 
  bad_public_record  = "NO", 
  credit_utilization = "LOW", 
  past_bankrupt      = "NO", 
  outcome            = "repaid"
)

bad_credit <- data.frame(
  loan_amount        = "LOW", 
  emp_length         = "< 2 years", 
  home_ownership     = "RENT", 
  income             = "LOW", 
  loan_purpose       = "car", 
  debt_to_income     = "LOW", 
  credit_score       = "LOW", 
  recent_inquiry     = "YES", 
  delinquent         = "NEVER", 
  credit_accounts    = "FEW", 
  bad_public_record  = "NO", 
  credit_utilization = "LOW", 
  past_bankrupt      = "YES", 
  outcome            = "default"
)

## 10 fold CV : 왜 해야 하나요? 그리고 언제 해야 하나요?
## cross validation
myFolds <- createFolds(train[, "outcome"], k = 5)
control <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds)

## preprocess data
# BoxCox : 데이터의 왜도 보정
# center & scale : 표준화
# spatialSign : outlier 보정
# corr : 공선성 데이터 제거
# nzv : near zero variance

## preprocess
preProc <- c("BoxCox", 
             "center",
             "scale",
             "spatialSign",
             "corr",
             "zv")

## logistic regression
set.seed(2018)
modelLookup("glm")

system.time(
  glm.model <- train(outcome ~ ., 
                     data = train, 
                     method     = "glm",
                     metric     = "ROC",
                     trControl  = control,
                     preProcess = preProc
                    )
)
predict(glm.model, newdata = test, type = "raw")
predict(glm.model, good_credit, type = "raw")
predict(glm.model, bad_credit, type = "raw")

## single tree : rpart
modelLookup("rpart")

rpartGrid <- expand.grid(cp = c(0.001, 0.01, 0.1, 0))

set.seed(2018)
system.time(
  rpart.model <- train(
    outcome ~., 
    data = train,
    method     = "rpart",
    metric     = "ROC",
    trControl  = control,
    preProcess = preProc,
    tuneGrid   = rpartGrid)
)

rpart.model
ggplot(rpart.model)

# Make a prediction for someone with good credit
predict(rpart.model, newdata = test, type = "raw")
predict(rpart.model, good_credit, type = "raw")
predict(rpart.model, bad_credit, type = "raw")

## multiple trees : random forest
modelLookup("rf")
rftGrid <- expand.grid(mtry = c(3, 7, 14)) # sqrt(p)
set.seed(2018)

system.time(
  rft.model <- train(
    outcome ~ ., 
    data = train,
    method     = "rf",
    metric     = "ROC",
    trControl  = control,
    preProcess = preProc,
    tuneGrid   = rftGrid,
    ntree      = 500)
)

rft.model
ggplot(rft.model)

# Make a prediction for someone with good credit
predict(rft.model, newdata = test, type = "raw")
predict(rft.model, good_credit, type = "raw")
predict(rft.model, bad_credit, type = "raw")


## GBM
modelLookup("gbm")

gbmGrid <- expand.grid(n.trees = 500,
                       interaction.depth = 30,
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 30)

set.seed(2018)

system.time(
  gbm.model <- train(
    outcome ~ ., 
    data = train,
    method     = "gbm",
    metric     = "ROC",
    trControl  = control,
    preProcess = preProc,
    tuneGrid   = gbmGrid,
    verbose    = F)
)

gbm.model
ggplot(gbm.model)

predict(gbm.model, newdata = test, type = "raw")
predict(gbm.model, good_credit, type = "raw")
predict(gbm.model, bad_credit, type = "raw")

## compare models 
resamps <- resamples(
  list(glm.model = glm.model, 
       rpart.model = rpart.model, 
       rft.model = rft.model, 
       gbm.model = gbm.model)
)

summary(resamps)

## 그래프 비교
bwplot(resamps, metric = "ROC")
xyplot(resamps, metric = "ROC")

difValues <- diff(resamps)
summary(difValues)
bwplot(difValues, layout = c(3, 1))
dotplot(difValues)

## predict test data
pred_glm <- predict(glm.model, test)
confusionMatrix(pred_glm, test$outcome, positive = "repaid")

pred_rpt <- predict(rpart.model, test)
confusionMatrix(pred_rpt, test$outcome, positive = "repaid")

pred_rft <- predict(rft.model, test)
confusionMatrix(pred_rft, test$outcome, positive = "repaid")

pred_gbm <- predict(gbm.model, test)
confusionMatrix(pred_gbm, test$outcome, positive = "repaid")

