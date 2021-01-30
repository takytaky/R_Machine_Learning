# Random Forest - Ensemble ####
# 의사결정나무의 앙상블 모델
# 여러개의 나무를 학습 후 종합하는 모델
# 이상치나 편향된 분포에 민감하지 않음
# install.packages("randomForest")

library(randomForest)



# Heart.csv - AHD(심장병 유무 정보)

HEART <- read.csv("[mlData]/Heart.csv")

str(HEART)

table(HEART$AHD)



# Train Data(70%)

set.seed(2045)


HRT_TR <- sample(1:nrow(HEART), nrow(HEART) * 0.7)


ncol(HEART)

# Modeling
# Number of trees: 500
# No. of variables tried at each split: 3

Model_En <- randomForest(AHD ~ ., 
                         HEART[HRT_TR,],
                         importance = TRUE)

structure(Model_En)



# 각 변수의 중요도(importance = TRUE)
# MeanDecreaseAccuracy:분류정확도 개선에 기여한 변수의 수치
# MeanDecreaseGini:노드 불순도(불확실성) 개선에 기여한 변수의 수치
# Ca와 ChestPain이 가장 크게 기여

Model_En$importance

varImpPlot(Model_En)



# Validation Data에 모델 적용

Pred_Model_En <- predict(Model_En, HEART[-HRT_TR,])

table(Pred_Model_En)


# Model Performance

library(caret)

confusionMatrix(Pred_Model_En, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")



# Hyper_Parameter ####
# 학습전에 지정하는 파라미터
# summary(Model_rf)

# mtry :적용되는 변수의 개수
# ntree:추출되는 나무의 개수
# nodesize:마지막 노드에 지정된 데이터 개수



# mtry = 1

Model_rf <- randomForest(AHD ~ ., 
                         HEART[HRT_TR,],
                         mtry = 2, ntree = 1500)

Pred_Model_rf <- predict(Model_rf, HEART[-HRT_TR,])

confusionMatrix(Pred_Model_rf, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")



# mtry = 3

Model_rf <- randomForest(AHD ~ ., 
                         HEART[HRT_TR,],
                         mtry = 3)

Pred_Model_rf <- predict(Model_rf, HEART[-HRT_TR,])

confusionMatrix(Pred_Model_rf, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")



# mtry = 5

Model_rf <- randomForest(AHD ~ ., 
                         HEART[HRT_TR,],
                         mtry = 5)

Pred_Model_rf <- predict(Model_rf, HEART[-HRT_TR,])

confusionMatrix(Pred_Model_rf, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")



# mtry = 9

Model_rf <- randomForest(AHD ~ ., 
                         HEART[HRT_TR,],
                         mtry = 9)

Pred_Model_rf <- predict(Model_rf, HEART[-HRT_TR,])

confusionMatrix(Pred_Model_rf, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")







# ranger Package ####
# install.packages("ranger")

library(ranger)


Model_rg <- ranger(AHD ~ ., 
                   HEART[HRT_TR,])


Pred_Model_rg <- predict(Model_rg, 
                        HEART[-HRT_TR,])$predictions


confusionMatrix(Pred_Model_rg, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")



# mtry = 3

Model_rg <- ranger(AHD ~ ., 
                   HEART[HRT_TR,],
                   mtry = 3)



Pred_Model_rg <- predict(Model_rg, 
                         HEART[-HRT_TR,])$predictions



confusionMatrix(Pred_Model_rg, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")




# mtry=1, num.trees=1500, min.node.size = 3

Model_rg <- ranger(AHD ~ ., 
                   HEART[HRT_TR,],
                   mtry = 1,
                   num.trees = 1500,
                   min.node.size = 3)



Pred_Model_rg <- predict(Model_rg, 
                         HEART[-HRT_TR,])$predictions



confusionMatrix(Pred_Model_rg, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")












# Boosting Model ####

# Bagging :선택된 Tree간에 관계가 없음

# Boosting:이전 모델이 다음 모델에 영향을 줌
#          순차적(Sequential) 방법
#          이전 분류된 학습결과로 
#          다음 학습데이터의 샘플가중치를 조정


# Gradient Boosting Model
# install.packages("gbm")
# Hyperparameter
# n.trees:생성되는 나무의 개수
# shrinkage:Learning Rate

library(gbm)



# One-Hot Encoding

HT <- HEART


HT$AHD <- ifelse(HT$AHD == "Yes", 1, 0)




# Validation Approach
# n.trees = 1000,
# shrinkage = 0.01

Model_gbm <- gbm(formula = AHD ~ ., 
                 data = HT[HRT_TR,],
                 n.trees = 1000,
                 shrinkage = 0.01)


structure(Model_gbm)


summary(Model_gbm)



Pred_Model_gbm <- predict(Model_gbm, 
                         HT[-HRT_TR,],
                         n.trees = 1000)


Pred_Model_gbm <- ifelse(Pred_Model_gbm < 0.5, 0, 1)


confusionMatrix(factor(Pred_Model_gbm), factor(HT[-HRT_TR,]$AHD), 
                positive = "1",
                mode = "prec_recall")


# Cross Validation
# cv.folds = 10,
# n.trees = 700,
# shrinkage = 0.01

Model_gbm <- gbm(formula = AHD ~ ., 
                 data = HT[HRT_TR,],
                 cv.folds = 10,
                 n.trees = 700,
                 shrinkage = 0.01)


structure(Model_gbm)


summary(Model_gbm)




Pred_Model_gbm <- predict(Model_gbm, 
                          HT[-HRT_TR,],
                          n.trees = 700)


Pred_Model_gbm <- ifelse(Pred_Model_gbm < 0.5, 0, 1)


confusionMatrix(factor(Pred_Model_gbm), factor(HT[-HRT_TR,]$AHD), 
                positive = "1",
                mode = "prec_recall")









# 개별실습 ####
# Credit.csv - default(연체유무)
# Train:Test(7:3)

CRDT <- read.csv("[mlData]/Credit.csv")

str(CRDT)

head(CRDT)

table(CRDT$default)









# The End ####