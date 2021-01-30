# Decision Tree

# Orange Data ####
# install.packages("ggplot2")

library(ggplot2)




# Orange Data File 읽어오기

Orange <- read.csv("[mlData]/Orange.csv")

str(Orange)

head(Orange)



# 탐색적 데이터 분석

# weight ~ species

ggplot(Orange, aes(x = species, y = weight)) +
  geom_boxplot(aes(fill = species),
               outlier.color = "red",
               outlier.shape = "*",
               outlier.size = 20)



# sugar ~ species

ggplot(Orange, aes(x = species, y = sugar)) +
  geom_boxplot(aes(fill = species),
               outlier.color = "red",
               outlier.shape = "*",
               outlier.size = 20)



# acid ~ species

ggplot(Orange, aes(x = species, y = acid)) +
  geom_boxplot(aes(fill = species),
               outlier.color = "red",
               outlier.shape = "*",
               outlier.size = 20)



# color ~ species

ggplot(Orange, aes(factor(color))) +
  geom_bar(aes(fill = species))




# species

ggplot(Orange, aes(species)) + 
  geom_bar(aes(fill = species))

table(Orange$species)




# Train:Test - (8:2)

set.seed(2045)



# 80% : Training Data Index

TR_8 <- sample(1:nrow(Orange), nrow(Orange) * 0.8)











# tree Package - tree() ####
# install.packages("tree")

library(tree)



# Modeling - tree()
# Entropy Index : 데이터 집합의 혼잡도 기준
# 이진 재귀 분할법(Binary Recursive Partitioning)

Model_tr <- tree(species ~ ., Orange[TR_8, ]) 

Pred_Model_tr <- predict(Model_tr, Orange[-TR_8,])

structure(Model_tr)




# Tree 시각화

plot(Model_tr)
text(Model_tr)
















# rpart Package - rpart() ####
# CART(Classification And Regression Tree)
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)



# Modeling - rpart()
# minsplit : 분류 시 포함할 최소 데이터의 개수
# 분순도(Impurity) 기반의 분리(splitting) 실행

Model_rp <- rpart(species ~ .,
                  Orange[TR_8,], 
                  control = rpart.control(minsplit = 2))



# Model Structure

structure(Model_rp)







# Model Visualization
# 범례, 실제 Train Data 비율, 조건식 포함

rpart.plot(Model_rp)



# 20% : Validation Data - Classification
# type = "class" - 분류 결과를 레이블로 출력
# type = "prob"  - 분류 결과를 확률로 출력

Pred_Model_rp <- predict(Model_rp,
                         Orange[-TR_8,],
                         type = "class")






# Performance Check
# caret Package - confusionMatrix()
# Accuracy, Cohen's Kappa
# Precision, Recall(Sensitivity), Specificity
# Accuracy    : 전체 정확도
# Kappa       : 평가자간 일치도(0 ~ 1 사이값)
# Precision   : T로 예측한것 중 실제 T의 비율
# Recall      : 실제 T 중 모델이 T로 맞춘 비율
# Specificity : 실제 F 중 모델이 F로 맞춘 비율

# install.packages("caret")
# install.packages("e1071")

library(caret)

confusionMatrix(Pred_Model_rp, Orange[-TR_8,]$species)




# CP(Complexity Parameter)
# 단순한모델 : 직관적이지만 성능이 낮음
# 복잡한모델 : 이해가 어렵고 계산 시간이 길어짐
# 너무 복잡한모델은 훈련데이터에 최적화(과적합)되는 문제 발생
# CP : 노드를 분리하는데 필요한 복잡성(Complexity/Cost)
# 최대CP값 도달 시 더 이상 분류작업을 진행하지 않음
# 최적을 CP 설정으로 범용적 모델 생성 가능



Model_rp$cptable









# Pruning ####

# CP값을 0.13으로 지정

Model_rp_p <- prune(Model_rp,cp = 0.13)


rpart.plot(Model_rp_p)


Pred_Model_rp_p <- predict(Model_rp_p,
                           Orange[-TR_8,],
                           type = "class")


confusionMatrix(Pred_Model_rp_p, Orange[-TR_8,]$species)




Model_rp_p$cptable









# party Package - ctree() ####
# install.packages("party")

library(party)



# Conditional Inference Tree

# 순열 검정 기반 비편향 재귀 분할법
# Unbiased Recursive Partitioning based on Permutation Test

# p검정 기준으로 가지치기 변수를 결정
# 편향의 위험이 적음(단, 입력변수가 31개로 제한됨)






# Heart.csv - AHD(심장병 유무 정보)

HEART <- read.csv("[mlData]/Heart.csv")

str(HEART)

table(HEART$AHD)



# Train Data(70%)

set.seed(2045)


HRT_TR <- sample(1:nrow(HEART), nrow(HEART) * 0.7)




# Modeling

Model_ct <- ctree(AHD ~ ., HEART[HRT_TR,])

structure(Model_ct)





# Visualization

plot(Model_ct, type = "simple")

plot(Model_ct)





# Test Data에 모델 적용

Pred_Model_ct <- predict(Model_ct, HEART[-HRT_TR,])


table(Pred_Model_ct)




# Model Performance

confusionMatrix(Pred_Model_ct, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")





# rpart()_V2 ####

library(rpart)
library(rpart.plot)


Model_rp2 <- rpart(AHD ~ ., HEART[HRT_TR,], 
                   method = "class")


structure(Model_rp2)


rpart.plot(Model_rp2)


Pred_Model_rp2 <- predict(Model_rp2,
                          HEART[-HRT_TR,],
                          type = "class")

# Model Performance

confusionMatrix(Pred_Model_rp2, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")

printcp(Model_rp2)



# Pruning_V2
# CP값을 0.05으로 지정

Model_rp2_p <- prune(Model_rp2, cp = 0.05)

rpart.plot(Model_rp2_p)

Pred_Model_rp2_p <- predict(Model_rp2_p,
                            HEART[-HRT_TR,],
                            type = "class")

confusionMatrix(Pred_Model_rp2, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")






# tree()_V2 ####

library(tree)

Model_tr2 <- tree(AHD ~ ., HEART[HRT_TR,])

plot(Model_tr2)
text(Model_tr2)


# Pruning_3
# tree 사이즈 별 분산 확인
# 가장 분산이 작은 사이즈 선택

tr2_cv <- cv.tree(Model_tr2, FUN = prune.misclass)

plot(tr2_cv)



Model_tr2_p <- prune.misclass(Model_tr2, best = 6)

plot(Model_tr2_p)
text(Model_tr2_p)



Pred_Model_tr2_p <- predict(Model_tr2_p,
                            HEART[-HRT_TR,],
                            type = "class")

confusionMatrix(Pred_Model_tr2_p, HEART[-HRT_TR,]$AHD, 
                positive = "Yes",
                mode = "prec_recall")






# The End ####