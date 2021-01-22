# Model Capacity 그래프 #### 

DATA <- read.csv("[mlData]/testData.csv")

str(DATA)
plot(DATA, cex = 0.5)




# lm() 1차함수

Model_1 <- lm(outputs ~ inputs, DATA)
structure(Model_1)

plot(DATA, pch = 1, cex = 0.5)
abline(Model_1, lwd = 3, col = "red")



# lm() 2차함수

Model_2 <- lm(outputs ~ poly(inputs, 2), DATA)
structure(Model_2)

a <- seq(0, 1, length = 1000)
points(a, predict(Model_2, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "blue")


# lm() 5차함수

Model_5 <- lm(outputs ~ poly(inputs, 5), DATA)
structure(Model_5)

points(a, predict(Model_5, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "green")



# lm() 9차함수

Model_9 <- lm(outputs ~ poly(inputs, 9), DATA)
structure(Model_9)

points(a, predict(Model_9, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "yellow")



# 비교 그래프 작성

par(mfrow = c(2,2))

plot(DATA, pch = 1, cex = 0.5)
abline(Model_1, lwd = 3, col = "red")

plot(DATA, pch = 1, cex = 0.5)
a <- seq(0, 1, length = 1000)
points(a, predict(Model_2, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "blue")

plot(DATA, pch = 1, cex = 0.5)
points(a, predict(Model_5, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "green")

plot(DATA, pch = 1, cex = 0.5)
points(a, predict(Model_9, data.frame(inputs = a)),
       type = "l", lwd = 3, col = "yellow")

par(mfrow = c(1,1))



















# Training Error ####

Elec <- read.csv("[mlData]/Electric.csv")
str(Elec)

plot(Elec$electricity ~ Elec$surface_area, cex = 1.5)




# 1차함수

Model_1 <- lm(electricity ~ surface_area, Elec)
structure(Model_1)



# 1차함수 : Capacity 부족

abline(Model_1, col = "red", lwd = 3)





# 1차함수 적용(예측값)

y_hat_1 <- predict(Model_1, Elec)
y_hat_1[1:50]



# 실제 전기사용량(y)

y <- Elec$electricity
y[1:50]




# Training Error - 1차함수
# mean((실제값 - 예측값)^2)
# 모든 오차제곱의 평균구하기

Train_Err_1 <- mean((y - y_hat_1)^2)
Train_Err_1














# 2차함수

Model_2 <- lm(electricity ~ poly(surface_area, 2), Elec)
structure(Model_2)






# 2차함수 적용(예측값)

y_hat_2 <- predict(Model_2, Elec)
y_hat_2[1:50]






# Training Error - 2차함수

Train_Err_2 <- mean((y - y_hat_2)^2)



# 1차함수 vs. 2차함수

Train_Err_1 ; Train_Err_2











# 5차함수

Model_5 <- lm(electricity ~ poly(surface_area, 5), Elec)
structure(Model_5)



# 5차함수 적용(예측값)

y_hat_5 <- predict(Model_5, Elec)
y_hat_5[1:50]



# Training Error - 5차함수

Train_Err_5 <- mean((y - y_hat_5)^2)



# 1차함수 vs. 2차함수 vs. 5차함수

Train_Err_1 ; Train_Err_2 ; Train_Err_5







# 9차함수

Model_9 <- lm(electricity ~ poly(surface_area, 9), Elec)
structure(Model_9)



# 9차함수 적용(예측값)

y_hat_9 <- predict(Model_9, Elec)
y_hat_9[1:50]



# Training Error - 9차함수

Train_Err_9 <- mean((y - y_hat_9)^2)



# 1차함수 vs. 2차함수 vs. 5차함수 vs. 9차함수

Train_Err_1 ; Train_Err_2 ; Train_Err_5 ; Train_Err_9







# 그래프 출력

plot(Elec$surface_area, Elec$electricity, cex = 2)



# 1차함수

abline(Model_1, lwd = 5, col = "red")



# 2차함수

a <- seq(500, 820, length = 1000)
points(a, predict(Model_2, data.frame(surface_area = a)),
       type = "l", lwd = 5, col = "blue")



# 5차함수

points(a, predict(Model_5, data.frame(surface_area = a)), 
       type = "l", lwd = 5, col = "green")



# 9차함수

points(a, predict(Model_9, data.frame(surface_area = a)), 
       type = "l", lwd = 5, col = "yellow")



















# Test Error ####
# Training Data와 Testing Data를 7:3(537:231)으로 분리
# str(Elec) vs. str(Elec[trainIndex,]) vs. str(Elec[-trainIndex,])

Elec <- read.csv("[mlData]/Electric.csv")





# 매번 동일한 값을 추출하기 위해 설정

set.seed(2045)



# Training Data(70%) : Index

train_Index <- sample(1:nrow(Elec), nrow(Elec) * 0.7)



# Testing Data(30%) : 실제값(y)

y_test <- Elec$electricity[-train_Index]





# 1차함수
# Training Data로 Model_1 생성

Model_1 <- lm(electricity ~ surface_area, Elec[train_Index,])



# Testing Data에 1차함수 적용(예측값)

y_hat_1 <- predict(Model_1, Elec[-train_Index,])



# Testing Error - 1차함수

Test_Err_1 <- mean((y_test - y_hat_1)^2)
Test_Err_1













# 5차함수
# Training Data로 Model_5 생성

Model_5 <- lm(electricity ~ poly(surface_area, 5), Elec[train_Index,])



# Testing Data에 5차함수 적용(예측값)

y_hat_5 <- predict(Model_5, Elec[-train_Index,])



# Testing Error - 5차함수

Test_Err_5 <- mean((y_test - y_hat_5)^2)
Test_Err_5













# 9차함수
# Training Data로 Model_9 생성

Model_9 <- lm(electricity ~ poly(surface_area, 9), Elec[train_Index,])



# Testing Data에 9차함수 적용(예측값)

y_hat_9 <- predict(Model_9, Elec[-train_Index,])



# Testing Error - 9차함수

Test_Err_9 <- mean((y_test - y_hat_9)^2)
Test_Err_9



# Model_1 vs. Model_5 vs. Model_9

Test_Err_1 ; Test_Err_5 ; Test_Err_9

















# Validation Error ####
# Training Data : Validation Data : Testing Data

Elec <- read.csv("[mlData]/Electric.csv")
plot(Elec$electricity ~ Elec$surface_area, cex = 1.5)





# Training Data : Validation Data : Testing Data(6:2:2) 데이터 분할
# 460:153:155

# Training Data(60%) : Index

set.seed(2045)
train_IDX <- sample(1:nrow(Elec), nrow(Elec) * 0.6)



# Validation Data(20%) : Index

set.seed(2045)
valid_IDX <- sample(setdiff(1:nrow(Elec), train_IDX), nrow(Elec) * 0.2)



# Testing Data(20%) : Index

test_IDX <- setdiff(1:nrow(Elec), c(train_IDX, valid_IDX))










# 1차함수

Model_1 <- lm(electricity ~ surface_area, Elec[train_IDX,])
Pred_W_1 <- predict(Model_1, Elec[valid_IDX,])
Valid_Err_1 <- mean((Elec$electricity[valid_IDX] - Pred_W_1)^2)
Valid_Err_1




# 5차함수

Model_5 <- lm(electricity ~ poly(surface_area, 5), Elec[train_Index,])
Pred_W_5 <- predict(Model_5, Elec[valid_IDX,])
Valid_Err_5 <- mean((Elec$electricity[valid_IDX] - Pred_W_5)^2)
Valid_Err_5




# 9차함수

Model_9 <- lm(electricity ~ poly(surface_area, 9), Elec[train_Index,])
Pred_W_9 <- predict(Model_9, Elec[valid_IDX,])
Valid_Err_9 <- mean((Elec$electricity[valid_IDX] - Pred_W_9)^2)
Valid_Err_9




# 평가결과 비교 및 Testing Data 적용모델 선택
# Model_1 vs. Model_5 vs. Model_9

Valid_Err_1 ; Valid_Err_5 ; Valid_Err_9






# Testing Data 적용
# Model_9 적용하여 Generalization Error 추정

Test_Pred <- predict(Model_9, Elec[test_IDX,])
Test_Err <- mean((Elec$electricity[test_IDX] - Test_Pred)^2)





# Valid_Err vs. Test_Err
# 하지만 우연히 에러가 낮은 Validation Data가 추출되었다면?

Valid_Err_9 ; Test_Err






# 개별실습자료 ####

Wage <- read.csv("[mlData]/Wage.csv")

str(Wage)

plot(Wage$wage ~ Wage$age)













# CV with Packages ####
# 01 - K-Fold Cross-Validaton
# 02 - LOOCV(Leave-One-Out Cross-Validation)

CARS <- read.csv("[mlData]/Cars.csv")
str(CARS)
plot(CARS$mpg ~ CARS$horsepower, cex = 1.5)



# Train : Test (7:3)

set.seed(2045)
TR <- sample(1:nrow(CARS), nrow(CARS) * 0.7)
TRD <- CARS[TR,]
TED <- CARS[-TR,]
nrow(TRD) ; nrow(TED)



# glm() : Generalized Linear Models
# 선형1차 모델

Model_CV <- glm(mpg ~ horsepower, data = TRD)
Model_CV



# cv.glm() 적용을 위하여 호출

library(boot)



# K-Fold Cross-Validaton

# glm()으로 생성한 모델에 
# cv.glm()로 Cross Validation을 적용
# K = 5(5-Fold) / K 대문자!!!

CV_ERR <- cv.glm(TRD, Model_CV, K = 5)



# Cross-Validaton 결과 확인
# 1st : prediction Error
# 2nd : adjusted Cross-Validation estimate(bias)

CV_ERR$delta



# 1차~5차 모델 생성 후 결과 비교

Degree <- 1:5



# 5-Fold Cross-Validation(K = 5)

CV_ERR_5 <- c()

for(d in Degree){
  Model_5CV <- glm(mpg ~ poly(horsepower, d), data = TRD)
  CV_ERR_5[d] <- cv.glm(TRD, Model_5CV, K = 5)$delta[1]
}

CV_ERR_5




# 결과 시각화

plot(CV_ERR_5 ~ Degree, type = "b", col = "red")





# 10-Fold Cross-Validation(K = 10)

CV_ERR_10 <- c()



for(d in Degree){
  Model_10CV <- glm(mpg ~ poly(horsepower, d), data = TRD)
  CV_ERR_10[d] <- cv.glm(TRD, Model_10CV, K = 10)$delta[1]
}

CV_ERR_10



# 결과 시각화

plot(CV_ERR_10 ~ Degree, type = "b", col = "blue")












# LOOCV(Leave-One-Out Cross-Validation)
# Data Point의 개수만큼 Model을 생성
# Model을 생성 시 한개의 Test Data를 제외
# 전체 결과의 평균을 적용

CV_ERR_L <- c()




# 1차부터 5차까지 모델적용

for(d in Degree){
  Model_LCV <- glm(mpg ~ poly(horsepower, d), data = TRD)
  CV_ERR_L[d] <- cv.glm(TRD, Model_LCV)$delta[1]
}

CV_ERR_L



# 결과 시각화

plot(CV_ERR_L ~ Degree, type = "b", col = "seagreen")






# 5-Fold CV vs. 10-Fold CV vs. LOOCV
# 결과 시각화 비교

plot(CV_ERR_5 ~ Degree, 
     type = "b", col = "red", ylab = "CV Errors", lwd = 2)

lines(CV_ERR_10 ~ Degree, type = "b", col = "blue", lwd = 2)

lines(CV_ERR_L ~ Degree, type = "b", col = "seagreen", lwd = 2)

legend("topright", pch = 1,
       c("5-fold CV", "10-fold CV", "LOOCV"),
       col = c("red", "blue", "seagreen"))







# The End ####