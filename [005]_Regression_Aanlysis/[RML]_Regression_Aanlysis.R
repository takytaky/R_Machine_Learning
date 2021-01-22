# 단일회귀분석 ####

# (문제) 습도가 53일때 불량품수량 예측

# (과거) 습도

x <- sort(round(runif(100, 33, 88), 3))



# (과거) 불량품수량

y <- sort(round(runif(100, 0, 10), 3))



# 난수 z 생성(평균:0, 표준편차:1)

z <- rnorm(100, mean = 0, sd = 2)



# x와 y에 z값으로 노이즈 적용

x <- x + z
y <- y + z




# 점으로 그래프에 표시

plot(x, y, pch = 19)







# 공분산 ####

cov(x, y)


cov(x, y) / (sd(x) * sd(y))




# 상관계수 ####
# pearson 상관계수

cor(x, y)



# spearman 상관계수

cor(x, y, method = "spearman")



# kedall tau

cor(x, y, method = "kendall")




# 상관계수 통계검정

cor.test(x, y)







# 탐색적 데이터 분석(EDA) ####

# 추세선 확인

scatter.smooth(x, y)



# 이상치 및 정규분포 형태 확인

par(mfrow = c(2, 2))
boxplot(x, main = "습도")
boxplot(y, main = "불량품")

plot(density(x), main = "습도")
plot(density(y), main = "불량품")
par(mfrow = c(1, 1))



# 왜도(치우친정도) 확인

# install.packages("e1071")
library(e1071)

skewness(x)
skewness(y)













# 회귀 Modeling ####

Model_lm <- lm(y ~ x)







# w와 b 회귀계수값 확인

structure(Model_lm)
coef(Model_lm)






# 회귀선 그리기

plot(x, y, pch = 19)
abline(Model_lm, col="red", lwd = 3)






# 회귀모델 평가기준
# Call : 모델설명
# Residuals : 관측된 잔차
# Coefficients : 설명변수(계수) 유의성
# R-squared : 결정 계수
# Adjusted R-squared : 조정 결정 계수(회귀모델의 설명력)
# F-statistic : F 통계량(모델이 통계적으로 의미가 있는가)

summary(Model_lm)











# 잔차분석 ####

# 모형의 선형성

plot(Model_lm, 1)



# 잔차의 정규성

plot(Model_lm, 2)
shapiro.test(Model_lm$residuals)



# 잔차의 등분산성

plot(Model_lm, 3)



# 극단값(이상치)

plot(Model_lm, 4)






# 회귀 Model 적용 ####

# (예측) 습도가 53일때 불량품수량?
# y_hat = wx + b
# w와 b의 값을 넣어 불량품수량 y_hat값을 예측

# 회귀계수 확인

w <- Model_lm$coefficients[["x"]]
b <- Model_lm$coefficients[["(Intercept)"]]




# 예측(회귀식)

y_hat = w * 53 + b



# 예측(함수)

predict(Model_lm, 
        newdata = data.frame(x = 53))



# 예측점 시각화

plot(x, y, pch = 19)
abline(Model_lm, col="red", lwd = 3)
points(x, fitted(Model_lm), col = "blue", cex = 1.5)



# 신뢰구간 - fit : lwr : upr
# 신뢰대(Confidence Band)

predict(Model_lm, 
        newdata = data.frame(x = 53), 
        interval = "confidence")



# 결과 시각화
# install.packages("ggplot2")

library(ggplot2)

ggplot(data.frame(x, y), aes(x,y)) + 
  geom_point() +
  stat_smooth(method = "lm")








# 다중회귀분석 ####

# (문제) 제품의 강도는 생산과정의 온도와 시간에 영향을 받음

# (과거) 강도

강도 <- c(82.8, 119.4, 105.5, 154.3, 139.5, 65.6, 88.9, 114.1)



# (과거) 온도

온도 <- c(199, 181, 203, 207, 202, 188, 211, 210)



# (과거) 시간

시간 <- c(59, 62, 58, 63, 60, 55, 57, 62)



# lm() 함수를 사용하여 회귀계수 계산

Model_mr <- lm(강도 ~ 온도 + 시간)



# 회귀계수값 확인

coef(Model_mr)



w1 <- Model_mr$coefficients[["온도"]]
w2 <- Model_mr$coefficients[["시간"]]
b <- Model_mr$coefficients[["(Intercept)"]]



# (예측) 온도(199), 시간(62)일때 강도(y_hat2) 예측

y_hat2 <- w1 * 199 + w2 * 62 + b



# (예측) predict() 함수

predict(Model_mr, 
        newdata = data.frame("온도" = 199, "시간" = 62), 
        interval = "confidence")



# 선형회귀모델 평가기준

summary(Model_mr)



# 다중회귀분석 시각화
# install.packages("rgl")

library(rgl)

y <- (강도)
x1 <- (온도)
x2 <- (시간)

plot3d(y ~ x1 + x2)










# 다중공선성(Mutilcolinearity) ####
# 독립변수들 간의 강한 상관관계로 인하여 발생 
# 회귀분석의 결과를 신뢰할 수 없는 현상

# 분산팽창요인(VIF: Variation Inflation Factor)
# VIF 값이 10이상인 경우 다중공선성 문제 의심

# install.packages("car")

library(car)

vif(Model_mr)








# 다중공선성 문제해결 ####

iris <- read.csv("[mlData]/Iris.csv")

head(iris)





# Modeling - 1

Model_VIF <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris)



# 다중공선성 이슈 발생

vif(Model_VIF)



# 독립변수들 간의 상관계수 확인
# Petal.Length와 Petal.Width 변수의 강한상관관계 확인

cor(iris[, 2:4])




# Petal.Width 독립변수 제거 후 Modeling

Model_VIF_A <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, iris)



# VIF값 10이하

vif(Model_VIF_A)



# Petal.Length 독립변수 제거 후 Modeling

Model_VIF_B <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, iris)



# VIF값 10이하

vif(Model_VIF_B)



# Sepal.Width 독립변수 제거 후 Modeling

Model_VIF_C <- lm(Sepal.Length ~ Petal.Length + Petal.Width, iris)



# VIF값 10이상

vif(Model_VIF_C)







# 다중회귀분석 시각화 ####
# install.packages("plotly")
# install.packages("reshape2")

library(plotly)
library(reshape2)

petal_lm <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, iris)



# Graph Resolution(more important for more complex shapes)

graph_reso <- 0.05



# Setup Axis

axis_x <- seq(min(iris$Sepal.Length), 
              max(iris$Sepal.Length), 
              by = graph_reso)

axis_y <- seq(min(iris$Sepal.Width), 
              max(iris$Sepal.Width), 
              by = graph_reso)



# Sample points

petal_lm_surface <- expand.grid(Sepal.Length = axis_x,
                                Sepal.Width = axis_y,
                                KEEP.OUT.ATTRS = FALSE)

petal_lm_surface$Petal.Length <- predict.lm(petal_lm, 
                                            newdata = petal_lm_surface)

petal_lm_surface <- acast(petal_lm_surface, 
                          Sepal.Width ~ Sepal.Length, 
                          value.var = "Petal.Length")



hcolors=c("red","blue","green")[iris$Species]

iris_plot <- plot_ly(iris, 
                     x = ~Sepal.Length, 
                     y = ~Sepal.Width, 
                     z = ~Petal.Length,
                     text = ~Species, 
                     type = "scatter3d", 
                     mode = "markers",
                     marker = list(color = hcolors))


iris_plot <- add_trace(p = iris_plot,
                       z = petal_lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface")

iris_plot

















# 의료비 예측 ####

Insure_Data <- read.csv("[mlData]/insurance.csv")

head(Insure_Data)





# 분류를 위한 Index 설정

set.seed(2045)

Train_Index <- sample(1:nrow(Insure_Data), nrow(Insure_Data) * 0.7)





# Train_Data와 Test_Data 나누기

Train_Data <- Insure_Data[Train_Index, ]
Test_Data <- Insure_Data[-Train_Index, ]






# Test_Data의 "expenses" 값 분리

Test_Expenses <- Test_Data$expenses

Test_Data <- Test_Data[, -7]





# 시각화를 통한 데이터 탐색

# 의료비 분포

ggplot(Train_Data, aes(x = 0, y = expenses)) +
  geom_boxplot()



# 성별에 따른 의료비 분포

ggplot(Train_Data, aes(x = sex, y = expenses)) +
  geom_boxplot(aes(fill = sex))



# 자녀에 따른 의료비 분포

ggplot(Train_Data, aes(x = factor(children), y = expenses)) +
  geom_boxplot(aes(fill = factor(children)))



# 흡연유무에 따른 의료비 분포

ggplot(Train_Data, aes(x = smoker, y = expenses)) +
  geom_boxplot(aes(fill = smoker))



# 거주지역에 따른 의료비 분포

ggplot(Train_Data, aes(x = region, y = expenses)) +
  geom_boxplot(aes(fill = region))






# 초기 Model_1 생성

Model_1 <- lm(expenses ~ ., Train_Data)

Pred_Expenses_1 <- predict(Model_1, Test_Data)





# MSE 값 확인

(MSE1 <- sqrt(mean((Test_Expenses - Pred_Expenses_1)^2)))







# Model_2 - 수정

Model_2 <- lm(expenses ~ bmi + smoker + children + sex, Train_Data)

Pred_Expenses_2 <- predict(Model_2, Test_Data)

(MSE2 <- sqrt(mean((Test_Expenses - Pred_Expenses_2)^2)))



# Model_3 - 수정

Model_3 <- lm(expenses ~ bmi + smoker + age, Train_Data)

Pred_Expenses_3 <- predict(Model_3, Test_Data)

(MSE3 <- sqrt(mean((Test_Expenses - Pred_Expenses_3)^2)))



# Model_4 - 수정

Model_4 <- lm(expenses ~ . -region, Train_Data)

Pred_Expenses_4 <- predict(Model_4, Test_Data)

(MSE4 <- sqrt(mean((Test_Expenses - Pred_Expenses_4)^2)))



# Model_5 - 수정

Model_5 <- lm(expenses ~ bmi + smoker + age + children, Train_Data)

Pred_Expenses_5 <- predict(Model_5, Test_Data)

(MSE5 <- sqrt(mean((Test_Expenses - Pred_Expenses_5)^2)))



# Model_6 - 수정

Model_6 <- lm(expenses ~ bmi + smoker + age + children + region, Train_Data)

Pred_Expenses_6 <- predict(Model_6, Test_Data)

(MSE6 <- sqrt(mean((Test_Expenses - Pred_Expenses_6)^2)))



# Model 비교

MSE1 ; MSE2 ; MSE3 ; MSE4 ; MSE5 ; MSE6







# The End ####