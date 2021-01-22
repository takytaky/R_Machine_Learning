# K-means ####

set.seed(2045)


# 100 x 2 Matrix : N(0,1) 난수 생성

MX_1 <- matrix(rnorm(200, mean = 0, sd = 1), 100, 2)
plot(MX_1, pch = 19, col = 1:4)


# 4 x 2 Matrix : N(0,4) 난수 생성

set.seed(2045)

MX_2 <- matrix(rnorm(8, mean = 0, sd = 4), 4, 2)
plot(MX_2, pch = 19, col = 1:4)



# 4개 그룹 Index 생성

set.seed(2045)

MX_Index <- sample(1:4, 100, replace = TRUE)
MX_Index




# x + xmean

MX_1 <- MX_1 + MX_2[MX_Index, ]



# 4개 그룹별 시각화

plot(MX_1, col = MX_Index , pch = 19)



# 4개의 Cluster로 군집

Model_KM <- kmeans(MX_1, centers = 4, nstart = 15)

structure(Model_KM)



# 결과 시각화

plot(MX_1, col = Model_KM$cluster, cex = 4, pch = 1, lwd = 2)

points(MX_1, col = MX_Index, pch = 19)















# k-means(iris Data) ####

str(iris)
head(iris)






# 데이터프레임 재지정

Iris_Data <- iris

head(Iris_Data)





# Species 정보 삭제

Iris_Data$Species <- NULL

head(Iris_Data)





# kmeans() 적용(k = 3)

Iris_Data_km <- kmeans(Iris_Data, 3, nstart = 15)


structure(Iris_Data_km)




# 데이터포인트가 어느 군집에 속하는지 출력

Iris_Data_km$cluster



# 각 군집별 데이터포인트(개체) 숫자

Iris_Data_km$size



# 군집분석 수행 반복 횟수

Iris_Data_km$iter



# 각 군집의 중심 위치

Iris_Data_km$centers



# 군집 별 개체간 거리의 제곱합

Iris_Data_km$withinss



# 전체 군집 내 개체간 거리의 제곱합
# 작아야함(각 군집 내 개체들 간 거리 최소화)

Iris_Data_km$tot.withinss



# 군집과 군집 간 중심의 거리 제곱합
# 커야함(각 군집들 간 거리 최대화)

Iris_Data_km$betweenss



# 군집분석 결과 비교(일반적으로 불가능)
# table(iris$Species, Iris_Data_km$cluster)




# 군집분석 결과 시각화(군집 & 중심점)

plot(Iris_Data[c(1, 2)], 
     col = Iris_Data_km$cluster, pch = 19)

points(Iris_Data_km$centers[,c(1, 2)], 
       col = 1:3, pch = "*", cex = 5)



# 최적 군집 개수 구하기
# 군집의 개수를 증가하며 tot.withinss 비교

KMS <- c()

for(i in 1:9){
  Model_result <- kmeans(Iris_Data, i, nstart = 15)
  KMS[i] <- Model_result$tot.withinss
}


plot(1:9, KMS, type = "b", cex = 2,
     col = "tomato",
     xlab = "Number of Groups",
     ylab = "Within Groups Sum of Squars")















# kproto() ####
# 범주형 데이터 처리 가능

# clusterMixType Package
# install.packages("clustMixType")

library(clustMixType)



# Orange Data:범주형 데이터(color) 포함

Orange <- read.csv("[mlData]/Orange.csv")

str(Orange)

head(Orange)



# species 정보 제거

Orange_KM <- Orange[, -1]

str(Orange_KM)

head(Orange_KM)



# Modeling
# k = 2 : 두개의 그룹으로 군집
# 내부적으로 표준화처리 지원

Model_cmt <- kproto(x = Orange_KM, k = 2, nstart = 15)


structure(Model_cmt)


summary(Model_cmt)


str(Model_cmt)






# 전체 군집 내 개체간 거리의 제곱합
# 작아야함(각 군집 내 개체들 간 거리 최소화)

Model_cmt$tot.withinss






# 1 ~ 10개의 군집의 편차제곱합 비교

WSS <- c()


for(i in 1:10){
  Model_result <- kproto(Orange_KM, i, nstart = 15)
  WSS[i] <- Model_result$tot.withinss
}


plot(WSS, type = "b", col = "tomato")








# 5개 군집으로 다시 Modeling

Model_cmt <- kproto(x = Orange_KM, k = 5, nstart = 15)

Model_cmt$cluster





# 5개 군집 결과

table(Model_cmt$cluster)







# Visualization
# Console 창에서 엔터입력하여 확인

par(mfrow = c(2,2))

clprofiles(Model_cmt, Orange_KM)

par(mfrow = c(1,1))












# The End ####