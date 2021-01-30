# iris Data ####

iris

str(iris)

head(iris)



# Train Data:Test Data(7:3)

set.seed(2045)

IDX_TR <- sample(1:nrow(iris), nrow(iris) * 0.7)



Iris_TR <- iris[IDX_TR, ]
dim(Iris_TR)



Iris_TE <- iris[-IDX_TR, ]
dim(Iris_TE)





# K_Nearest_Neighbers ####

library(class)

Model_knn <- knn(Iris_TR[, -5], 
                 Iris_TE[, -5], 
                 Iris_TR$Species, 
                 k = 1)


summary(Model_knn)


table(Iris_TE$Species, Model_knn)






# 최적값 탐색 ####

score_board <- c(1:60)



# k : 1 ~ 60 탐색

for(i in 1:60){
  Model_knn <- knn(Iris_TR[, -5], 
                   Iris_TE[, -5], 
                   Iris_TR$Species, 
                   k = i)
  true_value <- sum(Model_knn == Iris_TE$Species)
  true_rate  <- true_value / length(Iris_TE$Species)
  score_board[i] <- true_rate
}



# 실행결과 시각화

DF <- as.data.frame(score_board)


library(ggplot2)

index <- 1:60
ggplot(data = DF,
       aes(x = index, y = score_board)) +
  geom_line(color = 'tomato', size = 1)









# The End ####