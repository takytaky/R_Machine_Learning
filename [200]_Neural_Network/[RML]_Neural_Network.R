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





# nnet Package ####
# install.packages("nnet")
# install.packages("NeuralNetTools")

library(nnet)
library(NeuralNetTools)




# Neural Network Modeling

# size = 1 : 은닉층 노드 1개

Model_nn_1 <- nnet(Species ~ . -Sepal.Length, Iris_TR, size = 1)
plotnet(Model_nn_1)

structure(Model_nn_1)

summary(Model_nn_1)

Pred_Spec_1 <- predict(Model_nn_1, Iris_TE, type = "class")

table(Iris_TE$Species, Pred_Spec_1)






# size = 3 : 은닉층 노드 3개

Model_nn_3 <- nnet(Species ~ . -Sepal.Length, Iris_TR, size = 3)
plotnet(Model_nn_3)

structure(Model_nn_3)

summary(Model_nn_3)

Pred_Spec_3 <- predict(Model_nn_3, Iris_TE, type = "class")

table(Iris_TE$Species, Pred_Spec_3)








# The End ####