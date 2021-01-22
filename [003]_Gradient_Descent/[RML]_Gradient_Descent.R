# Machine 함수 정의 ####
# x, w, b를 받아 예측값(y_hat)을 리턴하는 함수
# y = wx + b

Machine <- function(x, w, b) {

  y_hat <- (w * x) + b
  
	return(y_hat)
	}



# Machine 함수 사용 예시

x <- c(1, 3, 5, 7, 9)
w <- 2
b <- 1



# 실행(함수사용)

Machine(x, w, b) 






# Gradient 함수정의 ####
# x, y, w, b를 받아 Parameter의 Gradient를 출력하는 함수

Gradient <- function(x, y, w, b) {
	y_hat <- Machine(x, w, b)

	dw <- mean((y - y_hat) * (-2 * x))
	db <- mean((y - y_hat) * (-2))

	GDs <- list(dw = dw, db = db)
	
	return(GDs)
	}



# Gradient 함수 사용 예시

y <- c(2, 4, 6, 8, 10)

GDs <- Gradient(x, y, w, b)

GDs$dw
GDs$db






# Learning 함수정의 ####
# x, y, w, b를 받아 Update된 Parameter를 출력

Learning <- function(x, y, w, b, step) {
	GDs <- Gradient(x, y, w, b)

	uw <- w - step * GDs$dw
	ub <- b - step * GDs$db 

	Updated <- list(w = uw, b = ub)

	return(Updated)
	}



# Learning 함수 사용 예시

# Learning Rate

step <- 0.01

parameters <- Learning(x, y, w, b, step)



# w1이 2에서 1.34로 Update

parameters$w


# b1이 1에서 0.9으로 Update

parameters$b






# Machine Learning Test ####
# Machine 함수를 정의하고 Gradient 함수를 정의하는데 사용
#	Gradient 함수를 정의하고 이를 Learning 함수를 정의하는데 사용
# 최종적으로 Learning 함수만 사용

# read.csv() 파일 읽어오기

DATA <- read.csv("[mlData]/testData.csv")


# 읽어온 데이터 확인

str(DATA)
head(DATA, 10)
plot(DATA)





# Learning Start!!!

# w와 b의 초기값 설정

w <- 2
b <- 3
step <- 0.05



# For(반복문)를 사용하여 반복 횟수를 1500번으로 지정

for(i in 1:1500){
	Parameters <- Learning(DATA$inputs, DATA$outputs, w, b, step)
	w <- Parameters$w
	b <- Parameters$b
} 



# 학습된 w & b

print(w)
print(b)






# 학습결과 시각화

plot(DATA$inputs, DATA$outputs,
     pch = 19, 
     cex = 0.3)

abline(b, w, lwd = 3, col = "red")













# 오차감소 시각화
# loss : MSE 계산

Gradient <- function(x, y, w, b) {

  y_hat <- Machine(x, w, b)

  dw <- mean((y - y_hat) * (-2 * x))
  db <- mean((y - y_hat) * (-2))

  loss <- mean((y - y_hat) ^ 2)
  
  GDs <- list(dw = dw, db = db, loss = loss)
  
  return(GDs)
  }




# loss 전달

Learning <- function(x, y, w, b, step) {
  
  GDs <- Gradient(x, y, w, b)
  
  uw <- w - step * GDs$dw
  ub <- b - step * GDs$db 
  
  Updated <- list(w = uw, b = ub, loss = GDs$loss)

  return(Updated)
  }







# 초기값 지정

w <- 2
b <- 3
step <- 0.05
loss <- c()




# 1500번 반복학습

for(i in 1:1500){
  Parameters <- Learning(DATA$inputs, DATA$outputs, w, b, step)
  w <- Parameters$w
  b <- Parameters$b
  loss[i] <- Parameters$loss
  }


# loss 감소 시각화

head(loss, 100)



# 전체

plot(loss)



# 앞쪽 30개

plot(loss[1:30])



# 뒤쪽 30개

plot(loss[1471:1500])









# The End ####