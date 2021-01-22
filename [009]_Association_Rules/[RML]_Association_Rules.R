# arules Package ####
# install.packages("arules")


# arules 패키지 실행

library(arules)



# 판매 물품 데이터

BuyItems <- list(c("삼겹살", "생수", "소주", "감자칩"),
                 c("삼겹살", "생수", "소주", "배"),
                 c("장어", "생수", "소주", "생강"),
                 c("땅콩", "생수", "맥주", "가지"),
                 c("땅콩", "생수", "맥주", "딸기"))



# 트랜재션 데이터로 형변환

BuyItemsStr <- as(BuyItems, "transactions")







# 트랜잭션 데이터
# 5개 거래, 11개 항목

BuyItemsStr



# 트랜잭션 확인

inspect(BuyItemsStr)










# apriori() 함수 ####
# 지지도 0.1, 신뢰도 0.8 이상 연관규칙 구하기

BuyItemsResult <- apriori(BuyItemsStr, 
                          parameter = list(support = 0.1, 
                                           confidence = 0.8))




# 도출된 연관규칙 확인

inspect(BuyItemsResult[1:5])







# 향상도(lift) 1.2 이상인 연관규칙 선택

SubBuyResult <- subset(BuyItemsResult, subset = lift > 1.2)
SubBuyResult

inspect(SubBuyResult[1:5])




# lhs(원인), rhs(결과)
# lhs에 삼겹살 포함

inspect(subset(BuyItemsResult, subset = lhs %in% c("삼겹살")))


# lhs에 삼겹살과 감자칩 모두 포함

inspect(subset(BuyItemsResult, subset = lhs %ain% c("삼겹살", "감자칩")))


# lhs에 삼겹살또는 과자 또는 삼겹살과 과자 모두 포함

inspect(subset(BuyItemsResult, subset = lhs %oin% c("삼겹살", "감자칩")))


# lhs에 "겹"이라는 글자가 포함

inspect(subset(BuyItemsResult, subset = lhs %pin% c("겹")))











# support(지지도), confidence(신뢰도), lift(향상도) 기준으로 정렬
# "땅콩-맥주", "삼겹살-소주" 항목간 연관성 존재

SubBuyResult_Order <- sort(SubBuyResult,
                           by = c("support", "confidence", "lift"))

inspect(SubBuyResult_Order[1:10])













# 결과 시각화 ####

# support(지지도) 0.2 이상인 항목의 빈도수

itemFrequencyPlot(BuyItemsStr, support = 0.2)





# install.packages("arulesViz")

library(arulesViz)

# 연관 분석 결과(52 rules)

SubBuyResult_Order



# 전체결과 시각화
# 산점도(52 rules)
# x축:지지도, y축:신뢰도, 색:향상도

plot(SubBuyResult_Order)





# Grouped Matrix
# x축:Items In LHS Group, y축:RHS
# 원의크기:지지도, 색:향상도

plot(SubBuyResult_Order, method = "grouped")





# 3번째 연관규칙 평행좌표 그래프로 시각화
# {땅콩, 생수} 구매자는 {맥주} 구매와 연관성이 있음

inspect(SubBuyResult_Order[3])

plot(SubBuyResult_Order[3], method = "paracoord")




# 3, 5, 33, 50번째 연관규칙 평행좌표 그래프로 시각화
# 선두께:지지도, 선색:향상도

inspect(SubBuyResult_Order[c(4, 6, 13, 23, 50)])

plot(SubBuyResult_Order[c(4, 6, 13, 23, 50)], method = "paracoord")














# 네트워크 그래프(Network Graph) ####
# 원으로 연관관를 나타냄
# 원크기:지지도, 원색:향상도

plot(SubBuyResult_Order, method = "graph")

plot(SubBuyResult_Order, method = "graph", interactive = TRUE)



# 조건(lhs)과 결과(rhs)를 화살표로 연결
# 지지도를 원의 크기로 시각화
# 장어, 생강, 삼겹살은 소주와 연관
# 땅콩은 맥주와 밀접한 관계
# 생수는 두 그룹에 공통적으로 존재하는 항목

inspect(SubBuyResult_Order[1:10])


plot(SubBuyResult_Order[1:10], method = "graph")


plot(SubBuyResult_Order[1:10], method = "graph", interactive = TRUE)

















# 개별실습 ####

data("Groceries")

Groceries
summary(Groceries)
inspect(Groceries[2001:2010])



# 물품 검사

itemName <- itemLabels(Groceries)
head(itemName)

itemCount <- itemFrequency(Groceries) * 9835
head(itemCount)



# 물품 시각화

itemFrequencyPlot(Groceries, support = 0.01)







# Modeling_1

AR_1 <- apriori(Groceries,
                parameter = list(support = 0.01,
                                 confidence = 0.35))



# lhs(Left Hand Side) -> rhs(Right Hand Side)

inspect(AR_1)



# Dynamic Visualization

plotly_arules(AR_1, method = "scatterplot",
     measure = c("support", "confidence"),
     shading = "lift")



plotly_arules(AR_1, method = "matrix",
     measure = c("support", "confidence"),
     shading = "lift")





# Modeling_2

AR_2 <- apriori(Groceries,
                parameter = list(support = 0.01,
                                 confidence = 0.5))





# Visualization

plotly_arules(AR_2)



# 원 크기 : Support(지지도)
# 원 색상 : Lift(향상도)
# 원으로 들어오는 화살표 : 조건절
# 원에서   나가는 화살표 : 결과절

plot(AR_2, method = "graph")



#  선 : 규칙
# x축 : 아이템 순서
# y축 : 해당 아이템

plot(AR_2, method = "paracoord")










# The End ####