food <- read.csv('C:\\Java\\food.csv')
food

#유사부류 찾기 - 거리개념을 이용 (유클리드 거리행렬 = 피타고라스)
#학업집중도에 따라 학생 반평성해보기.
academy <- read.csv('c:/Java/academy.csv', stringsAsFactors =F)
head(academy)
summary(academy)

#dist()로 변환.
distaca <-dist(academy)
#좌표로 표시하기 위해 2차원 그래프로 변환해야하기. x,y좌표. - cmdscale()
aca_xy <- cmdscale(distaca)
plot(aca_xy[,1],aca_xy[,2], type='n', asp=1, main='academy')
plot(aca_xy, type='n')
text(aca_xy[,1],aca_xy[,2],cex=0.7)
abline(v=0, h=0, lty=2, lwd=0.5, col='grey')

#2) 민지방법 - 학업집중도와 점수 평균 나누기.
library(sqldf)
score_mean <- subset(academy, select = c('국어점수평균','수학점수평균','영어점수평균', '과학점수평균','학업집중도'))
head(score_mean)
mean <- apply(score_mean[1:4],1,mean)
mean<- as.data.frame(mean)
mean

total_aca <- merge(mean, academy[,6])
head(total_aca)
names(total_aca) <- c('score_mean', 'concentration') 
library(sqldf)
total_aca <- sqldf('select * from total_aca order by 1 desc')
head(total_aca)

#dist()로 변환.
distaca <-dist(total_aca)
head(distaca)

#좌표로 표시하기 위해 2차원 그래프로 변환해야하기. x,y좌표. - cmdscale()
aca_xy <- cmdscale(distaca)
plot(aca_xy[,1],aca_xy[,2], type='n', asp=1, main='academy')
plot(aca_xy)
text(aca_xy[,1],aca_xy[,2],cex=0.7)
abline(v=0, h=0, lty=2, lwd=0.5, col='grey')

#고객들이 선호하는 음식메뉴 유사성 알아보기.
food <- read.csv('c:/Java/food.csv')
head(food)
foo<-dist(food)
head(foo)
food_xy<-cmdscale(foo)
plot(food_xy, type='n')
text(food_xy, colnames(food_xy))
abline(v=0, h=0, col='blue')


#
food2 <- read.csv('c:/Java/food.csv')
food2 <- food2[-1] #1열만 제외.
head(food2)
food2 <- t(as.matrix(food2)) %*% as.matrix(food2) 
  #t() : 기존 데이터를 행렬로 전환
  #거리계산이 제대로 되도록 자기자신을 한번 더 곱함.
head(food2)
distfood <- dist(food2) #거리행렬
plot(distfood, type='n')
text(distfood, colnames(food2))

#18번 고객이 선택한 추어탕1, 갈비탕 1간의 거리 :0
#2번 고객이 선택한 추어탕 0,갈비탕0 간의 거리 :0
#즉, 추어탕,갈비탕을 선택한 고객과 추어탕,갈비탕을 선택한 고객이 같은 결과값을 가짐 : 중요도가 없음. 서로 구분지어 줘야 할 필요 존재.




