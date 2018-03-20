#군집분석
#다중척도법을 계량한 분석방법

#각 개체의 유사성을 측정하여 유사성이 높은 대상집단을 분류하고 군집에 속한 유사성과 서로 다른 군집에 속한 개체간의 상이성을 규명하는 분석방법
#개별군집의 특성은 각 군집에 속한 구성원들의 평균값으로 나타낼 수 있고 이것은 다른 군집간의 이질성을 구분하는데 사용할 수도 있음.
#가장 가까운 데이터들끼리 먼저 연결하여 트리형태로 표시 이러한 트리를 덴드로그램이라고 함.

#군집 분석의 유사성은 거리척도/상관계수를 이용.
#즉, 거리는 값이 작을수록 서로 유사한 것으로 인식.

install.packages('cluster')
library(cluster)
#dist(academy, method='')
#euclidean, maximum, manhattan, canberra, binary, minkowski

#비계층적 군집 - kmeans 클러스터링.
#계층적 군집방법. 
academy<- read.csv('c:/Java/academy.csv')
aca_dist <- dist(academy)

#1)최단연결법 : 거리가 가장 가까운 데이터끼리 묶음.
hcl1 <- hclust(aca_dist, method='single') 
plot(hcl1, hang=-1, xlab='학생', ylab='거리')

#2)최장거리 : 먼 데이터끼리 묶음
hcl2 <- hclust(aca_dist, method='complete') 
plot(hcl2, hang=-1, xlab='학생', ylab='거리')

#3)평균거리 : 최단연결한 거리에서 평균을 구함.
hcl3 <- hclust(aca_dist, method='average') 
plot(hcl3, hang=-1, xlab='학생', ylab='거리')

#비계층적 분석 - k-means 클러스터링
data(iris)
summary(iris)
newiris <- iris
newiris$Species <- NULL
kc <- kmeans(newiris, 3) #3개의 군집.
 table(iris$Species, kc$cluster)
head(kc)
 plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)

#cluster means : 국군집의 중심점 (평균값)
#cluster vector : 각 요소의 군집 분류번호.
 
 
 
 
 
 
 
 
 
 
 
 
 


