#다차원 척도법
#개체사이의 유사/비유사성을 측정하여 2/3차원 공간에 점으로 표현하는 방법.
#개체간의 근접성을 시각화하여 테스터 속에 잠재해 있는 패턴이나 구조를 찾아내는 통계기법. 개체간의 거리계산은 유클리드 거리 행렬을 이용.

#계량적 다차원척도법(MDS)
#데이터가 연속형변수인 경우 사용하는 분석방법

library(MASS)
data(eurodist) #유럽 21개 도시 강의 거리 측정한 데이
class(eurodist) #dist라는 클래스임...
?eurodist

#각 도시정보를 2차원으로 정리.
citydist <- cmdscale(eurodist)
citydist
#2차원을 위해서 xy 좌표로 나타냄
x <- citydist[,1]
x
y <- -citydist[,2]
y
plot(x,y, type='n', asp=1, main='Metric MDS') #type=n은 글자만 찍히고 점은 안찍힘..ㅎㅎㅎ 
text(x,y, rownames(citydist), cex=0.7)
abline(v=0, h=0, lty=2, lwd=0.5, col='grey')

#자동차에 대한 선호도 조사를 다차원척도법으로 분석
#차종에 대한 호감도를 1-9사이의 점수로 평가.
#5대의 차종에 대한 호감도를 1-9사이의 점수로 평가
set.seed(1565411)
qmatrix <- matrix(sample(c(1:9), 25, replace=T), nrow=5, ncol=5)

#설문조사 내용을 난수로 만들어 행렬에 저장.
car_names <-c('mini', 'landrover', 'rangerover', 'benz', 'genesses')
colnames(qmatrix) <- car_names
rownames(qmatrix) <- car_names
qmatrix
qm <- as.dist(qmatrix)
qm <- cmdscale(qm)
plot(qm, type='n')
text(qm, rownames(qm), cex=1.2)
abline(h=0, v=0, col='grey')

qmatrix <- matrix(rnorm(100), nrow=5)
q<-cmdscale(dist(qmatrix))
plot(qm, type='n')
text(qm, c(rownames(qmatrix)))

library(datasets)
data(USArrests)
head(USArrests)
pairs(USArrests, pane=panel.smooth, main='USArrests data')
?USArrests

US.prin <- princomp(USArrests, cor=T)
summary(US.prin)
screeplot(US.prin, npcs=4, type='l')


