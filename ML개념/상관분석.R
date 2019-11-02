#상관분석

#2개의 변수 X,Y가 있을 때
#두 변수가 서로 어떤 관계에 있는지 파악하는 경우 존재

#키(X)가 클수록 몸무게(Y)도 증가하는가?
#교육(X)을 많이 받으면 소득(Y)이 증가하는가?
#광고(Y)를 많이 하면 판매량(Y)이 커지는가?
#종속변수 : 영향을 받는 변수 - Y
#독립변수 : 영향을 주는 변수 - X

#이처럼 두 변수가 서로 어떤 관계에 있는지 파악 - 상관분석
#두 변수의 상관관계는 산점도를 나타내고 점들이 흩어져 있는 모습을 보고 관계를 파악.
#기울기에 따라 양/음의 상관관계로 분석
#상관분석은 서로가 어떤관계에 있는 지 파악할수가 있지만, 서로의 인과관계는 파악할 수 없다.
#즉 여름이 다가오면 빙과류가 많이 팔린다. 여름이 다가오면 모기가 점점 많아진다는 것은 알 수 있지만 어떤 이유로 그러해지는지 모름.
#간혹 다른 상황에서는 서로 직접적인 영향을 주고 받는 경우도 존재

cor()

#회사에서 신제품이 출시 되었을 때
#안내메일을 발송하는 횟수와 제품판매량 사이 관계 조사
#제품 판매량 : 3,5,8,11,13
#안내메일발송수 : 1,2,3,4,5
sales <- c(3,5,8,11,13)
mail <- c(1,2,3,4,5)
plot(sales, mail, type='l', xlab='판매량', ylab='안내메일 발송', lwd=2)
cor(sales,mail)

#-0.7 ~ -1.0 : 강한 음적 선형관계
#-0.3 ~ -0.7 : 뚜렷한 음적 선형관계
#-0.1 ~ -0.3 : 약한 음적 선형관계
#-0.1 ~ 0.1 : 거의 무시해도 좋음
#0.1 ~ 0.3 : 약한 양적 선형관계
#0.3 ~ 0.7 : 뚜렷한 양적 선형관계
#0.7 ~ 1.0 : 강한 양적 선형관계

#iris에서 꽃받침, 꽃잎의 너비/길이에 대한 상관계수 계산.
cor(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width)

cor(iris[, 1:4]) #가장 상관이있는 부분은 petal의 길이와 너비다.

airquality
air1<-na.omit(airquality)
airs<-air1[,1:4]
cor(air1[,1:4])
plot(air1$Ozone, air1$Temp)
plot(air1[,1:4])
plot(air1[,1:4])
pairs(airs, panel=panel.smooth, lwd=2)
qplot(Ozone, Solar.R, data=airs, geom=c("point", "smooth"))

#상관계수
#상관분석은 두 변수가 서로 어떤 관계인지를 파악하는 분석
#점들이 흩어져 있는 모습을 보고 두변수의 관계를 파악하는데 과연 점들이 모여있는 밀도는 어떻게 표현할까?

#예를 들어, 둘 다 같은 양의 상관관계라 하더라도 점들이 모여있는 모습(밀도)가 다를 수 있기 때문에 서로 얼마나 다른지 정도를 알 수 있는 방법이 필요. 그림으로 파악이 되어도 수학적 표기 역시 필요.
#따라서 통게에서는 추가로 숫자를 사용해서 밀도표현을 하며 숫자가 상관계수라고 한다.
# 보통 -1 ~ +1 까지만 사용해서 밀도를 표현함
#한편, 상관분석은 변수에 따라 치우침이 다른 경우 발생.
#즉, 치우침이 상대적으로 상관계수가 크게 나왔어도 치우침이 상대적으로 작거나 상관계수가 작게 나오지만 치우침이 크게 나오는 경우도 발생. 따라서, 위 기준에 맞지 않는 상황발생. 보조적으로 가설검증으로 결과 해석이 필요
#cor.test()사용!!!

#공분산
#치우침을 활용해서 상관계수를 구하는데 통계에서 치우침을 표현하는 대표적인 기호는 분산을 이용해서 상관계수를 구하기도 하지만 복잡!~
var()

#한편 상관분석은 기본적으로 변수가 2개이므로 치우침은 2개의 변수에 의해 발생. 따라서 각각의 분산외에도 두 변수의 공통된 치우침도 알아야하는데 두 변수의 공통된 치우침을 공분산이라 함.
cov()


#실업자수와 개인 소비 지출의 상관계수 분석
library(ggplot2)
economics
?economics
str(economics)

cor(economics$unemploy, economics$pce)
plot(economics$unemploy, economics$pce, type='l')

#상관계수에 대한 가설검증
cor.test(economics$unemploy, economics$pce)

#결과..!
#Pearson's product-moment correlation
#data:  economics$unemploy and economics$pce
#t = 18.605, df = 572, p-value < 2.2e-16 
#유의수준을 알 수 있음. 2.2e-16은 매우 낮은 수치여서 오류가 날 확률 낮음.
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.5603164 0.6625460
#sample estimates:
#cor 
#0.6139997 


#실업자수unemploy와 개인저축률psavert의 상관관계 분석
#상관계수 : 양의 상관관계 존재
#즉, x값이 증가하면 y값이 증가. (정비례)
#음의 상관관계 존재
#x값이 증가하면 y값이 감소 (반비례)
cor(economics$unemploy, economics$psavert)
plot(economics$unemploy, economics$psavert)
cor.test(economics$unemploy, economics$psavert)

#Cars93 데이터에서 고속도로 연비와 차체중량의 상관관계 분석.
library(MASS)
str(Cars93)
plot(MPG.highway ~ Weight, data=Cars93)
abline(lm(MPG.highway ~ Weight, data=Cars93), col='red', lwd=2)

cor(Cars93$MPG.highway, Cars93$Weight) #공분산을 표준화한 것 - 상관계수
cov(Cars93$MPG.highway, Cars93$Weight) #음수출력 - 음의 상관

#자동차가 무거울 수록 연비는 낮음. 반비례이며 상관관계가 -0.8
cor.test(Cars93$MPG.highway, Cars93$Weight)
#p-value는 p-value < 2.2e-16 이며 오류가 날 확률은 매우 작으며 서로 상관관계가 있다고 할 수 있다.

#상관행렬
#여러 변수(2개이상)의 상관관계를 한번에 알아보고자 하는 경우 모든 변수에 대한 상관관계를 알려주는 상관 행렬을 이용.

cor()
corrplot()
#자동차 각요소에 대한 상관관계를 분석-mtcars이용.
head(mtcars)

cor_cars <- cor(mtcars) #상관행렬 생성
cor_cars <- round(cor(mtcars), 2)
cor_cars

#많은 데이터들이 나오는데 상관계수 가독성을 위한 전용 그래프!
install.packages('corrplot')
library(corrplot)
head(cor_cars)
corrplot(cor_cars) 
#상관행렬을 열 그래프로 시각화, 파란색은 양상관계, 빨간색은 음의 상관관계!

corrplot(cor_cars, method='ellipse')
corrplot(cor_cars, method='shade')


#놀이동산 만족도에 대한 상관관계 분석
#주말이용여부, 동반자녀수, 공원거리까지의 거리, 기구만족도, 게임만족도, 대기시간 만족도, 청결도 만족도, 전체만족도.
#그래프 확인
amusement <- read.csv('C:\\Java\\놀이동산에 대한 만족도.csv')
head(amusement)
amuse <- amusement[,2:8]
head(amuse)
plot(amuse$overall~amuse$rides)
sum_amuse <- sapply(amuse, mean)


#공분산 - cov()
cov(amuse$overall, amuse$rides)

#상관계수
cor(amuse)
cor_amuse <- round(cor(amuse),2)
corrplot(cor_amuse, method='ellipse')

#상관계수 가설검증
cor.test(amuse$clean, amuse$rides)
plot(clean ~ rides, data=amuse)
plot(amuse[2:7]) #산포도 작성
abline(lm(clean~rides, data=amuse), col='red', lwd=2)
corrplot(cor_amuse, method='ellipse') #상관행렬 그래프.













