# 기술통계

#모집단 vs 표본
#전국 대학생 수가 350만명이라 할 때, 전국 대학생의 학업만족도를 조사한다고 하자.
#350만명 학생 모두를 조사해야 하나?

#폭탄 100만개를 생산했다.
#불량 폭탄을 조사하기 위해 폭탄을 터트려야 하는데 모두 다 떨어트려 조사해야 하나?no

#모집단 : 조사하고자 하는 대상 전체
#표본 : 전체를 다 조사하지 않고 일부 선별한 대상들
#표본의 양이 많을 수록 데이터의 신뢰도는 증가

#기술통계의 대표적인 내용
#min,max,평균, 표준편차, 분산

#평균 vs 표준편차, 분산
#세상의 일은 쉽게 예측 하기 어려움.
#정답이라 생각하는 것도 시간/상황에 따라 변함 -오차발생
#이러한 오차에 대비하기 위해 오차예측을 함.
#편차 : 평균에서 데이터값 사이 / 분산 : 편차 제곱

#오차 - 정답에서 벗어난 정도 (치우침 정도)
#통계는 이러한 치우침을 분석하고 관리하는 분야
#치우침을 표현하는 대표적인 척도 : 표준편차, 분산

#그럼, 치우침의 기준점이 필요 = 통계에서는 평균을 사용...!
#그외에도 최빈값, 중앙값 등이 있음.

#편차와 분산
#편차 : 평균에서 데이터값 사이. 편차의 합은 0. 
#그런데, 데이터가 여러개 존재할 경우 : 편차들의 합을 계산
#한편, 수학적계산을 통해 편차들의 합을 구하려면 문제발생
#평균 기준 왼쪽 영역 값들은 -로 인식하기 때문이다. 
#원하는 값을 구하기 위해 (즉, -를 +로 변환) 제곱 계산을 시행.
#제곱 계산을 시행 - 편차합 계산전에 미리 제곱으로 +로 바꿈
#그래서 표준편차가 있음에도 불구하고 분산을 많이 사용
#분산 : 편차들의 제곱 합
#표준편차 : 편차에 제곱을 해서 계산. 실제 값에서 다소 멀어져 있음.
#따라서 값을 다시 가까워지게 만들기 위해 분산에 루트를 씌움.

#기술통계 관련 함수
#데이터 탐색
mtcars
iris
str(mtcars) #데이터 구조, 변수 등등 확인
length(mtcars) # 데이터 객체의 요소(:원소) 갯수 = ncol
nrow(mtcars)
ncol(mtcars)
head(mtcars) + tail(mtcars) #상하위 6개 관측치 보기
install.packages('car')
library(car)
some(mtcars) #무작위 관측치 보기-car 패키지 설치
names(mtcars) #열이름 보기
class(mtcars) #데이터 원소의 속성

#데이터 요약
min(mtcars$mpg) #최소값
max(mtcars$mpg) #최대값
mean(mtcars$mpg) #평균
median(mtcars$mpg) #중앙값
sd(mtcars$mpg) #표준편차 = 분산의 제곱에 루트씌움.
var(mtcars$mpg) #분산 = 표준편차의 제곱
range(mtcars$mpg) #범위
quantile(mtcars$mpg) #사분위
summary(mtcars$mpg)
boxplot(mtcars$mpg)

apply(mtcars, 2, mean) #집계함수를 이용한 데이터 요약
sapply(mtcars[3:10], mean)
tapply(iris$Sepal.Length+iris$Petal.Length, iris$Species, mean)

#분포형태의 대칭정도파악
#중심화 경향,처짐정도
#왜도 - 분포의 좌/우/비대칭 정도
#좌우대치 : 왜도 0
#좌대칭(왼쪽으로 치우침) : 왜도가 0보다 큼 (양수)
#왜도가 양수인 경우 : 최빈값<중앙값<평균
#우대칭(오른쪽으로 치우침) : 왜도가 0보다 작음 (음수)
#왜도가 음수인 경우 : 평균<중앙값<최빈값
hist(mtcars$mpg)
hist(mtcars$disp)

install.packages('fBasics')
library(fBasics)

skewness(mtcars$mpg) #0보다 크기 때문에 왼쪽으로 치우쳐짐. 
skewness(mtcars$disp)

library(MASS)
head(Cars93)
skewness(Cars93$RPM) #우대칭-음수. 
#cars93데이터 중에서 차종별 가격에 대한 왜도 측정.
with(Cars93, tapply(Price, Type, skewness))


#첨도 - 분포의 중심인 뾰족한 부분의 정도.
#정규분포와 동일 : 첨도 0
#정규분포보다 뾰쪽 : 첨도가 +
#정규분포보다 납작 : 첨도가 -

kurtosis(mtcars$mpg)
kurtosis(mtcars$disp)

#산술통계량 - 통계량과 함께 그래프도 같이 보는 것이 좋음.
#중심화경향
 #평균, 중앙값, 최빈값
mean()
median()
min()

#퍼짐정도
 #분산, 표준편차, 범위, 최대/최소, 사/백분위
var()
sd()
summary()
boxplot()

#대칭정도
 #왜도, 첨도
skewness()
kurtosis()

#빈도를 밀도함수로 바꿀 수 있음.
hist(mtcars$mpg)
#밀도함수를 그래프로 그릴 수 있음
#freq=F는 빈도를 FALSE 쓰지 않는다. 때문에 대신 density인 밀도가 y축으로 나옴.
hist(mtcars$mpg, freq=F)
lines(density(mtcars$mpg), col='red', lwd=2) 
hist(mtcars$disp)
hist(mtcars$disp, freq=F)
lines(density(mtcars$disp), col='red', lwd=2)

#분위수, 사분위수, 백분위수
#모집단 또는 표본의 전체 요소를 정렬한 후
#일정한 비율로 나눈 것을 의미 (등분)
#다양한 분위수가 존재(1000분위, 20분위, 12분위, 5분위)
#하지만, 주로 100분위수와 4분위수가 많이 이용

#일(1-10)별 출근 소요시간에 대한 분위수 계산
data <- c(30,29,32,28,32,29,27,32,29,32)

#사분위수
quantile(data)
summary(data) #평균 30분걸리고 최소 27분, 최대 32분걸림.
sd(data) #표준편차 = 1.88
var(data) #분산

#100분위수
quantile(data, 0.1)
quantile(data, 0.2)
quantile(data, 0.3)
quantile(data, 0.4)

#분위수를 그래프로 표시
qqnorm(data)
qqline(data)


hist(data, freq=F)
lines(density(data), col='red', lwd=2)

#줄기- 잎 그래프
#통계자료를 표형태와 그래프 형태의 혼합된 형태
summary(Cars93$MPG.highway)
hist(Cars93$MPG.highway)
stem(Cars93$MPG.highway) #시내에서의 연비
stem(Cars93$MPG.city) #고속도로에서의 연비

#faithful. 미국의 옐로우스톤 공원의 간혈천 분화 발생 데이터
head(faithful)
str(faithful)
plot(faithful$eruptions, faithful$waiting, xlab='간혈천 분화', ylab='분화간격 초')
summary(faithful)
lines(faithful$eruptions, faithful$waiting, col='red')
stem(faithful$eruptions)






