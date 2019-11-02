#회귀분석
#상관분석은 변수들이 서로 얼마나 밀접한 관계를 가지고 있는지 분석하는 통계적 기법.
#한편, 회귀분석은 상관분석 + 예측하는 통계적 기법.
#회귀분석시 변수는 한개 이상이 될 수 없음.
#각각의 변수들을 독립변수와 종속변수로 나누어 관계파악.

#예를 들어, 작년 여름에 온도가 30도 이상일때, 빙과류가 하루 평균 100개 팔렸다. 올해 여름에 온도가 30도 이상되면 빙과류는 30개 이상 팔릴까? -회귀
#즉, 앞으로 발생할 일은 지난 과거에 일어난 일의 평균수준으로 돌아가려는 성질이 있음. 

#따라서, 회귀분석은 시간에 변화하는 데이터나 어떤 영향, 가설적 실험, 인과관계등의 모델링등의 통계적 예측에 사용될 수 있다.

#영국 유전학자 골턴은 부모의 키와 자녀들의 키사이의 연관관계 조사 - 선형적 관계 있음을 파악. 후에 칼 피어슨이 이것을 수학적으로 증명함. - 회귀분석이론정립.

#일정한 패턴을 분석해서 활용하면 무언가를 예측할 수 있다.
#예를들어, 어떤 d회사가 광고를 5번 했을 때 판매량은 얼마나 될까? 를 조사한다고 할 때 회귀분석을 이용하면 된다.
#산포도에서 일정한 점들이 모여있는 패턴을 파악하고 공식으로 도출해 낸다면 판매량을 예측할 수 있다. 공식을 도출해서 Y=50 + 30X가 나왔다면 X=5를 적용하면 Y(판매량)가 200이 된다.
#이렇게 점들이 퍼져있는 형태에서 일정한 패턴을 찾고 이 패턴을 통해 무언가를 예측하는 것이 회귀분석이다.
#회귀분석에는 선형회귀분석과 로지스틱 회귀분석이 있다.

library(MASS)
head(Cars93)
car <- Cars93
attach(car)
lm(Price~EngineSize+RPM+Weight, data=car)
summary(lm(Price~EngineSize+RPM+Weight, data=car))
7.087274**-11

library(boot)
data(nodal)
a<-c(2,4,6,7)
head(nodal)
data<-nodal[,a]
data
glmModel <- glm(r~., data=data, family='binomial')
glmModel
summary(glmModel)

head(nodal) #r이 양성반응이 나오는 것이기 때문에 종속변수.
#나머지는 독립변수로서 전립선암에 영향을 주는지 판단해보자.

mjmodel <- glm(r~., data=nodal, family = 'binomial')
summary(mjmodel)

#부모와 자식의 IQ관계 파악
#부모의 IQ가 117이라면 자식의 IQ는 얼마나될까?
#부모 : 110, 120, 130, 140, 150
#자식 : 100, 105,128, 115, 142
parents = c(110, 120, 130, 140, 150)
child = c(100, 105,128, 115, 142)

plot(parents, child, type='l')
pc_iq <- lm(child~parents) # y=-4.20 + 0.94x
abline(lm(child~parents), col='red', lwd=2)

#Call:
#  lm(formula = child ~ parents)

#Coefficients:
#  (Intercept)      parents  
#    -4.20         0.94  

#회귀모형 검증 및 적합도 파악.
#lm()함수로 구한 회귀식이 통계적으로 유의 여부 확인.
#어느정도 설명 가능한지 여부 확인.
#F통계량
#p-value
#R제곱

summary(pc_iq)

#Call:
#  lm(formula = child ~ parents)

#Residuals:
#  1     2     3     4     5 
#0.8  -3.6  10.0 -12.4   5.2 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  -4.2000    40.9644  -0.103   0.9248  
#parents       0.9400     0.3133   3.001   0.0576 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 9.906 on 3 degrees of freedom
#multiple r스퀘어가 0.75라는 건 믿을만한 자료라는 것이 75%이고 수정된 알값은 66% 믿을만 하다.
#Multiple R-squared:  0.7501,	Adjusted R-squared:  0.6668 
#F-statistic: 9.004 on 1 and 3 DF,  p-value: 0.05764

#p-value: 0.05764인 것은 57퍼센트 오류가 날 확률이기 때문에 통계적으로 의미가 없음.

coef(pc_iq)
predict(pc_iq, data.frame(c(117,117,117,117,117))) #예측값 계산

#자동차 주행속도와 제동거리간의 관계 파악
cars 
lm(cars$speed ~ cars$dist) #intercept -절편 / car$dist-기울기. 
distsp <- lm(cars$dist~cars$speed)
distsp #-17.579 + 3.932 * x
plot(distsp)
plot(cars$dist~cars$speed)
abline(distsp, col='blue')
fitted(distsp)

#speed 100km일때 제동거리 예측
-17.579 + 3.932*100
-17.579 + 3.932*10

summary(distsp)
#p-value : 1.49e-12 (0.05)유의범위 5% 내 -통계 유의미.
#Adjusted R-squared : 0.6438. 64%정도 신뢰 가능성 존재
#cars$speed    3.9324     0.4155   9.464 [1.49e-12] ***

#놀이동산 만족도 여부 회귀분석.
amuse <- (amuse[,2:7])
head(amuse)

overall_rides <- lm(overall ~ rides, data=amuse)
overall_rides #y=-94.962 + 1.703*x
plot(overall ~ rides, data=amuse)
abline(overall_rides, col='red', lwd=2)
summary(overall_rides)
plot(overall_rides)

#만약 rides의 만족도가 10이라면?
-94.962 + 1.703*10 #전체만족도는 -77로 추정됨.
#F검증 : rides         1.7033     0.1055   16.14   <2e-16 ***
#P값 = 2.2e-16 유의수준은 좋음. 오류날 확률이 거의 없음.
#R제곱 = 0.3421 즉 34프로 신뢰도.

college<- read.csv('c:/Java/대학입학성적.csv')
head(college)

#로지스틱 회귀분석
#선형 회귀분석에서는 특정 독립변수를 입력하면 회귀식을 통해 수치값으로 결과를 얻게됨. 따라소 소득,시험성적,연간소득을 알려면 선형회귀분석 사용
#하지만, 내가 예측하고자 하는 것이 수치적으로 나타내기 어려운 것들은 로지스틱 회귀분석을 사용. 예) 고객의 부도발생여부, 조난된 배에서 살아남을 확률, 어떤 학생의 최종학력 등

#회귀분석이므로 기본적으로 지도 학습(하향적방식 - 기계학습)에 속하고 다양한 분야에서 분류 및 예측을 위한 기법으로 사용

#단, 선형회귀분석과 달리 결과변수가 범주형 데이터인 경우 사용되는 기법임을 명심하자.

#다음 시험점수와 순위로 해당 학교에 입학 가능여부 확인
#입학여부, gre(대학졸업점수), gpa(내신성적), 학교등급
#admit : 1 입학성공, 0 입학실패
college
glm(college)
library(sqldf)
admit_ok <- sqldf('select * from college where admit==1')
admit_no <- sqldf('select * from college where admit==0')
head(admit_ok)
head(admit_no)

ok_mean <- sapply(admit_ok[2:3], mean)
no_mean <- sapply(admit_no[2:3], mean)

ok_mean
no_mean

#rank는 이산형 데이터이므로 범주형 변수로 변환
range(college$rank)
college$rank <- as.factor(college$rank)
head(college)

#회귀식 분석
attach(college)
glm_college <- glm(admit~gre+gpa+rank, family='binomial')
#family는 y의 값이 binomial.즉 0또는 1이라는 것이다.
summary(glm_college)
plot(college)
plot(gpa, rank, data=college)
plot(gre, rank, data=college)
head(college)


library(tseries)
install.packages('tseries')
install.packages('forecast')
install.packages('TTR')


#다중선형 회귀분석
#단순선형회귀분석에 비해 변수가 2개 이상 증가 - 여기서말하는 변수는 독립변수.
#놀이기구에 대한 전체만족도를 분석에서 놀이기구+게임+청결도와 전체만족도관계 분석
head(amuse)
lm_amuse <- lm(overall~rides+games+clean, data=amuse)
summary(lm_amuse)
#회귀식 : -131.67877(절편) + 0.57798a + 0.26028b + 1.28381c
#p값 :  p-value: < 2.2e-16 *** 유의수준은 서로 관계가 유의하다.
#r값 : 0.4358 신뢰성은 43%로 그닥 믿을만하지는 않다.

