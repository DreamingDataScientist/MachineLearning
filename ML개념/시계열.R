#시계열
#관측계열이 1년 또는 계절마다, 월마다 주기적으로 같은 패턴을 그리는 것을 '계절성으 갖는다'고 한다.
#보통 기온/강수량 같은 기후데이터나 산업생상. 수입 및 수출과 관련된 데이터는 이런 패턴을 가짐.
#추세: 시계열에서 계절성 말고 다른특징은 패턴을 그리되 시간에 따라 움직이는 경우 존재 
#따라서, 시계열은 계절성과 추세성 그리고 잡음으로 구성.
#이렇게 겹쳐 있으면 잘 구분이 가지 않기 때문에 주로 그래프를 이용하며 시계열은 확률적 현상을 관측하여 얻은 값을 계열이라고 한다. (일정한 간격으로 배치된 값)

kimchi <- read.csv('c:/Java/kimchi.csv')
head(kimchi)

attach(kimchi)
plot(대형마트수량, type='l' ,xlab='주', ylab='금액') 
#50주기로 판매량 증가 - 주기성 존재.
plot(YYWW, 대형마트수량, type='l')
plot(편의점수량, type='l' ,xlab='주', ylab='금액')
plot(백화점수량,type='l' ,xlab='주', ylab='금액')
plot(수퍼수량, type='l' ,xlab='주', ylab='금액')

#각각의 그래프를 한번에 표시.
library(zoo)
dates <- as.Date(as.character(주마지막일자), format='%Y%m%d')
dates
all_sales <- data.frame(cbind(대형마트수량,백화점수량,수퍼수량,편의점수량))
head(all_sales)

z_sales <- zoo(all_sales, dates)
head(z_sales)
plot(z_sales, screens=1, xlab='날짜', ylab='판매량', col=c('violetred2','orange','blue','green3'), lwd=2) 

plot(z_sales, screens=c(1,3), xlab='날짜', ylab='판매량', col=c('violetred2','orange','blue','green3'), lwd=2) 

#마지막 년도를 기준으로 잘라서 추세를 그래프로 확인
big_sales <- zoo(대형마트수량, dates)
sale2015 <- subset(kimchi, 주마지막일자 >= '20150101' & 주마지막일자 <= '20151231')
tail(sale2015)
plot(sale2015)

sales2015 <- data.frame(cbind(대형마트수량,백화점수량,수퍼수량,편의점수량))
plot(sales2015)

z_sales2015 <- zoo(sales2015, dates)

sales_2015 <- zoo(all_sales, dates)
kimchi2015 <- read.csv('c:/Java/kimchi2.csv')
k_sales <- zoo (kimchi2015$SALES, as.Date(as.character(kimchi2015$LAST_WK), format='%Y%m%d'))
merge_2015 <- merge(k_sale=window(sales_2015, start='2015-01-01', end='2015-12-31'), k_sales, all=F)
head(merge_2015)
plot(merge_2015)


#각 그래프 간의 상관관계르 알아봄.
ccf(k_sales, window(big_sales, start='2015-01-01', end='2015-12-31'), main='판매량 상관관계')

#대형마트 김치판매량과 반찬가게 판매추세를 비교하는 그래프.
#각그래프 간의 상관관계를 알아봄.
#각 요소가 서로 영향을 미치는지? 미친다면 수학적으로 얼마나 되는지??

#시간에 따른 변화량 확인 - 차분(현재값-이전값)
plot(diff(big_sales))
acf(big_sales)
head(big_sales)

#두 매장의 판매량의 상관관계는 초반 5주후부터 증가 - 마트판매량 증가하면 다른 매장 파내량 역시 증가..!

#현재의 매출은 과거의 매출과 상관관계가 있나?
#자기상관관계 함수 acf()
acf(big_sales)

#그럼 이러한 자기상관이 김치 시계열에 존재하는건지?
#자기상관에 대한 검증 실시 :p값 조사.
#시차에 대해 조사된 통계량의 '자기상관은 0이다' 라는 귀무가설을 검증
Box.test(big_sales, type='Ljung-Box') #box-pierce검증
Box.test

#이동평균법으로 시계열 그래프를 좀 더 부드럽게 표시
#데이터들을 몇개씩 묶어서 평균을 내어 새로운 값 구함.
#이동평균으로 새로운 값을 구하면 복잡한 선들이 좀더 부드럽게 표시 - 재략적인 패턴 파악 쉽다.
plot(big_sales)
par(mfrow=c(2,2))
roll2 <- rollapply(big_sales, 2, mean) #숫자는 몇개씩 묶어서 평균을 낼 것이다.
roll4 <- rollapply(big_sales, 4, mean)
roll6 <- rollapply(big_sales, 6, mean)
roll8 <- rollapply(big_sales, 8, mean)
plot(roll2); plot(roll4); 
plot(roll6); plot(roll8)

#이동평균의 묶음 width수는 얼마나 좋을까?
#adf를 통해 구한 자기상관수를 사용하자.
par(mfrow=c(1,1))
roll11 <- rollapply(big_sales, 11, mean)
plot(roll11)

#예측값 계산. - 시계열 모형 생성 : ARIMA모델
#R에서는 auto.arima 만능함수를 사용.
install.packages("forecast")
library(forecast)
install.packages('TTR')
fit <- auto.arima(ts(log(big_sales), frequency = 52))
plot(forcast(fit1))
fit1 <- auto.arima(ts(log(big_sales), frequency=52))
fit2 <- auto.arima(ts(log(big_sales), frequency=52))

#생선된 모혀으로 값 예측
v <- predict(fit2, n.ahead=10) #향후 10주 동안 마트 판매량 예측.
v








