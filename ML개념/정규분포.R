# 정규분포 - 평균:80, 분산 :10
#40부터 120까지 300개를 차례로 쪼갬.
x <- seq(40, 120, length=300) 
summary(x)
y<-dnorm(x, mean=80, sd=10)
plot(x,y, type='l', col='red', lwd='2')
#plot위에 쓰기 위해 lines를 씀.
lines(x, dnorm(x, mean=80, sd=20), col='blue')

#65~75까지의 범위가 전체에 몇퍼센트가 되느냐?
x2<- seq(65, 75, length=200)
y2<- dnorm(x2, mean=80, sd=10)

polygon(c(65, x2, 75), c(0,y2,0), col='grey')
pnorm
qnorm
dnorm
rnorm









