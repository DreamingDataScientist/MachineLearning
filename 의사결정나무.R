#의사결정나무
#가게에 찾아오는 고객에게 설문지를 돌려 간단한 인적사항이나 개인정보를 조사하면서
#할인쿠폰을 선물로 준다고 가정하자.
#이때 할인쿠폰에 대한 반응도를 Y/N으로 구분하고 어떠한 패턴을 가진 사람이 쿠폰을 좋아하는지 관찰한다.

#이러한 문제를 분류문제라고 한다. 즉, 입력값의 결과가 Y/N 같이 이산형 데이터가 출력. 따라서 이러한 값을 예측하도록 훈련시키는 것을 감독(지도)학습이라고 한다.
#보통 분류문제는 답이 2개중 하나를 고르는 것이다.

#앞서 받은 설문지 데이터에서 고객의 특징이나 규칙을 파악하고 싶다. 의사결정나무 알고리즘 이용!!!

#감독(지도)학습이라고 한다.
#보통 분류문제는 보기2/3개중 하나르 고르는 것이다.
#앞서 바은 설문지 데이터에서 고객의 특징이나 규칙을 파악하고 싶다. 패턴 - 의사결정 나무 알고르즘 이용.

#다음달에 연체할 카드가입회원은 누구인가?
#약정 끝나면 이탈할 고객은 누구인가?
#타이타닉에서 살아남을 사람은 누구인가?
#http://titanic-survivor.herokuapp.com

#기계학습의 하나로 특정항목에 대한 의사결정규칙을 이진 트리 형태로 분류해나가는 분석기법. 수치형/범주형 데이터 모두 사용 가능.
#의사결정나무 분석방법에는 통계학에 기반한 카이제곱, t검증, f검증등을 활용한 CART, CHAID 알고리즘이나 기계학습 계열(에트로피, 정보이득)인 ID3, C4.5, C5.0등이 존재. rpart 함수는 ID3 알고리즘을 적용
#tree 패키지는 CART알고리즘을 적용.


#설문조사 내용중 직장인이 있고 30대인 기혼자는 쿠폰을 사용하도록!!!  구매할 수 있도록 한다.
skin <- read.csv('c:/Java/skin.csv')
skin <- [,-1]
head(skin)

#예측분서과 분류분석.
library(rpart)
head(skin)

attach(skin)
tree1 <- rpart(쿠폰반응여부~., data=skin, control=rpart.control(minsplit = 3))
plot(tree1, compress=T, uniform=T)
text(tree1, use.n=T, col='blue')

#CART알고리즘을 사용하는 TREE패키지 적용
install.packages('tree')
library(tree)
skin2 <- tree(쿠폰반응여부~., data=skin)
plot(skin2)
text(skin2)

#skin의 각 변수에 대해 결정여부를 확인
xtabs(~결혼여부+쿠폰반응여부, data=skin)
chisq.test(xtabs(~결혼여부+쿠폰반응여부, data=skin))

xtabs(~성별+쿠폰반응여부, data=skin)
xtabs(~차량보유여부+쿠폰반응여부, data=skin)
xtabs(~직장여부+쿠폰반응여부, data=skin)

#의사결정나무 비율(가중치) 확인
tree1

iris.tr <- tree(Species~., iris)
plot(iris.tr)
text(iris.tr)

idx <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7,0.3))
train.data <- iris[idx==2,]
test.data <- iris[idx==1,]
summary(train.data)
summary(test.data)

install.packages('party')
library(party)
iris.tree <- ctree(Species~., data=train.data)
plot(iris.tree)

table(predict(iris.tree), train.data$Species)
test.pre <- predict(iris.tree, newdata=test.data)
table(test.pre, test.data$Species)

