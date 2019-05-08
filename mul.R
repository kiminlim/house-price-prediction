getwd()


hp <- read.csv('train.csv')

train <- read.csv('train.csv')

head(hp)
hp2=hp[,c(-1,-2,-18,-19)]
head(hp2)


hp2[,1]
names(hp2)
hp2$date

cor(train)
# 주성분 분석 - 의미없ㅇ

scdata=scale(hp2)
scdata=scale(hp3)


str(hp2)
V<-cor(scdata)
head(V)
PCres<-eigen(V)
PCres
PCres$vectors[,1:2]

PCres$values

cumsum(PCres$values)[1:8]/sum(PCres$values)

plot(PCres$values)


plot(cumsum(PCres$values)/sum(PCres$values))

# 주성분.


# 분석해보기
# 상관 행렬로 가격과 상관계수 보기
V[,1]

summary(hp2)
# 집값의 평균은 5억정도
# 방 개수는 보통3개 정도
# 화장실 개수는 2개정도
# 평방비트(평수 대신 피트로 집넓이)> 평균 58평

hp3 <- hp2[,c(-5,-9,-13,-14,-15.-17)]
head(hp3)
lm(price~sqft_living,hp3)



# 
unique(train)

# 다중 회귀분석********************** 추가

train <- read.csv('train.csv')
train$date <- substr(train$date,1,8)
train$date <- as.numeric(train$date)
names(train)

(m <- lm(price~., data=train))
summary(m)
str(train)
names(train)



train$date <- as.numeric(train$date)
hp3$sqft_basement # 의미 없ㅇ
train2 <- train[,c(-14)] # 의미없는 변수 뺌 (문자형,)
names(train2)
head(train$sqft_basement)
cor(train$sqft_basement)
train[,c(14)]
# train2로 다중회귀분석.
(m2 <- lm(price~., data=train2))

summary(m2) # 회귀분석 결과 floors는 피벨류가 작아서 회귀 변수로 적합하지 않음.

# summary를 했을 때 floors는 유의하지 않음을 알 수 있음
# 따라서 유의한 변수만 있도록 회귀모형을 수정.

train3 <- train2[,c(-8)]
length(names(train3))


###### 
(m3 <- lm(price~., data=train3))
summary(m3)

par(mfrow=c(1,1))
plot(m3$fitted.values, m3$y)
abline(0,1,col="blue", lty=2)
plot(m3, which=1)





# 정규성 검정 - 
library(car)
qqPlot(m3,labels=row.names(train3),id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 종속변수(가격)에 로그변환을 취해주니 잔차가 정규성을 띄는 것을 확인할 수 있음.
qqPlot(log_m4,labels=row.names(train4),id.method="identify",simulate=TRUE,main="Q-Q_ plot")

hist(train$price)
hist(log(train$price))
qplot(train$price)
qplot(log(train$price))
# 로그변환 후 price가 정규분포에 가까워짐을 확인.

# 독립성 검정 -
durbinWatsonTest(m3) # p값이 의미가 없으므로 자기상관은 없다고 할 수 있다. lag의 값 1은 각각의 자료를 바로 다음 자료와 비교했다는 것은 뜻한다

# 선형성 
crPlots(m3)

# 등분산성 
ncvTest(m3)
# 여기서 유의한 결과 가 나온다면 오차의 등분산성 가정이 위배된다고 할 수 있다.
spreadLevelPlot(m3) #. Suggested power transformation 값은 일정하지 않은 오차의 분산을 안정화시키기 위해 필요한 power transformation 값을 제시해준다. ( 0.5의 경우 Y 대신 sqrt(Y) 사용, 0인 경우 log사용 등 ) 








#######< 로그변환

(log_m3 <- lm(log(price)~., data=train3))
summary(log_m3)
# 로그변환후 결정계수도 0.77로 더 높아진 것을 확인할 수 있음. 



## 오차항 가정 4가지

par(mfrow=c(2,2))
plot(m3)


plot(log_m3)


# 정규성 검정 - 
library(car)
qqPlot(log_m2,labels=row.names(train2),id.method="identify",simulate=TRUE,main="Q-Q_ plot")
# > 종속변수(가격)에 로그변환을 취해주니 잔차가 정규성을 띄는 것을 확인할 수 있음.
qqPlot(log_m4,labels=row.names(train4),id.method="identify",simulate=TRUE,main="Q-Q_ plot")

hist(train$price)
hist(log(train$price))
qplot(train$price)
qplot(log(train$price))
# 로그변환 후 price가 정규분포에 가까워짐을 확인.

# 독립성 검정 -
durbinWatsonTest(log_m4) # p값이 의미가 없으므로 자기상관은 없다고 할 수 있다. lag의 값 1은 각각의 자료를 바로 다음 자료와 비교했다는 것은 뜻한다

# 선형성 
crPlots(log_m4)

# 등분산성 
ncvTest(log_m4)
# 여기서 유의한 결과 가 나온다면 오차의 등분산성 가정이 위배된다고 할 수 있다.
spreadLevelPlot(log_m4) #. Suggested power transformation 값은 일정하지 않은 오차의 분산을 안정화시키기 위해 필요한 power transformation 값을 제시해준다. ( 0.5의 경우 Y 대신 sqrt(Y) 사용, 0인 경우 log사용 등 ) 


# 선형모형에 대한 전반적인 검증
install.packages('gvlma')
library(gvlma)
gvmodel<-gvlma(log_m4)
summary(gvmodel)
# 출력물 중 Global stat 을 보면 p값이 0.597로 OLS 회귀의 모든 가정을 만족한다고 할 수 있다. 만일 p값이 0.05 이하인 경우에는 어느 부분이 위배되었는지 평가하여야 한다.
# > Global Stat  > 0.00000로 가정 만족하지 않음.. ㅜㅜㅜ
gvmodel<-gvlma(reduced.model)
summary(gvmodel) # 이거로해도 가정 만족하지 않음..

# 이상값
par(mfrow=c(1,1))
library(car)
car::outlierTest(log_m2)
car::outlierTest(log_m4) # . 이 함수는 가장 큰 잔차가 outlier인지의 통계적인 유의성을 보여준다. 가장 큰 잔차가 유의하지 않다면 데이터에 더 이상 이상치는 없다는 뜻이다. 만일 유의할 경우 이상치를 제거하고 다시 outliertest()를 실시해 보아야 한다.

car::influencePlot(log_m2, id.method="identify", main="Influence Plot",
                   sub="Circle size is proportional to Cook’s distance")
train4 <- train2[-c(1232,5469,6780,8757,8913),]
str(train4)
str(train2) # 5개의 이상값 제거됨을 확인
(log_m4 <-  lm(log(price)~., data=train4))
summary(log_m4) # Multiple R-squared:  0.7753,	Adjusted R-squared:  0.775 


## 모형 비교

# 이상값 제거하거나 하는 식으로 해서 분산분석 후 아노바로 결정계수 더 큰거로 더 좋은 회귀모형을 찾아보자.
anova(m2,m3)
# 두 모형을 비교한 F test에서 p값이 ?로 유의하지 않으므로 fit1에서 두 변수를 제거하는 것은 정당하다. 두 모형의 R2 값과 adjusted R2값을 비교해보자.
summary(m2) # Multiple R-squared:  0.7007,	Adjusted R-squared:  0.7003 

(log_m2 <-  lm(log(price)~., data=train2))
summary(log_m2) # Multiple R-squared:  0.7741,	Adjusted R-squared:  0.7738 
summary(log_m4) # Multiple R-squared:  0.7753,	Adjusted R-squared:  0.775 

summary(m3) # Multiple R-squared:  0.7006,	Adjusted R-squared:  0.7003
summary(log_m3) # Multiple R-squared:  0.7712,	Adjusted R-squared:  0.7709 
# > 	Adjusted R-squared 로 비교시 train4를 로그변환한 'log_m4' 회귀모형이 더 유의한 것을 확인할 수 있음.


# Backward regression



# 로그변환 5가지에 한 것을 5에 담음.
skew_columns = c('price','sqft_living', 'sqft_lot', 'sqft_basement')
train5 <- train2
for (c in skew_columns){
train5[c] = log(train2[c])
}
str(train5)
length(train5)



(log5_m5 <- lm(price~., data=train5))
summary(log5_m5)
plot(log5_m5)



#******************************************** 변수 5개에 로그변환한, 이 데이터가 

# 서윤이가 보내준 블로그 참조해서 >>
# 3. 모델 적합성 판정

# Backward stepwise selection: 모든 독립변수를 사용하는 모델에서 독립변수를 줄여가며 모델의 성능을 향상시키는 방식


## Automatic Backward Selection
backward <- step(m2, direction = "backward", trace = F)
summary(backward) # 이렇게 하면 유의미하지 않은 변수 제거?

# 4. 다중공선성(Multicollinearity) 확인
install.packages("car")
library(car)
install.packages("psych")
library(psych)


pairs.panels(train3[names(train3)]) # 렉..

car::vif(m3) 
sqrt(car::vif(m3)) > 2
# 'sqrt(car::vif(fit))' 가 2보다 크면 다중공선성이 있는 것입니다. 
# false이면 다중공선성이 없다고 판단할 수 있습니다. 

# 이건 패키지 안쓴거>>
vif(backward)
sqrt(vif(backward))>2

## >> sqft_living, sqft_above 는 다중공선성 문제가 있는 것으로 보임.




# 3. 단계별 선택법(stepwise)
# 
# stepwise는 전진선택과 후진소거를 왔다갔다하며 모두 쓰는 방법입니다.
# 우선 상수항만을 포함하고 있는 모형에서 시작해 보겠습니다.


# 먼저 상수항만 포함된 회귀모형을 만들어 줍니다.


m3.con <- lm(price~1,data=train2) 
m3.both <- step(m3.con,scope=list(lower=m3.con,upper=log5_m5), direction = "both")


m3.con <- lm(price~1,data=train5) 
m3.both <- step(m3.con,scope=list(lower=m3.con,upper=log5_m5), direction = "both")
m3.both
summary(m3.both)
# 0.05 유의수준으로 보았을 때, m3에 있는 변수 모두 설명변수(x)로 유의함. 



# 마지막으로 지금까지 새운 모델로 집값(y, 종속변수)을 예측해 보겠습니다.
# pridict 함수를 이용해서 예측하고, 보기좋게 data frame 형태로 바꿔줍니다.

pre_price <- predict(m3.both, newdata = train5) 
pre_price <- as.data.frame(pre_price)
pre_price
head(pre_price) # 예측된 집값.

# 위 방법은 점추정이었으며 이번에는 구간추정을 해보겠습니다.

pre_price <- predict(m3.both, newdata = train5, interval = "confidence") 
pre_price <- as.data.frame(pre_price)
pre_price
head(pre_price) # lwr와 upr로 구간이 나타나게 됩니다.


# 그렇다면 얼마나 잘 예측했는지 실제값과 비교해 보겠습니다.

predict_price <- cbind(pre_price,train5$price) 
head(predict_price,10) # >> 실제값(train$price)이 예측한 구간안에 있는 것도 있고  아닌 것도 있지만
# 다중 회귀분석으로는 예측 율으 매우 낮은 것을 확인할 수 있었다...ㅠㅠ



# 다중 회귀분석 하기.

# 일단 변수 확인하고 널값있는거 처리 어떻게 하는지, 유의미한 피벨류 인 것들로 다시 회귀분석.
