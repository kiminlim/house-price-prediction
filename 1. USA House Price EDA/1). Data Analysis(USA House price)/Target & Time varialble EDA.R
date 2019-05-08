install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("car")

library(car)
library(ggplot2)
library(dplyr)
library(corrplot)
library(gridExtra)

## ������ �ҷ����� �� �ľ�
train <- read.csv(file.choose(),header = T)
head(train)

# �� ������ �ս��� ���� �� ������ house�� �����ֱ⸦ �Ѵ�
house <- train

str(house)
sum(is.na(house))
# ����ġ�� ������ �� �� �ִ�.

## Target variable
## ������� ���� ����(price)���� �������
cor.mat <- cor(house[,-c(1,2)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
par(mfrow=c(1,2))
corrplot(cor.mat, method = "ellipse", type="upper", diag=F, col=col(200))
corrplot(cor.mat, method = "color", type="upper", diag=F, addCoef.col = "black", col=col(200))
# ���ݰ� �ٸ� ��������� ������踦 ���캻 ���
# sqft_living, grade, sqft_above, sqft_living15, bathroom ������ ���� ���� ������踦 ������.

summary(house$price)
par(mfrow=c(1,2));boxplot(house$price,main="boxplot of price");
boxplot(log(train$price),main="boxplot of log(price)")

price_p <- qplot(house$price,geom = "density")
lprice_p <- qplot(log(house$price),geom = "density")
grid.arrange(price_p,lprice_p,ncol=1)

# price�� ������ ������ �䲿�� ���·�, log��ȯ�Ͽ� ���캸�Ҵ�.
# log ��ȯ�� �׷����� �����ʲ����� ���� ������ ���������� �׷��� ���� ���Ժ����� ���������. 

qqnorm(house$price,main="price Q-Q Plot")+ 
  qqline(house$price,col="red")
qqnorm(log(house$price),main="log price Q-Q Plot")+
  qqline(log(house$price),col="red")

# Q-Q plot�� ���� log ��ȯ�� price�� ���� ���Լ��� ��°��� Ȯ���ߴ�.

## Input Variable
## 1) Time Variable
### 1. ���ų�¥ yyymmdd���·� �ٲٱ�
house$date <- substr(house$date,1,8)
house$yyyy <- substr(house$date,1,4)
house$yyyymm <- substr(house$date,1,6)

house %>% select(yyyymm) %>% table

temp <- house %>% group_by(yyyymm) %>% summarise(Freq=n(),
                                                 Total_price=sum(price),
                                                 Mean_price=mean(price),
                                                 Median_price=median(price))
p1 <- temp %>% ggplot(aes(x=yyyymm, y=Freq, group=1))+ 
  geom_line() + 
  geom_point() +
  ggtitle("Freq by yyyymm")

p2 <- temp %>% ggplot(aes(x=yyyymm, y=Total_price,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Total_price by yyyymm")  

p3 <- temp %>% ggplot(aes(x=yyyymm, y=Mean_price,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Mean_price by yyyymm")  

p4 <- temp %>% ggplot(aes(x=yyyymm, y=Median_price,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Median_price by yyyymm")  

grid.arrange(p1, p2, p3, p4, ncol=2)
# ���Ž����� ��ȭ�� ���� ���ź�(p1),�� ����(q2), ��հ���(q3), ������ �߾Ӱ�(q4) 
# 2014�� 12���� 2015�� 1��,2�� ���̿� ������ ���� �������� ���� �Ѵ޻��̿� ���ź󵵰� �þ��, �̷� ���� �� �ŷ����� �����ߴ�.
# 2015�� 4���� 5�����̿� ������ ����ϸ鼭 �ŷ����� �޼��Կ� ���� �� �ŷ����� �����Ͽ���. 


### 2. �ǹ��� ������ ������ ���� ��հ���
# yr_bulit : �ǹ��� ������ �⵵

table(house$yr_built)

yrb_price <- house %>% group_by(yr_built) %>% summarise(price_Mean=mean(price))
# ������ ������ ���� ��հ���

yrb_p <- yrb_price %>% ggplot(aes(x=yr_built, y=price_Mean,group=1)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Mean_price by yr_built")  
grid.arrange(yrb_p)

diff <- yrb_price[c(1:nrow(yrb_price)-1),2] - yrb_price[c(2:nrow(yrb_price)),2]

# ������ ������ ���� ��հ��ݱ׷���
# 1940�⵵�� ����2������ �߻����� ���� ������ �޶��ߴ�.


### 3. ���� ����� ���ο� ���� ����
# yr_renovate : ���� ������� �⵵

nrow(house)
table(house$yr_renovated)
# ��ü �� 15000���� ������ �� ��������� ���� ���๰�� 14000���� �ִ�.
# ����� ������ ���� �񱳺��ٴ� ������� �ǹ��� ��������� ���� �ǹ����� �񱳰� �� �ǹ��ִ� �Ǵ��Ͽ�
# ��������� ���� �ǹ��� 0, ������� �ǹ��� 1�� ������ �ڷḦ �����Ͽ� ���Ѵ�.

house$renovated <- ifelse(house$yr_renovated==0,"0","1")
table(house$renovated)
# ��������� �����ǹ� = 0, ������� �ǹ�= 1�� ������ �ڷ����

qplot(renovated,price,data=house,geom="boxplot")
qplot(renovated,log(price),data=house,geom="boxplot")

# ����� ���ο� ���� ������ ����
# ��ü������ ������� �ǹ��� ������ ���� ���� �ǹ����� ���Ҵ�.

bartlett.test(price ~ renovated, data=house)
bartlett.test(log(price) ~ renovated, data=house)

summary(aov(price ~ renovated, data=house))
summary(aov(log(price)~ renovated, data=house))

# �Ͽ���ġ �л�м� ���������� ������ ��л꼺�� �����ϴ���  ������ �ǽ��ߴ�
# price, log(price)��� p-value�� �ſ� �۱� ������ �͹������� �Ⱒ�����ν�
# ������ ��л꼺 ������ �����Ѵٰ� �Ǵ��Ѵ�.

# ANOVA ���, p-value�� �ſ� �۱� �빮�� �͹������� �Ⱒ�Ѵ�.
# ���� ����� ���ο� ���� price�� ��հ����� �ٸ��ٰ� �� �� �ִ�.


