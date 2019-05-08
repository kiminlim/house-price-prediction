## 3) Sqaure feet variables
### �ְ� ���� ����(sqft_living)�� ���� ���� ����(price)
### sqft_living : �ְ� ������ ��� ��Ʈ(����)

# �� �������� ������

p1 <- house %>% ggplot(aes(x=sqft_living, y=price))+
  geom_point(shape=1, alpha=0.2, color="#DC143C")+
  geom_smooth(method = lm,color="#6495ED")+
  ggtitle("Price by sqft_living")

p2 <- house %>% ggplot(aes(x=sqft_living, y=log(price)))+
  geom_point(shape=1, alpha=0.2, color="#DC143C")+
  geom_smooth(method = lm,color="#6495ED")+
  ggtitle("Log price by sqft_living")

grid.arrange(p1,p2,ncol=2)
# log��ȯ���Ŀ� ȸ���������� �ָ������� ������� ������ ����ġ�� ����(Ȥ�� ����) �����Ͱ� �ִ�.
# �̻����� �ǽɵ�����  outlierTest �� ���� Ȯ���Ѵ�.
# �������� ȸ�������� ���� �ְ� ���� ������ ���� ���ݿ��� ������ ���谡 ������ �� �� �ִ�.
# ��Ȯ�� ��ġ�� ���� �� �������� �������� �����Ѵ�.

# price�� sqft_living������ ����м�
cor.test(house$price, house$sqft_living)
cor.test(log(house$price), house$sqft_living)
# ������跮 t�� ũ�� p���� �ſ� �۱� ������ �� �������̿� ������ ������谡 �ִٰ� �Ǵ��Ѵ�.
# ������ = 0.70�� 1�� �����ٰ� �Ǵ��Ͽ� �� �������� ������ ������谡 �ִٰ� �� �� �ִ�.
# log��ȯ�� ���Ӻ���(price)�� �� ������ ���� ��ȯ������ �������� �ſ� �����ϴ�.

# �ܼ�ȸ�ͺм� �� �̻�ġ Ȯ��

out <- lm(price ~ sqft_living, data=house)
summary(out)

log_out <- lm(log(price) ~ sqft_living, data=house)
summary(log_out)

outlierTest(out)
rstudent(out) %>% summary()
outlierTest(log_out)
# y������ sqft_living������ ���� p���� �ſ��۰� ��������� 0.49�̴�
# outlierTest�� �� ��� ���� price�� ���� �̻�ġ�� 10��, log��ȯ�� price�� ���� �̻�ġ 1���� �߰� �Ǿ���.
# rstudent�� ���� ��Ʃ��Ʈȭ ������ ���� summary�غ� ��� Q3�� Max���̿� ��û�� ���̰� ������ �� �� �־���. 

# �α׺�ȯ�� outlierTest�� ���� ���� �����͸� �����ϰ� �ٽ� Ȯ���غ��Ҵ�.
test <- house[-c(8913),]
test_out <- lm(log(price) ~ sqft_living, data=test)
summary(test_out)

# 95% �ŷڱ���
confint(out,level = 0.95)
# y������ x(sqrt_living)�� ���� ������ ��� �ŷڱ����ȿ� ���ϱ� ������ 95%���ؿ��� �����ϴٰ� �� �� �ִ�.


# �л�м�ǥ(ANOVA Table)
anova(out)
anova(log_out)
# F��跮�� ���� ����Ȯ���� �ſ� �۱� ������ �� ���� ��� ȸ�͸����� �� �������ְ� �ִٰ� �Ǵ��Ѵ�.


### ���Ͻ��� ������ ����(sqft_above)�� ���� ���� ����(price)
### sqft_above : ���Ͻ��� ������  ��� ��Ʈ(����)

# �� �������� ������

p1 <- house %>% ggplot(aes(x=sqft_above, y=price))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Price by sqft_above")

p2 <- house %>% ggplot(aes(x=sqft_above, y=log(price)))+
  geom_point(shape=4, alpha=0.2)+
  geom_smooth(method = lm,color="blue")+
  ggtitle("Log price by sqft_above")

grid.arrange(p1,p2,ncol=2)

# �������� ȸ�������� ���� �ְ� ���� ������ ���� ���ݿ��� ������ ���谡 �������̶� �����Ѵ�..
# �׷��� ���Ͻ��� ������ ������ 2750��Ʈ���� ���� ������ ���ߵǾ� �־� �׷������� ����� ��ġ���� ������谡 ���� �ٸ������� ����ȴ�.
# ���� ��Ȯ�� ��ġ�� ���� �� �������� �������� �����Ѵ�.


# price�� sqft_living������ ����м�
cor.test(house$price, house$sqft_above)
cor.test(log(house$price), house$sqft_above)
# ������跮 t�� ũ�� p���� �ſ� �۱� ������ �� �������̿� ������ ������谡 �ִٰ� �Ǵ��Ѵ�.
# ������ = 0.60�� 1�� �����ٰ� �Ǵ��Ͽ� �� �������� ������ ������谡 �ִٰ� �� �� �ִ�.
# log��ȯ�� ���Ӻ���(price)�� �� ������ ���� ��ȯ������ �������� �ſ� �����ϴ�.

# �ܼ�ȸ�ͺм�
out <- lm(price ~ sqft_above, data=house)
summary(out)

log_out <- lm(log(price) ~ sqft_above, data=house)
summary(log_out)
# y������ sqft_above������ ���� p���� �ſ��۰� ��������� �� 0.4���� 
# p-value�� �ſ� ������ ������ ȸ�ͽ��� �������� ���ٰ� �Ǵ��ϱ� ��ȣ�ϴ�.


# 95% �ŷڱ���
confint(out,level = 0.95); out$coefficients
confint(log_out,level = 0.95); log_out$coefficients
# y������ x(sqrt_living)�� ���� ������ ��� �ŷڱ����ȿ� ���ϱ� ������ 
# �� ���� ��� 95%���ؿ��� �����ϴٰ� �� �� �ִ�.


# �л�м�ǥ(ANOVA Table)
anova(out)
anova(log_out)

### sqft_lot: ������ �����Ʈ(����)
#he lot size is the living size + front yard, back yard (total land space)
# �� �������� ������
head(house$sqft_lot)

p1 <- house %>% ggplot(aes(x=sqft_lot, y=price))+
  geom_density(alpha=0.5)+
  ggtitle("Price by sqft_lot")

p2 <- house %>% ggplot(aes(x=sqft_lot, y=log(price)))+
  geom_density(alpha=0.5)+
  ggtitle("Log price by sqft_lot")

grid.arrange(p1,p2,ncol=2)


p3 <- house %>% ggplot(aes(sqft_lot))+
  geom_density(alpha=0.5)+
  ggtitle("density plot of sqft_lot")

grid.arrange(p3)

## sqft_basement
p1 <- house %>% ggplot(aes(sqft_basement)) + 
  geom_density(alpha=0.3) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Density plot of sqft_basement")

p2 <- house %>% ggplot(aes(x=sqft_basement, y=price)) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red") +
 
  ggtitle("Scatter plot of sqft_basement and price")

p3 <- house %>% ggplot(aes(x=sqft_basement, y=log(price))) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red") +
  ggtitle("Scatter plot of sqft_basement and log price")

grid.arrange(p1, p2, p3, ncol=3)

### sqft_living15: 2015�� ���� �ְ� ������ ��� ��Ʈ(����)
# ��, ���� ������ߴٸ� ��ȭ�� ���� �� ����

p1 <- house %>% ggplot(aes(sqft_living15))+ 
  geom_density(alpha=0.3)+
  ggtitle("Density plot of sqft_living15")

p2 <- house %>% ggplot(aes(x=sqft_living15, y=price)) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red") +
  ggtitle("Scatter plot of sqft_living15 and price")

p3 <- house %>% ggplot(aes(x=sqft_living15, y=log(price))) +
  geom_point(shape=3, alpha=0.3) +
  geom_smooth(method=lm, linetype="dashed", color="red", fill="blue")+
  ggtitle("Scatter plot of sqft_living15 and price")

grid.arrange(p1,p2,p3, ncol=3)

# price�� sqft_living15������ ����м�
cor.test(house$price, house$sqft_living15)
cor.test(log(house$price), house$sqft_living15)
# ������跮 t�� ũ�� p���� �ſ� �۱� ������ �� �������̿� ������ ������谡 �ִٰ� �Ǵ��Ѵ�.
# �������� �α׺�ȯ������ 0.58, �α׺�ȯ���� 0.62�� �� �������� ������ ������谡 �ִٰ� �� �� �ִ�.

# �ܼ�ȸ�ͺм� �� �̻�ġ Ȯ��

out <- lm(price ~ sqft_living15, data=house)
summary(out)

log_out <- lm(log(price) ~ sqft_living, data=house)
summary(log_out)

outlierTest(out)
rstudent(log_out) %>% summary()
outlierTest(log_out)
# y������ sqft_living15������ ���� p���� �ſ��۰� ��������� 0.49�̴�
# outlierTest�� �� ��� sqft_livingȸ�ͽĿ� ���� �̻�ġ �˻縦 �������� ������ �����Ͱ� ��µǾ���.

# �α׺�ȯ�� outlierTest�� ���� ���� �����͸� �����ϰ� �ٽ� Ȯ���غ��Ҵ�.
test <- house[-c(8913),]
test_out <- lm(log(price) ~ sqft_living15, data=test)
summary(test_out)

# 95% �ŷڱ���
confint(out,level = 0.95)
# y������ x(sqrt_living)�� ���� ������ ��� �ŷڱ����ȿ� ���ϱ� ������ 95%���ؿ��� �����ϴٰ� �� �� �ִ�.


# �л�м�ǥ(ANOVA Table)
anova(out)
anova(log_out)

# sqft_living�� sqft_living15�� ���絵
sum(house$sqft_living == house$sqft_living15)/nrow(house)
# sqft_living�� sqft_living15�� �����Ͱ� 12%���� �����Ѱ����� �Ǵܵȴ�.
# ����� �ÿ� ���� ���� �� �ִٰ� �����Ƿ� ����� �� ���� sqft ���� ���� ������ Ȯ���ʿ�

living_diff <- house %>% filter(renovated=="1") %>% select(sqft_living,sqft_living15,price)
sum(living_diff[,1]==living_diff[,2] )
# ������� �ǹ� 500�� �� ������ ������ ���� �����ʹ� 500-39= 461�� �̴�

