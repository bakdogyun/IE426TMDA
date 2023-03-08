#��� ����
setwd("C:/Users/user/Dropbox/((KHU))/___Teaching (course)/2023_1_Spring/����濵�����ͺм�/��浥_�����ڷ�_2023/�ǽ�")
#���� �ҷ�����
mydata <- read.csv("sample_data_1_it_firm.csv", fileEncoding='euc-kr')
#������ �׷�����
plot(SALES ~ IT.STOCK, data=mydata)
#������� �м�
cor(mydata$SALES, mydata$IT.STOCK)



#regresult�� ȸ�ͺм� ��� ���� �� ����� ���
regresult <- lm(formula = SALES ~ IT.STOCK, data=mydata)
summary(regresult)



#regresult�� ȸ�ͺм� ����� �������� �ŷڱ��� ���
#�ŷڼ��� ���� ���� (95%, 99%, 90%), default=95%
confint(regresult, level = 0.95)
confint(regresult, level = 0.99)
confint(regresult, level = 0.90)



#�츮�� ȸ�ͺм� ����� ������ regresult �ȿ� ����Ǿ� ����
#����
regresult$residuals
#����ġ
regresult$fitted.values
#���� X, Y, ������ Y, ������ �����ϴ� dataframe�� ������
newdf <- cbind(mydata$IT.STOCK, mydata$SALES, regresult$fitted.values, regresult$residuals)
#Į���� ���� �� ���������������� ���� ����
colnames(newdf) <- c("obs x", "obs y", "fitted y", "residual")
newdf <- data.frame(newdf)

#
abline(a= 14.41633, b=0.76529)


#�ռ� ���� newdf�� �ִ� ���� �� y ����ġ�� �̿��Ͽ� rss �� tss ��� 
rss <- sum(newdf$residual^2)
rss
tss <- sum((newdf$obs.y - mean(newdf$obs.y))^2)
tss
#R-squared
Rsq <- 1 - rss/tss
Rsq
#adjusted R-squared
AdRsq <- 1 - ((rss/tss)*504/503)
AdRsq
#RMSE
rmse <- sqrt(rss/505)
rmse


#########################�Ʒ��� ��Ÿ �ڵ� (�����ڷ� 1 �ð�ȭ)

install.packages("corrplot")
library(corrplot)

#mtcars�� �⺻ ��������
mcor <-cor(mtcars)

corrplot(mcor)
