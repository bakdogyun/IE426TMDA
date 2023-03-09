#경로 설정
setwd("C:/Users/user/Dropbox/((KHU))/___Teaching (course)/2023_1_Spring/기술경영데이터분석/기경데_수업자료_2023/실습")
#파일 불러오기
mydata <- read.csv("./sample_data_1_it_firm.csv", fileEncoding='euc-kr')
#산포도 그려보기
plot(SALES ~ IT.STOCK, data=mydata)
#상관관계 분석
cor(mydata$SALES, mydata$IT.STOCK)



#regresult에 회귀분석 결과 저장 및 요약결과 출력
regresult <- lm(formula = SALES ~ IT.STOCK, data=mydata)
summary(regresult)



#regresult에 회귀분석 결과를 바탕으로 신뢰구간 계산
#신뢰수준 설정 가능 (95%, 99%, 90%), default=95%
confint(regresult, level = 0.95)
confint(regresult, level = 0.99)
confint(regresult, level = 0.90)



#우리가 회귀분석 결과를 저장한 regresult 안에 저장되어 있음
#잔차
regresult$residuals
#예측치
regresult$fitted.values
#실제 X, Y, 예측된 Y, 잔차를 포함하는 dataframe을 만들어보자
newdf <- cbind(mydata$IT.STOCK, mydata$SALES, regresult$fitted.values, regresult$residuals)
#칼럼명 지정 및 데이터프레임으로 형식 변경
colnames(newdf) <- c("obs x", "obs y", "fitted y", "residual")
newdf <- data.frame(newdf)

#
abline(a= 14.41633, b=0.76529)


#앞서 만든 newdf에 있는 잔차 및 y 관측치를 이용하여 rss 및 tss 계산 
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


#########################아래는 기타 코드 (수업자료 1 시각화)

install.packages("corrplot")
library(corrplot)

#mtcars는 기본 데이터임
mcor <-cor(mtcars)

corrplot(mcor)

