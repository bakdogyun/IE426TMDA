# set path, working folder path
setwd('./')

#import file
mydata = read.csv('./sample_data_1_it_firm.csv')

# plot
plot(SALES ~ IT.STOCK, data = mydata)
# cor
cor(mydata$SALES, mydata$IT.STOCK)

# LRM
regresult = lm(formula = SALES ~ IT.STOCK, data = mydata)
summary(regresult)

# confint
confint(regresult, level = 0.95)

# residual
regresult$residuals
# fitted
regresult$fitted.values

#data frame
newdf = cbind(mydata$IT.STOCK, mydata$SALES, regresult$fitted.values, regresult$residuals)
# convert dataframe
colnames(newdf) = c("obs x", "obs y", "fitted y", "residual")
newdf = data.frame(newdf)

abline(a=14.41633, b=0.766)

rss = sum(newdf$residual^2)
tss = sum(newdf$obs.y-mean(newdf$obs.y)^2)

# r
1-rss/tss

# adjusted R Square
1-(rss/tss)*504/503

# RMSE
rmse = sqrt(rss/505)
rmse
