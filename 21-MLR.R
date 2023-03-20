# set path, working folder path
setwd('./')

#import file
mydata = read.csv('./sample_data_1_it_firm.csv')

regress = lm(formula=SALES ~ IT.STOCK + LABOUR+CAPITAL, data=mydata)
summary(regress)

rmse = sum(regress$residual^2)/length(regress$residual)
rmse
