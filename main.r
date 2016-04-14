wape = function(pred,test)
{ 
        len<-length(pred) 
    errSum<-sum(abs(pred[1:len]-test[1:len])) 
    corSum<-sum(test[1:len])
    result<-errSum/corSum
    result
}


mae = function(pred,test)
{ 
    errSum<-mean(abs(pred-test))     
    errSum
}

rmse = function(pred,test)
{ 
    res<- sqrt(mean((pred-test)^2) )
    res
}




passenger = read.csv('passenger.csv',header=F,sep=' ')

p<-unlist(passenger)


#把数据变成time series。  frequency=12表示以月份为单位的time series. start 表示时间开始点，可以用c(a,b,...)表示，  例如按月为单位，标准的做法是 start=c(2011,1) 表示从2011年1月开始
#如果要表示按天的，建议用 ts(p,frequency=7,start=c(1,1))  很多人喜欢用 ts(p,frequency=365,start=(2011,1))但是这样有个坏处就是没有按星期对齐
pt<-ts(p,frequency=12,start=2001) #
plot(pt)
train<-window(pt,start=2001,end=2011+11/12)
test<-window(pt,start=2012)

acf(train)
acf(diff(train,lag=1))
acf(diff(diff(train,lag=7)))


library(forecast)
pred_meanf<-meanf(train,h=12)
rmse(test,pred_meanf$mean) #226.2657

pred_naive<-naive(train,h=12)
rmse(pred_naive$mean,test)#102.9765

pred_snaive<-snaive(train,h=12)
rmse(pred_snaive$mean,test)#50.70832

pred_rwf<-rwf(train,h=12, drift=T)
rmse(pred_rwf$mean,test)#92.66636

pred_ses <- ses(train,h=12,initial='simple',alpha=0.2)
rmse(pred_ses$mean,test) #89.77035

pred_holt<-holt(train,h=12,damped=F,initial="simple",beta=0.65)
rmse(pred_holt$mean,test)#76.86677  without beta=0.65 it would be 84.41239

pred_hw<-hw(train,h=12,seasonal='multiplicative')
rmse(pred_hw$mean,test)#16.36156

fit<-ets(train)
accuracy(predict(fit,12),test) #24.390252


pred_stlf<-stlf(train)
rmse(pred_stlf$mean,test)#22.07215

plot(stl(train,s.window="periodic"))  #Seasonal Decomposition of Time Series by Loess

fit<-auto.arima(train)
accuracy(forecast(fit,h=12),test) #23.538735

ma = arima(train, order = c(0, 1, 3),   seasonal=list(order=c(0,1,3), period=12))
p<-predict(ma,12)
accuracy(p$pred,test)  #18.55567
BT = Box.test(ma$residuals, lag=30, type = "Ljung-Box", fitdf=2)

