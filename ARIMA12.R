

#input data sample1: OMX crisis period
library(readxl)
OMX_daily <- read_excel("//home.org.aalto.fi/trangp1/data/Downloads/thesis/run r/dataRun/OMX daily.xlsx")
View(OMX_daily)
class(OMX_daily)
head(OMX_daily)
summary(OMX_daily)



data1=na.omit(mydata)[mydata$Date>="2007-01-01"&mydata$Date<="2009-12-31",]
dataIn1=data1[mydata$Date>="2007-01-01"&mydata$Date<="2009-06-30",]
dataOut1=data1[mydata$Date>="2009-06-30"&mydata$Date<="2009-12-31",]



data2=na.omit(mydata)[mydata$Date>="2015-10-01"&mydata$Date<="2018-09-30",]
dataIn2=na.omit(mydata)[mydata$Date>="2015-10-01"&mydata$Date<="2018-03-31",]
dataOut2=na.omit(mydata)[mydata$Date>="2018-03-31"&mydata$Date<="2018-09-30",]

#log return, 1 lag, 
priceIn1=dataIn1[,2]
logPriceIn1=log(priceIn1)
returnIn1=diff(logPriceIn1,1)
returnIn1=diff(log(priceIn1))

priceOut1=dataOut1[,2]
logPriceOut1=log(priceOut1)
returnOut1=diff(logPriceOut1,1)


priceIn2=dataIn2[,2]
logPriceIn2=log(priceIn2)
returnIn2=diff(logPriceIn2,1)

priceOut2=dataOut2[,2]
logPriceOut2=log(priceOut2)
returnOut2=diff(logPriceOut2,1)


#Plot data
plot.ts(priceIn1)
plot.ts(returnIn1)

plot.ts(priceIn2)
plot.ts(returnIn2)

acf(returnIn1)
pacf(returnIn1)
 returnIn1

acf(returnIn2)
pacf(returnIn2)
#fit by AR
ar(returnIn1)
ar(returnIn2)


#fit by ARIMA
library("forecast", lib.loc="C:/Program Files/Microsoft/R Open/R-3.5.1/library")
fit10=auto.arima(returnIn1,approximation=FALSE,stepwise=FALSE,ic="aic")
summary(fit10) 
 #->fit1:approximation and stepwise set to false, auto.arima() automates the inclusion of constant 

  #--> aic choose arima(2,0,3), bic choose arima(0,0,0)



fit20=auto.arima(returnIn2,approximation=FALSE,stepwise=FALSE,ic="aic")
summary(fit20)
 #auto.arima(logPriceIn1)#check again d=1

#check residuals

acf(fit10$residuals)
pacf(fit11$residuals)

Box.test(fit10$residuals,type="Ljung") #H0. no autocorelation
Box.test(fit11$residuals,lag=12,type="Ljung")

checkresiduals(fit10)
checkresiduals(fit11)



###### forecast using tsCV() 
dataForecast1=na.omit(mydata)[mydata$Date>="2007-01-01"&mydata$Date<="2009-12-31",2]
logPrice1=log(dataForecast1)
return1=diff(logPrice1)


    #dataForecast1 783
    #logPrice1  783
    #return1  782

    # = Number of windows and window size
    #w_size = 650
    #n_windows = 132 (out of sample forecast size)

# Forecast Function and Forecast error
####Farima=function(x,h){forecast(arima(x,order = c(2,0,3)),h=h)}
####e1=tsCV(return1,Farima,h=1,window = 650)
auto.arima(return1,approximation = FALSE,stepwise = FALSE,ic="aic")
forecast(auto.arima(return1,approximation = FALSE,stepwise = FALSE,ic="aic"),h=1)
tsCV(return1,h=1,window = 130)


Farima=function(x,h){
  forecast(auto.arima(x,approximation = FALSE,stepwise = FALSE,ic="aic"),h=h)
  }
e1=tsCV(return1,Farima,h=1,window = 130)

summary(e1)

mae1=mean(abs(e1),na.rm = TRUE)
mae1

    #note:
    #for 2 dimention 
    #mse=colMeans(e^2,na.rm = TRUE)
    #mse=mean(e^2,na.rm = TRUE)

#############For LSTAR
w_size = 130
n_windows = 25
pmatrix=matrix(0)

for (t in 1:n_windows)
{
  priceIn1=data1[t:(t+w_size),2]
  logPriceIn1=log(priceIn1)
  returnIn1=diff(logPriceIn1,1)
  est=auto.arima(returnIn1,approximation = FALSE,stepwise = FALSE,ic="aic")
  helpV=rbind(pmatrix,est$arma[1])
  pmatrix=helpV
}

helpV=helpV[2:25,0]
helpV

row=5
helpV[10]
for (i in 1:25) {
 if(helpV[i]==0) {
  helpV[i]<-2
  }}
helpV




  
  t=matrix(1:5)
for (t in t){
  print(t+1)
}


######################

data1=mydata[mydata$Name>="2007-01-01"&mydata$Name<="2009-12-31",2]
fore1=matrix(0)

for (t in 1:n_windows){
  
  priceIn1=data1[t:(t+w_size),2]
  logPriceIn1=log(priceIn1)
  returnIn1=diff(logPriceIn1,1)
  LSTAR1=lstar(returnIn1,m=helpV[t],d=1,steps = 1,thDelay = 1,trace = TRUE,include = "const")
  e=rbind(fore1,predict(LSTAR1,n.ahead = 1)[1])
  fore1=e
}
fore1

fore1[2:133]
forecastError=mean(abs(returnOut1-fore1[2:133]))
forecastError







############### below is not used


#forecast

fore1=forecast(fit10,h=1)
summary(fore1)
accuracy(fore1,dataOut)

autoplot(fore1)

autoplot(fit10)



  




