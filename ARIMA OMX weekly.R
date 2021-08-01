

#input data sample1: OMX 


mydata=data.frame(OMX_weekly)

data1=na.omit(mydata)[mydata$Date>="2007-01-01"&mydata$Date<="2009-12-31",2]
dataIn1=na.omit(mydata)[mydata$Date>="2007-01-01"&mydata$Date<="2009-06-30",2]
dataOut1=na.omit(mydata)[mydata$Date>="2009-06-30"&mydata$Date<="2009-12-31",2]



data2=na.omit(mydata)[mydata$Date>="2015-10-01"&mydata$Date<="2018-09-30",2]
dataIn2=na.omit(mydata)[mydata$Date>="2015-10-01"&mydata$Date<="2018-03-31",2]
dataOut2=na.omit(mydata)[mydata$Date>="2018-03-31"&mydata$Date<="2018-09-30",2]

#log return, 1 lag, 
priceIn1=dataIn1[,2]
logPriceIn1=log(priceIn1)
returnIn1=diff(logPriceIn1,1)

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
 

acf(returnIn2)
pacf(returnIn2)

#fit by AR
ar(returnIn1)
ar(returnIn2)


#fit by atuo.ARIMA
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
pacf(fit20$residuals)

Box.test(fit10$residuals,type="Ljung") #H0. no autocorelation
Box.test(fit20$residuals,lag=12,type="Ljung")

checkresiduals(fit10)
checkresiduals(fit20)



###### forecast using tsCV() 

      # Forecast Function and Forecast error
      ####Farima=function(x,h){forecast(arima(x,order = c(2,0,3)),h=h)}
      ####e1=tsCV(return1,Farima,h=1,window = 650)
      #test
      auto.arima(return1,approximation = FALSE,stepwise = FALSE,ic="aic")
      forecast(auto.arima(return1,approximation = FALSE,stepwise = FALSE,ic="aic"),h=1)
      tsCV(return1,h=1,window = 130)

      
      
##period 1
      w_size = 129
      n_windows = 27
#data1=na.omit(mydata)[mydata$Date>="2007-01-01"&mydata$Date<="2009-12-31",2]
logPrice1=log(data1)
return1=diff(logPrice1,1)
      
      
Farima=function(x,h){
  forecast(auto.arima(x,approximation = FALSE,stepwise = FALSE,ic="aic"),h=h)
  }
e1=tsCV(return1,Farima,h=1,window = w_size)

summary(e1)

mae1=mean(abs(e1),na.rm = TRUE)
mae1

    #note:
    #for 2 dimention 
    #mse=colMeans(e^2,na.rm = TRUE)
    #mse=mean(e^2,na.rm = TRUE)

#############For LSTAR

pmatrix1=matrix(0)

for (t in 1:n_windows)
{
  priceIn1=data1[t:(t+w_size-1)]
  logPriceIn1=log(priceIn1)
  returnIn1=diff(logPriceIn1,1)
  est=auto.arima(returnIn1,approximation = FALSE,stepwise = FALSE,ic="aic")
  helpV=rbind(pmatrix1,est$arma[1])
  pmatrix1=helpV
}

helpV=helpV[2:28,]
helpV
helpV[helpV=2]=0



#############
##period 2
      w_size = 130
      n_windows = 26

##data2=na.omit(mydata)[mydata$Date>="2015-10-01"&mydata$Date<="2018-09-30",2]

logPrice2=log(data2)
return2=diff(logPrice2)

e2=tsCV(return2,Farima,h=1,window = w_size)

summary(e2)

mae2=mean(abs(e2),na.rm = TRUE)
mae2


pmatrix2=matrix(0)

for (t in 1:n_windows)
{
  priceIn2=data2[t:(t+w_size-1)]
  logPriceIn2=log(priceIn2)
  returnIn2=diff(logPriceIn2,1)
  est=auto.arima(returnIn2,approximation = FALSE,stepwise = FALSE,ic="aic")
  helpV2=rbind(pmatrix2,est$arma[1])
  pmatrix2=helpV2
}

helpV2=helpV2[2:27,]

helpV2
helpV2[helpV2<2]=2
helpV2

###################### LSTAR2=lstar(returnIn2,m=2,d=1,steps = 1,thDelay=1,trace = TRUE,include = "const")
library(tsDyn)

fHelp2=matrix(0)

for (t in 1:n_windows){
  
  priceIn2=data2[t:(t+w_size-1)]
  logPriceIn2=log(priceIn2)
  returnIn2=diff(logPriceIn2,1)
  LSTAR2=lstar(returnIn2,m=helpV2[t],d=1,steps = 1,thDelay=1,trace = TRUE,include = "const")
  fe=rbind(fHelp2,predict(LSTAR2,n.ahead = 1)[1])
  fHelp2=fe
}
fHelp2

out2=return2[131:156]
out2

t=fHelp2

forecastError2=mean(abs(out2-fHelp2[2:27]))
forecastError2







  




