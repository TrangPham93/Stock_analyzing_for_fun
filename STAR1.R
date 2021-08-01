
library(tsDyn)

######AR suggest AR(0), using arima 
##au1=ar(returnIn,method = "mle")
##au1
##summary(au1)


LSTAR1=lstar(return1,m=2,d=1,steps = 1,thDelay = 1,trace = TRUE,include = "const")
summary(LSTAR1)

residuals(LSTAR1)


####AIC(LSTAR1)
plot(LSTAR1)


#####automatic selection of model hyper parameters
#selectLSTAR(returnIn1, m=2)




#threshold nonlinearity test
#Null hypothesis: no threshold nonlinearity (simple AR)

###########
w_size = 650
n_windows = 132
pmatrix=matrix(0)

for (t in 1:n_window)
{
  priceIn1=data1[t:(t+650)]
  logPriceIn1=log(priceIn1)
  returnIn1=diff(logPriceIn1,1)
  est=auto.arima(x,approximation = FALSE,stepwise = FALSE,ic="aic")
  helpV=rbind(pmatrix,est$arma[1])
  pmatrix=helpV
}
helpV=helpV[2:133]

####################

w_size = 130
n_windows = 25
pmatrix=matrix(0)

for (t in 1:n_windows)
{
  priceIn1=data1[t:(t+130)]
  logPriceIn1=log(priceIn1)
  returnIn1=diff(logPriceIn1,1)
  est=auto.arima(returnIn1,approximation = FALSE,stepwise = FALSE,ic="aic")
  helpV=rbind(pmatrix,est$arma[1])
  pmatrix=helpV
}
helpV=helpV[2:26]
helpV
#######Forecast out-of-sample, rolling window

w_size=130
n_window=25
data1=mydata[mydata$Name>="2007-01-01"&mydata$Name<="2009-12-31",2]
fore1=matrix(0)

for (t in 1:n_window){

      priceIn1=data1[t:(t+w_size)]
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







