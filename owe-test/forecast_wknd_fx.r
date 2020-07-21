#! /usr/bin/Rscript

# http://robjhyndman.com/hyndsight/forecast4/
library(forecast)

setwd("/Users/rodrigo/Programs/git/oweg-sample/owe-test/owe-test")

f = read.csv(file="sample/sample.csv", header=TRUE, sep=",")
sitenames = colnames(f)[2:ncol(f)]




tr= 0.8

aarima = TRUE
p=3
q=max(nlags)
a<-rep(0,p+q)
a[p+q]=NA

a[p + nlags] = NA


s = 0
ix_end = length(sitenames)

for (ix in 1:ix_end){
  print("*******************")
  sn = sitenames[ix]

  print(sn)

#  print(paste(ix, ":", files[c(ix)]))
  
  y <- (f[sn])
  y <- c(y[[1]])
  
  nf = length(y)
  yTS= y[1:floor(nf*tr)];
  if (aarima == TRUE){
    fit<-auto.arima(yTS, max.p=50, max.q=50 ,max.P = 50, max.Q = 50, max.d=1, stationary=FALSE, seasonal=FALSE, stepwise = TRUE);
    print(fit$coef)
    print(fit)
#   print(paste("range",max(y)-min(y)))
    
  }else{
    print("Manual Parameter Selection")
 #   model = arima(yTS, order = c(p, 1, q), fixed = c(a))
#    yhat = predict(model, 1)
#    yhat$pred[1]
    for (t in floor(nf*tr):(nf-1)){ 
      yprev = y[1:t]
      ynext = y[t+1]
      model = arima(yprev, transform.pars = FALSE,order = c(p, 1L, q), fixed = c(a))
      yh_0 = predict(model,1)
      yh_1 = yh_0$pred[1]
      print("pred")
      print(yh_1)
      print("real")
      print(ynext)
      s = s + ((yh_1-ynext)^2)^0.5
    }
    n = length(floor(nf*tr) :(nf-1))
    print(s/ n)
  }
}



#notes

#p=0
#q=31
#a <- rep(0, p+q+1)
#predict(model, 3)




