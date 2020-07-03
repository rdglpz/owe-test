#! /usr/bin/Rscript

# http://robjhyndman.com/hyndsight/forecast4/
library(forecast)

#folder = "/home/rodrigo/Dropbox/wknd-fx python notebooks/weekend-fx/";
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.html

#setwd("/Users/rodrigolopez/Dropbox/Paper weekend effect global/predictability_scripts")
files = c(
  "o3_CUA_y.csv", 
  "o3_FAC_y.csv", 
  "o3_MER_y.csv", 
  "o3_MON_y.csv",
  "o3_PED_y.csv",
  "o3_SAG_y.csv", 
  "o3_TAH_y.csv", 
  "o3_TLA_y.csv", 
  "o3_UIZ_y.csv",
  "o3_XAL_y.csv"
)
path = "/home/rodrigo/Dropbox/wknd-fx python notebooks/weekend-fx/"
path = "/Users/rodrigolopez/Dropbox/Paper weekend effect global/predictability_scripts/"
tr= 0.6;
nlags = c(5,6,22,23,26,27,28,29,30,31)
nlags = c(22)
nlags=c(21:21)
nlags=c(1:17)
aarima=TRUE
p=3
q=max(nlags)
a<-rep(0,p+q)
a[p+q]=NA

a[p + nlags] = NA

#a[p + nlags+1] = NA

#a[p+q]=NA
#a[q-1]=NA
#a[q-2]=NA

s = 0
ix_end = length(files)

for (ix in 1:ix_end){
  print("*******************")	
  print(paste(ix, ":", files[c(ix)]))
  pth = paste(path,files[c(ix)], sep = "")
  Y <- read.csv(file=pth, header=FALSE, sep=",")
  y = Y$V2
  
  nf = length(y)
  yTS= y[1:floor(nf*tr)];
  if (aarima == TRUE){
    fit<-auto.arima(yTS, max.p=50, max.q=50 ,max.P = 50, max.Q = 50, max.d=1, stationary=FALSE, seasonal=FALSE);
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




