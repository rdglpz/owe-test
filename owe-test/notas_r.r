library(forecast)

pth="~/git/oweg-sample/owe-test/owe-test/sample/sample.csv"



Y <- read.csv(file=pth, header=TRUE, sep=",")


CUA        FAC       MER        MON    PED        SAG       TAH    TLA    UIZ


tr = 0.6

y = Y$TLA

yTS= y[1:floor(nf*tr)]


fit<-auto.arima(yTS, max.p=50, max.q=50 ,max.P = 50, max.Q = 50, max.d=1, stationary=FALSE, seasonal=FALSE);




