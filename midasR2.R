library(midasr)
library(quantreg)
library(forecast)
library(stringr)
library(dplyr)
library(rlang)
library(glue)
library(greybox)

ytyvar2 <- read.csv("/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/yty.csv", sep=",")
ytyvar2 <- diff(ytyvar2$TOTAL)

ytyvar2ts <- ts(ytyvar2, frequency=4, start=c(1991,2))
ytyvar2ts <- ytyvar2ts[4:length(ytyvar2ts)] # Commence en janvier 92

X_brut2 <- ts(read.csv('/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/alternativebrut.csv'), frequency=12, start=c(1991,1))
X_brut2 <- diff(X_brut2, lag=12)/X_brut2 # Commence en janvier 92

y2_train <- ts(ytyvar2ts[1:100], frequency=4,start=c(1992,1))
y2_test <- ts(ytyvar2[100:length(diff(ytyvar2ts))], frequency=4, start=c(2017,1))

x1 <- ts(X_brut2[1:300,1], frequency=12, start = c(1992,1))
x2 <- ts(X_brut2[1:300,2], frequency=12, start = c(1992,1))
x3 <- ts(X_brut2[1:300,3], frequency=12, start = c(1992,1))
x4 <- ts(X_brut2[1:300,4], frequency=12, start = c(1992,1))
x5 <- ts(X_brut2[1:300,5], frequency=12, start = c(1992,1))
x6 <- ts(X_brut2[1:300,6], frequency=12, start = c(1992,1))
x7 <- ts(X_brut2[1:300,7], frequency=12, start = c(1992,1))

ndt <- list(
    y2_train = y2_test,
    x1 = ts(X_brut2[301:336,1], frequency=12, start = c(2017,1)),
    x2 = ts(X_brut2[301:336,2], frequency=12, start = c(2017,1)),
    x3 = ts(X_brut2[301:336,3], frequency=12, start = c(2017,1)),
    x4 = ts(X_brut2[301:336,4], frequency=12, start = c(2017,1)),
    x5 = ts(X_brut2[301:336,5], frequency=12, start = c(2017,1)),
    x6 = ts(X_brut2[301:336,6], frequency=12, start = c(2017,1)),
    x7 = ts(X_brut2[301:336,7], frequency=12, start = c(2017,1))
)

p = 2

model <- midas_u(y2_train ~ mls(y2_train,1:2,1) + mls(x1,0:p,3) + mls(x2,0:p,3) +  mls(x3,0:p,3) + mls(x4, 0:p,3) + mls(x5,0:p,3)+mls(x6,0:p,3)+mls(x7,0:p,3), start=NULL)

params = c(2,2,0)

starting_params = list(
  x1 = params,
  x2 = params,
  x3 = params,
  x4 = params,
  x5 = params,
  x6 = params,
  x7 = params
)

model2 <- midas_r(y2_train ~ mls(y2_train,1:8,1) + mls(x1,0:p,3,nealmon) + mls(x2,0:p,3,nealmon) +  mls(x3,0:p,3,nealmon) + mls(x4, 0:p,3,nealmon) + mls(x5,0:p,3,nealmon)+mls(x6,0:p,3,nealmon)+mls(x7,0:p,3,nealmon), start=starting_params)

arrecursive = c()
arrolling = c()

for (i in 1:12){
    arrecursive[i] = predict(arima(ytyvar2ts[1:99+i], order=c(8,0,0)))$pred
    arrolling[i] = predict(arima(ytyvar2ts[i:99+i], order=c(8,0,0)))$pred
}

arima(ytyvar2ts[1:100], order=c(8,0,0))$sigma2
mean(arima(ytyvar2ts[1:100], order=c(8,0,0))$residuals^2)

avg_fcst1 <- average_forecast(list(model2), data=list(
    y2_train = ytyvar2ts,
    x1 = c(X_brut2[,1]),
    x2 = c(X_brut2[,2]),
    x3 = c(X_brut2[,3]),
    x4 = c(X_brut2[,4]),
    x5 = c(X_brut2[,5]),
    x6 = c(X_brut2[,6]),
    x7 = c(X_brut2[,7])
),
insample=1:100,
outsample=101:112,
type='fixed')

avg_fcst2 <- average_forecast(list(model2), data=list(
    y2_train = ytyvar2ts,
    x1 = c(X_brut2[,1]),
    x2 = c(X_brut2[,2]),
    x3 = c(X_brut2[,3]),
    x4 = c(X_brut2[,4]),
    x5 = c(X_brut2[,5]),
    x6 = c(X_brut2[,6]),
    x7 = c(X_brut2[,7])
),
insample=1:100,
outsample=101:112,
type='recursive')

avg_fcst3 <- average_forecast(list(model2), data=list(
    y2_train = ytyvar2ts,
    x1 = c(X_brut2[,1]),
    x2 = c(X_brut2[,2]),
    x3 = c(X_brut2[,3]),
    x4 = c(X_brut2[,4]),
    x5 = c(X_brut2[,5]),
    x6 = c(X_brut2[,6]),
    x7 = c(X_brut2[,7])
),
insample=1:100,
outsample=101:112,
type='rolling')

# Graphique des différentes prévisions sur le test

png('Images/fcsts2.png')
plot(avg_fcst1$forecast, ylim=c(-0.02,0.02), col='red', xlab='Temps', ylab='Variation de la consommation')
points(avg_fcst2$forecast, col='blue')
points(avg_fcst3$forecast, col='orange')
points(arrecursive, pch=19)
points(arrolling, pch=19, col='purple')
lines(ytyvar2ts[101:112])
legend('topright', inset=0.01,legend=c('Statique','Récursif','Roulant','AR(8) récursif'),cex=1,pch=c(1,1,1,19),col=c('red','blue','orange','black'))
dev.off()

# Barplots des MSEs et des MAPEs

png('Images/fcsts2mses.png')
par(las=2)
barplot(
    c(mean(model2$residuals^2),
    avg_fcst1$accuracy$individual$MSE.out.of.sample,
    avg_fcst2$accuracy$individual$MSE.out.of.sample,
    avg_fcst3$accuracy$individual$MSE.out.of.sample,
    arima(ytyvar2ts[1:100], order=c(8,0,0))$sigma2,
    mean((arrecursive - ytyvar2ts[101:112])^2),
    mean((arrolling - ytyvar2ts[101:112])^2)),

    names.arg=c("MIDAS Train",
                "MIDAS Statique Test", 
                "MIDAS Récursif Test",
                "MIDAS Rolling Test",
                "AR(8) Train",
                "AR(8) Récursif Test",
                "AR(8) Rolling Test"),
    
    main="MSE des différents modèles"
)
dev.off()

png('Images/fcsts2mapes.png')
barplot(
    c(avg_fcst1$accuracy$individual$MAPE.in.sample,
    avg_fcst1$accuracy$individual$MAPE.out.of.sample,
    avg_fcst2$accuracy$individual$MAPE.out.of.sample,
    avg_fcst3$accuracy$individual$MAPE.out.of.sample,
    mean(abs(arima(ytyvar2ts[1:100], order=c(8,0,0))$residuals)/ytyvar2ts[1:100]),
    mean(abs(arrecursive / ytyvar2ts[101:112] - 1)),
    mean(abs(arrolling / ytyvar2ts[101:112] - 1))),

    names.arg=c("MIDAS Train",
                "MIDAS Statique Test", 
                "MIDAS Récursif Test",
                "MIDAS Rolling Test",
                "AR(8) Train",
                "AR(8) Récursif Test",
                "AR(8) Rolling Test"),
    
    main="MAPE des différents modèles"
)
dev.off()