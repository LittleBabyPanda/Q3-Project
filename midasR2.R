library(midasr)
library(quantreg)
library(forecast)
library(stringr)
library(dplyr)
library(rlang)
library(glue)
require(midasr)


ytyvar2 <- read.csv("/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/yty.csv", sep=",")
ytyvar2 <- diff(ytyvar2$TOTAL)

ytyvar2ts <- ts(ytyvar2, frequency=4, start=c(1991,2))
ytyvar2ts <- ytyvar2ts[4:length(ytyvar2ts)]

X_brut2 <- ts(read.csv('/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/alternativebrut.csv'), frequency=12, start=c(1991,1))
X_brut2 <- diff(X_brut2, lag=12)/X_brut2

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
    x1 = ts(X_brut2[301:336,1], frequency=12, start = c(2017,1)),
    x2 = ts(X_brut2[301:336,2], frequency=12, start = c(2017,1)),
    x3 = ts(X_brut2[301:336,3], frequency=12, start = c(2017,1)),
    x4 = ts(X_brut2[301:336,4], frequency=12, start = c(2017,1)),
    x5 = ts(X_brut2[301:336,5], frequency=12, start = c(2017,1)),
    x6 = ts(X_brut2[301:336,6], frequency=12, start = c(2017,1)),
    x7 = ts(X_brut2[301:336,7], frequency=12, start = c(2017,1))
)


model <- lm(y2_train ~ mls(y2_train,1:2,1) + mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:1,3)+mls(x7,0:1,3))
summary(model)

forecast(model,ndt)

