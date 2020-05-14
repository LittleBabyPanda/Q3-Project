library(midasr)
library(quantreg)
library(forecast)
library(stringr)
library(dplyr)
library(rlang)
library(glue)
require(midasr)

ytyvar <- read.csv("/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/yty.csv", sep=",")
ytyvar2 <- read.csv("/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/yty.csv", sep=",")

ytyvar <- ytyvar$TOTAL[82:length(ytyvar$TOTAL)]
ytyvar <- c(ytyvar)

ytyvar2 <- ytyvar2$TOTAL

X_cvs <- ts(read.csv("/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/inputdatacvs.csv"),frequency=12, start=c(2011,4))
X_brut <- ts(read.csv('/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/inputdatabrut.csv'), frequency=12, start=c(2011,4))

X_brut2 <- ts(read.csv('/Users/croscharles/Documents/ENSAE/2A/S2/Q3-Project/Ressources/alternativebrut.csv'), frequency=12, start=c(1991,1))

y2_train <- ts(diff(ytyvar2)[1:100], frequency=4,start=c(1991,1))
y2_test <- ts(diff(ytyvar2)[101:length(diff(ytyvar2))], frequency=4, start=c(2016,1))

X_brut2_train <- diff(X_brut2)[1:300,]
X_brut2_test <- diff(X_brut2)[301:length(diff(X_brut2))]

#_____________________________________________________________________________________________

#Comparaison avec le code python ;
y_train <- structure(as.vector(diff(ytyvar)[1:25]), .Tsp = c(2011+3/4, 2017+3/4, 4), class="ts")
y_test <- structure(diff(ytyvar)[26:34], .Tsp=c(2017+3/4,2019+3/4,4), class="ts")
X_brut_train = diff(X_brut)[3:77,]
X_brut_test = diff(X_brut)[78:104,]

x1 <- structure(as.vector(X_brut_train[,2]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x2 <- structure(as.vector(X_brut_train[,3]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x3 <- structure(as.vector(X_brut_train[,4]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x4 <- structure(as.vector(X_brut_train[,5]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x5 <- structure(as.vector(X_brut_train[,6]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x6 <- structure(as.vector(X_brut_train[,7]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x7 <- structure(as.vector(X_brut_train[,8]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x8 <- structure(as.vector(X_brut_train[,9]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x9 <- structure(as.vector(X_brut_train[,10]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x10 <- structure(as.vector(X_brut_train[,11]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")

model5 <- midas_u(y_train ~ mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:1,3)+mls(x7,0:1,3)+mls(x8,0:1,3)+mls(x9,0:1,3)+mls(x10,0:1,3))
model6 <- midas_u(y_train ~ mls(y_train,1:2,1) + mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:1,3)+mls(x7,0:1,3)+mls(x8,0:1,3)+mls(x9,0:1,3)+mls(x10,0:1,3))

ndt = list(
  x1 = structure(as.vector(X_brut_test[,2]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x2 = structure(as.vector(X_brut_test[,3]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x3 = structure(as.vector(X_brut_test[,4]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x4 = structure(as.vector(X_brut_test[,5]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x5 = structure(as.vector(X_brut_test[,6]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x6 = structure(as.vector(X_brut_test[,7]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x7 = structure(as.vector(X_brut_test[,8]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x8 = structure(as.vector(X_brut_test[,9]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x9 = structure(as.vector(X_brut_test[,10]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x10 = structure(as.vector(X_brut_test[,11]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts")
)

f5 = forecast(model5,ndt)

ndt = list(
  y_train = structure(diff(ytyvar)[26:34], .Tsp=c(2017+3/4,2019+3/4,4), class="ts"),
  x1 = structure(as.vector(X_brut_test[,2]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x2 = structure(as.vector(X_brut_test[,3]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x3 = structure(as.vector(X_brut_test[,4]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x4 = structure(as.vector(X_brut_test[,5]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x5 = structure(as.vector(X_brut_test[,6]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x6 = structure(as.vector(X_brut_test[,7]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x7 = structure(as.vector(X_brut_test[,8]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x8 = structure(as.vector(X_brut_test[,9]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x9 = structure(as.vector(X_brut_test[,10]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts"),
  x10 = structure(as.vector(X_brut_test[,11]), .Tsp = c(2017+9/12,2019+11/12,12), class="ts")
)

f6 <- forecast(model6,ndt)

starting_params = list(
  x1 = rep(0,3),
  x2 = rep(0,3),
  x3 = rep(0,3),
  x4 = rep(0,3),
  x5 = rep(0,3),
  x6 = rep(0,3),
  x7 = rep(0,3),
  x8 = rep(0,3),
  x9 = rep(0,3),
  x10 = rep(0,3)
)

lag = 3

model7 <- midas_r(
  y_train ~ mls(x1,0:1,3,nealmon) 
+ mls(x1,0:lag,3,nealmon) 
+ mls(x2,0:lag,3,nealmon) 
+ mls(x3,0:lag,3,nealmon) 
+ mls(x4, 0:lag,3,nealmon) 
+ mls(x5,0:lag,3,nealmon)
+ mls(x6,0:lag,3,nealmon)
+ mls(x7,0:lag,3,nealmon)
+ mls(x8,0:lag,3,nealmon)
+ mls(x9,0:lag,3,nealmon)
+ mls(x10,0:lag,3,nealmon), start = starting_params)

f7 <- forecast(model7,ndt)

model8 <- midas_r(
  y_train ~ mls(y_train,1:2,1)
+ mls(x1,0:1,3,nealmon) 
+ mls(x1,0:lag,3,nealmon) 
+ mls(x2,0:lag,3,nealmon) 
+ mls(x3,0:lag,3,nealmon) 
+ mls(x4, 0:lag,3,nealmon) 
+ mls(x5,0:lag,3,nealmon)
+ mls(x6,0:lag,3,nealmon)
+ mls(x7,0:lag,3,nealmon)
+ mls(x8,0:lag,3,nealmon)
+ mls(x9,0:lag,3,nealmon)
+ mls(x10,0:lag,3,nealmon), start = starting_params)

f8 <- forecast(model8,ndt)
plot(f8)
y_plot <- structure(as.vector(diff(ytyvar)[1:34]), .Tsp = c(2011+2/4, 2019+3/4,4), class="ts")

pdf(file='Prédiction du MIDAS et du U-MIDAS.pdf')
plot(f8,col='blue',ylim=c(-0.02,0.02))
lines(f7$mean,col='orange')
lines(f6$mean,col='red')
lines(f5$mean,col='green')
lines(far$mean,col='yellow')
lines(y_train)
lines(y_test)
dev.off()

png(file='bidule.png')
plot(f8,col='blue',ylim=c(-0.02,0.02))
lines(f7$mean,col='orange')
lines(far$mean,col='green')
lines(y_train)
lines(y_test)
dev.off()

mse_midasu = mean((f7$mean-y_test)^2)
mse_midasr = mean((f8$mean-y_test)^2)


# On essaie sur des périodes plus longues

x1 <- structure(as.vector(X_brut_train[,2]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x2 <- structure(as.vector(X_brut_train[,3]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x3 <- structure(as.vector(X_brut_train[,4]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x4 <- structure(as.vector(X_brut_train[,5]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x5 <- structure(as.vector(X_brut_train[,6]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x6 <- structure(as.vector(X_brut_train[,7]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x7 <- structure(as.vector(X_brut_train[,8]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x8 <- structure(as.vector(X_brut_train[,9]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x9 <- structure(as.vector(X_brut_train[,10]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")
x10 <- structure(as.vector(X_brut_train[,11]), .Tsp = c(2011+6/12, 2017+8/12, 12), class="ts")