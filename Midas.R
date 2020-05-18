library(midasr,quantreg,forecast)
library(stringr)
library(dplyr)
library(rlang)
install.packages("rlang")
yes
library(glue)
require(midasr)

ytyvar <- read.csv("C:/Users/trist/Document ordinateur/Q3-Project/Ressources/yty.csv", sep=",")

ytyvar <- ytyvar$TOTAL[82:length(ytyvar$TOTAL)]
ytyvar <- c(ytyvar)

X_cvs <- ts(read.csv("C:/Users/trist/Document ordinateur/Q3-Project/Ressources/inputdatacvs.csv"),frequency=12, start=c(2011,4))
X_brut <- ts(read.csv('C:/Users/trist/Document ordinateur/Q3-Project/Ressources/inputdatabrut.csv'), frequency=12, start=c(2011,4))

y_test <- structure(ytyvar[32:35], .Tsp=c(2019+0/4,2019+3/4,4), class="ts")

#prévision du dernier trimestre:
y_train = ytyvar[1:34] #on va jusqu'à 2019T3

X_brut_train = X_brut[1:102,] #on va jusqu'à septembre 2019
X_brut_test = X_brut[103:105,]

x1 <- structure(as.vector(X_brut_train[,2]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x2 <- structure(as.vector(X_brut_train[,3]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x3 <- structure(as.vector(X_brut_train[,4]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x4 <- structure(as.vector(X_brut_train[,5]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x5 <- structure(as.vector(X_brut_train[,6]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x6 <- structure(as.vector(X_brut_train[,7]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x7 <- structure(as.vector(X_brut_train[,8]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x8 <- structure(as.vector(X_brut_train[,9]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x9 <- structure(as.vector(X_brut_train[,10]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
x10 <- structure(as.vector(X_brut_train[,11]), .Tsp = c(2011+3/12, 2019+8/12, 12), class="ts")
y_train <- structure(as.vector(ytyvar[1:34]), .Tsp = c(2011+1/4, 2019+2/4, 4), class="ts")

model1 <- midas_u(y_train ~ mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:1,3)+mls(x7,0:1,3)+mls(x8,0:1,3)+mls(x9,0:1,3)+mls(x10,0:1,3))

ndt = list(
  x1 = structure(as.vector(X_brut_test[,2]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x2 = structure(as.vector(X_brut_test[,3]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x3 = structure(as.vector(X_brut_test[,4]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x4 = structure(as.vector(X_brut_test[,5]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x5 = structure(as.vector(X_brut_test[,6]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x6 = structure(as.vector(X_brut_test[,7]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x7 = structure(as.vector(X_brut_test[,8]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x8 = structure(as.vector(X_brut_test[,9]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x9 = structure(as.vector(X_brut_test[,10]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts"),
  x10 = structure(as.vector(X_brut_test[,11]), .Tsp = c(2019+9/12,2019+11/12,12), class="ts")
)

f1 <- forecast(model1,ndt)


#prévision des 2 derniers trimestres:
y_train = ytyvar[1:33] #on va jusqu'à 2019T3

X_brut_train = X_brut[1:99,] #on va jusqu'à septembre 2019
X_brut_test = X_brut[100:105,]

x1 <- structure(as.vector(X_brut_train[,2]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x2 <- structure(as.vector(X_brut_train[,3]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x3 <- structure(as.vector(X_brut_train[,4]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x4 <- structure(as.vector(X_brut_train[,5]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x5 <- structure(as.vector(X_brut_train[,6]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x6 <- structure(as.vector(X_brut_train[,7]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x7 <- structure(as.vector(X_brut_train[,8]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x8 <- structure(as.vector(X_brut_train[,9]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x9 <- structure(as.vector(X_brut_train[,10]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
x10 <- structure(as.vector(X_brut_train[,11]), .Tsp = c(2011+3/12, 2019+5/12, 12), class="ts")
y_train <- structure(as.vector(ytyvar[1:33]), .Tsp = c(2011+1/4, 2019+1/4, 4), class="ts")

model2 <- midas_u(y_train ~ mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:1,3)+mls(x7,0:1,3)+mls(x8,0:1,3)+mls(x9,0:1,3)+mls(x10,0:1,3))

ndt = list(
  x1 = structure(as.vector(X_brut_test[,2]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x2 = structure(as.vector(X_brut_test[,3]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x3 = structure(as.vector(X_brut_test[,4]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x4 = structure(as.vector(X_brut_test[,5]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x5 = structure(as.vector(X_brut_test[,6]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x6 = structure(as.vector(X_brut_test[,7]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x7 = structure(as.vector(X_brut_test[,8]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x8 = structure(as.vector(X_brut_test[,9]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x9 = structure(as.vector(X_brut_test[,10]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts"),
  x10 = structure(as.vector(X_brut_test[,11]), .Tsp = c(2019+6/12,2019+11/12,12), class="ts")
)

f2 <- forecast(model2,ndt)




#prévisions des 3 derniers trimestres:
y_train = ytyvar[1:32] #on va jusqu'à 2019T3

X_brut_train = X_brut[1:96,] #on va jusqu'à septembre 2019
X_brut_test = X_brut[97:105,]

x1 <- structure(as.vector(X_brut_train[,2]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x2 <- structure(as.vector(X_brut_train[,3]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x3 <- structure(as.vector(X_brut_train[,4]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x4 <- structure(as.vector(X_brut_train[,5]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x5 <- structure(as.vector(X_brut_train[,6]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x6 <- structure(as.vector(X_brut_train[,7]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x7 <- structure(as.vector(X_brut_train[,8]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x8 <- structure(as.vector(X_brut_train[,9]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x9 <- structure(as.vector(X_brut_train[,10]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
x10 <- structure(as.vector(X_brut_train[,11]), .Tsp = c(2011+3/12, 2019+2/12, 12), class="ts")
y_train <- structure(as.vector(ytyvar[1:32]), .Tsp = c(2011+1/4, 2019+0/4, 4), class="ts")

model3 <- midas_u(y_train ~ mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:2,3)+mls(x7,0:1,3)+mls(x8,0:1,3)+mls(x9,0:1,3)+mls(x10,0:1,3))

ndt = list(
  x1 = structure(as.vector(X_brut_test[,2]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x2 = structure(as.vector(X_brut_test[,3]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x3 = structure(as.vector(X_brut_test[,4]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x4 = structure(as.vector(X_brut_test[,5]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x5 = structure(as.vector(X_brut_test[,6]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x6 = structure(as.vector(X_brut_test[,7]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x7 = structure(as.vector(X_brut_test[,8]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x8 = structure(as.vector(X_brut_test[,9]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x9 = structure(as.vector(X_brut_test[,10]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts"),
  x10 = structure(as.vector(X_brut_test[,11]), .Tsp = c(2019+3/12,2019+11/12,12), class="ts")
)

f3 <- forecast(model3,ndt)



#Prévisions des 4 derniers trimestres:
y_train = ytyvar[1:30] #on va jusqu'à 2019T3

X_brut_train = X_brut[1:93,] #on va jusqu'à septembre 2019
X_brut_test = X_brut[94:105,]

x1 <- structure(as.vector(X_brut_train[,2]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x2 <- structure(as.vector(X_brut_train[,3]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x3 <- structure(as.vector(X_brut_train[,4]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x4 <- structure(as.vector(X_brut_train[,5]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x5 <- structure(as.vector(X_brut_train[,6]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x6 <- structure(as.vector(X_brut_train[,7]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x7 <- structure(as.vector(X_brut_train[,8]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x8 <- structure(as.vector(X_brut_train[,9]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x9 <- structure(as.vector(X_brut_train[,10]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
x10 <- structure(as.vector(X_brut_train[,11]), .Tsp = c(2011+3/12, 2018+11/12, 12), class="ts")
y_train <- structure(as.vector(ytyvar[1:31]), .Tsp = c(2011+1/4, 2018+3/4, 4), class="ts")

model4 <- midas_u(y_train ~ mls(x1,0:1,3) + mls(x2,0:1,3) +  mls(x3,0:1,3) + mls(x4, 0:1,3) + mls(x5,0:1,3)+mls(x6,0:1,3)+mls(x7,0:1,3)+mls(x8,0:1,3)+mls(x9,0:1,3)+mls(x10,0:1,3))

ndt = list(
  x1 = structure(as.vector(X_brut_test[,2]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x2 = structure(as.vector(X_brut_test[,3]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x3 = structure(as.vector(X_brut_test[,4]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x4 = structure(as.vector(X_brut_test[,5]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x5 = structure(as.vector(X_brut_test[,6]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x6 = structure(as.vector(X_brut_test[,7]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x7 = structure(as.vector(X_brut_test[,8]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x8 = structure(as.vector(X_brut_test[,9]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x9 = structure(as.vector(X_brut_test[,10]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts"),
  x10 = structure(as.vector(X_brut_test[,11]), .Tsp = c(2019+0/12,2019+11/12,12), class="ts")
)

f4 <- forecast(model4,ndt)


y_test;f1;f2;f3;f4

