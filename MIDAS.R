library(midasr)
library(stringr)
library(dplyr)
conso <- read.csv('Ressources/Consommation des ménages trimestrielle.csv', sep=';')

conso <- ts(as.double(str_replace_all(conso$TOTAL,",",".")), frequency=4, start=c(1990,1))
ytyvar <- diff(conso, differences=4)/conso
ytyvar <- ytyvar[82:length(ytyvar)]
ytyvar <- c(ytyvar)

X_cvs <- select(tbl_df(ts(read.csv('Ressources/inputdatacvs.csv'), frequency=12, start=c(2011,4))),2:7)
X_brut <- select(tbl_df(ts(read.csv('Ressources/inputdatabrut.csv'), frequency=12, start=c(2011,4))), 2:11)

y_train = ytyvar[1:25]
y_test = ytyvar[26:35]

X_brut_train = X_brut[1:75,]
X_brut_test = X_brut[76:105,]

x1 <- as.vector(as.matrix(select(X_brut_train,1)))
x2 <- as.vector(as.matrix(select(X_brut_train,2)))
x3 <- as.vector(as.matrix(select(X_brut_train,3)))
x4 <- as.vector(as.matrix(select(X_brut_train,4)))
x5 <- as.vector(as.matrix(select(X_brut_train,5)))
x6 <- as.vector(as.matrix(select(X_brut_train,6)))
x7 <- as.vector(as.matrix(select(X_brut_train,7)))
x8 <- as.vector(as.matrix(select(X_brut_train,8)))
x9 <- as.vector(as.matrix(select(X_brut_train,9)))
x10 <- as.vector(as.matrix(select(X_brut_train,10)))

model2 <- midas_u(formula=y_train~mls(y_train,1:3,1)+fmls(x1,6,3)+fmls(x2,6,3)+fmls(x3,6,3)+fmls(x4,6,3)+fmls(x5,6,3)+fmls(x6,6,3)+fmls(x7,6,3)+fmls(x8,6,3)+fmls(x9,6,3)+fmls(x10,6,3))

ndt = list(
  y_train = y_test,
  x1 = as.vector(as.matrix(select(X_brut_test,1))),
  x2 = as.vector(as.matrix(select(X_brut_test,2))),
  x3 = as.vector(as.matrix(select(X_brut_test,3))),
  x4 = as.vector(as.matrix(select(X_brut_test,4))),
  x5 = as.vector(as.matrix(select(X_brut_test,5))),
  x6 = as.vector(as.matrix(select(X_brut_test,6))),
  x7 = as.vector(as.matrix(select(X_brut_test,7))),
  x8 = as.vector(as.matrix(select(X_brut_test,8))),
  x9 = as.vector(as.matrix(select(X_brut_test,9))),
  x10 = as.vector(as.matrix(select(X_brut_test,10)))
)

forecast(model2,newdata=ndt)
summary(model3)
hf = c()
mixed = c()
for (i in 2:ncol(X_brut)){
  hf[i-1] <- select(data.frame(X_brut),i)
  mixed[[i-1]] <- fmls(hf[[i-1]],4,3)
}

model1 <- midas_u(
  ytyvar~mls(ytyvar,1:3,1)
  +mixed[[1]]
  +mixed[[2]]
  +mixed[[3]]
  +mixed[[4]]
  +mixed[[5]]
  +mixed[[6]]
  +mixed[[7]]
  +mixed[[8]]
  +mixed[[9]]
  +mixed[[10]]
  )



hf_train = c()
mixed_train = c()
for (i in 2:ncol(X_brut_train)){
  hf_train[i-1] <- select(data.frame(X_brut_train),i)
  mixed_train[[i-1]] <- fmls(hf_train[[i-1]],4,3)
}

hf_test = c()
mixed_test = c()
for (i in 2:ncol(X_brut_test)){
  hf_test[i-1] <- select(data.frame(X_brut_test),i)
  mixed_test[[i-1]] <- fmls(hf_test[[i-1]],4,3)
}

x1 = mixed_train[[1]]
x2 = mixed_train[[2]]
x3 = mixed_train[[3]]
x4 = mixed_train[[4]]
x5 = mixed_train[[5]]
x6 = mixed_train[[6]]
x7 = mixed_train[[7]]
x8 = mixed_train[[8]]
x9 = mixed_train[[9]]
x10 = mixed_train[[10]]

modeltrain = midas_u(
  y_train~mls(y_train,1:3,1)
  +mixed_train[[1]]
  +mixed_train[[2]]
  +mixed_train[[3]]
  +mixed_train[[4]]
  +mixed_train[[5]]
  +mixed_train[[6]]
  +mixed_train[[7]]
  +mixed_train[[8]]
  +mixed_train[[9]]
  +mixed_train[[10]]
  )

modeltrain2 = lm(
  y_train~mls(y_train,1:3,1)
  +mixed_train[[1]]
  +mixed_train[[2]]
  +mixed_train[[3]]
  +mixed_train[[4]]
  +mixed_train[[5]]
  +mixed_train[[6]]
  +mixed_train[[7]]
  +mixed_train[[8]]
  +mixed_train[[9]]
  +mixed_train[[10]]
)

newdata = c()
for (i in 2:(length(mixed_test)+1)){
  newdata[[i]] = mixed_test[i-1]
}
newdata[[1]] = mls(y_test,1:3,1)
forecast(modeltrain2, newdata=newdata, method="static")


