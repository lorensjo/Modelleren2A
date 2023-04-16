# R Script. Modelleren 2A.
data = read.csv("Battery_train.csv")
num = length(data)-1

test = read.csv("Battery_test.csv")


################################################################################
# Functions.
################################################################################

# Get the lifetime of the battery.
get_lifetime = function(cap) {
  return (match(min(cap, na.rm=TRUE), cap)-1)
}

get_RUL_data = function(cap) {
  life = get_lifetime(cap)
  return (life-data$Cycle)
}

convert_to_log = function(rul) {
  for (index in 1:length(rul)) {
    if (rul[index] < 1) {
      rul[index] = NA
    }
    else {
      rul[index] = log(rul[index])
    }
  }
  return (rul)
}


################################################################################
# Take all data
################################################################################
#create a vector a with all capacities from training data
#and a vector b with the corresponding RULs
a=c()
b=c()
for (i in 2:num) {
  cap = data[,i]
  rul = get_RUL_data(cap)
  for (j in 1:length(cap)){
    if (is.na(cap[j])==FALSE){ 
      a <- append(a,cap[j])
      b <- append(b,rul[j])
    }
  }
}

#create a vector d with all capacities from test data
#and a vector e with the corresponding RULs
d=c()
e=c()
for (i in 2:length(test)) {
  cap = test[,i]
  rul = get_RUL_data(cap)
  for (j in 1:length(cap)){
    if (is.na(cap[j])==FALSE){ 
      d <- append(d,cap[j])
      e <- append(e,rul[j])
    }
  }
}

#################################
f = formula("y ~ poly(x, degree=5, raw = TRUE)")
variables = data.frame(x=a, y=b)
mylm = lm(f,variables)

xlist = seq(0.85,1.10,by=0.001)
newdata = data.frame(x=xlist)
ynew = predict.lm(mylm, newdata)

# extension
#################################
# now we shall try the model ln(RUL) = a + b*CAP + error
# again we base this model fit on the training data and compute the erro for the testing data.

# first convert RUL to logRUL data.
logRUL = convert_to_log(b)
# logRul = a + b*CAP + error

g = formula("y ~ poly(x, degree=5, raw = TRUE)")
variables = data.frame(x=a, y=logRUL)
logmodel = lm(g,variables)

# thus RUL = e^(a + b*CAP + e)
cap_axis = seq(0.85,1.10,by=0.001)
beta = logmodel$coefficients
plot(d,e,cex=0.01,main="Prediction and realisation of RUL from test data",xlab="Capacity Ah",ylab="RUL")
lines(cap_axis, exp(beta[1]+beta[2]*cap_axis+beta[3]*cap_axis^2+beta[4]*cap_axis^3+beta[5]*cap_axis^4+beta[6]*cap_axis^5), col="red")  #theoretical line based on the fit of the model

mean(logmodel$residuals^2)

#intervals prediction
xlist = seq(0.85,1.10,by=0.001)
ci_95 = predict.lm(logmodel,newdata=data.frame(x=xlist),interval="prediction", level = 0.90)
ci_80 = predict.lm(logmodel,newdata=data.frame(x=xlist),interval="prediction", level = 0.50)
# Plotting CI's
lines(xlist, exp(ci_95[,2]), col="blue", lty=2)
lines(xlist, exp(ci_95[,3]), col="blue", lty=2)
lines(xlist, exp(ci_80[,2]), col="lightblue", lty=2)
lines(xlist, exp(ci_80[,3]), col="lightblue", lty=2)


# determine percentages in CIs
count = 0
count90 = 0
for (index in 1:length(d) ) {
  cap = d[index]
  rul = e[index]
  #first find position j of cap in xlist so to speak
  for (j in 1:length(xlist) ) {
    if (cap <= xlist[j]) {
      break
    }
  }
  # Now if the real RUl value is within interval, add to count
  if (exp(ci_80[,2][j]) <= rul && rul <= exp(ci_80[,3][j])) {
    count = count + 1}
  if (exp(ci_95[,2][j]) <= rul && rul <= exp(ci_95[,3][j])) {
      count90 = count90 + 1
    }
}
percentage50 = count/length(d)
perc90 = count90/length(d)

# Let´s compare our results, first for training data:

#1. Log-MSE for Log-model
#training-data
mean(logmodel$residuals^2)
#test-data
mset=0
countje = 0
for (i in 1:length(e)){
  if (e[i]>0){
    countje = countje + 1
    cur_cap2=d[i]
    pred=logmodel$coefficients[1]
    for (j in 1:5){
      pred=pred+(logmodel$coefficients[j+1])*(cur_cap2)^j
    }
    addmse=(log(e[i])-pred)^2
    mset=mset+addmse
  }
}
mset=mset/countje
mset
length(e)-countje

#2. Log-MSE for poly-model
#training-data
mselp=0
countlp=0
for (i in 1:length(b)){
  if (b[i]>0){
    cur_cap2=a[i]
    pred=mylm$coefficients[1]
    for (j in 1:5){
      pred=pred+(mylm$coefficients[j+1])*(cur_cap2)^j
    }
    if (pred>0){
      addmselp=(log(b[i])-log(pred))^2
      mselp=mselp+addmselp
      countlp=countlp+1
    } 
  }
}
mselp=mselp/countlp
mselp
length(b)-countlp

#test-data
mselpt=0
countlpt=0
for (i in 1:length(e)){
  if (e[i]>0){
    cur_cap2=d[i]
    pred=mylm$coefficients[1]
    for (j in 1:5){
      pred=pred+(mylm$coefficients[j+1])*(cur_cap2)^j
    }
    if (pred>0){
      addmselp=(log(e[i])-log(pred))^2
      mselpt=mselpt+addmselp
      countlpt=countlpt+1
    } 
  }
}
mselpt=mselpt/countlpt
mselpt
length(e)-countlpt

#3 poly-MSE for log-model
#training-data
mse = 0
for (i in 1:length(b)){
  cur_cap=a[i]
  pred=logmodel$coefficients[1]
  for (j in 1:5){
    pred=pred+(logmodel$coefficients[j+1])*(cur_cap)^j
  }
  addmse=(b[i]-exp(pred))^2
  mse=mse+addmse
}
mse=mse/length(b)
mse

#test-data
mset = 0
for (i in 1:length(e)){
  cur_cap=d[i]
  pred=logmodel$coefficients[1]
  for (j in 1:5){
    pred=pred+(logmodel$coefficients[j+1])*(cur_cap)^j
  }
  addmse=(e[i]-exp(pred))^2
  mset=mset+addmse
}
mset=mset/length(e)
mset

#4 poly-MSE for poly-model
#training-data
mean(mylm$residuals^2)
#test-data
msept=0
coutn = 0
for (i in 1:length(e)){
  if (e[i]>0){
    coutn = coutn + 1
    cur_cap2=d[i]
    pred=mylm$coefficients[1]
    for (j in 1:5){
      pred=pred+(mylm$coefficients[j+1])*(cur_cap2)^j
    }
    addmse=(e[i]-pred)^2
    msept=msept+addmse
  }
}
msept=msept/coutn
msept
length(e)-coutn

#5 log-MAPE for log-model
#training data
count=0
mape=0
for (i in 1:length(b)){
  if (b[i]>0){
    hold=log(b[i])
    if ((hold>0) &  (is.na(logmodel$residuals[i])==FALSE)){
      count=count+1
      res=logmodel$residuals[i]
      add=abs(res)/abs(log(b[i]))
      mape=mape+add
    }
  }
}
mape=mape/(count)                 
mape

#test-data
countt=0
mapett=0
for (i in 1:length(e)){
  if (e[i]>0){
    hold=log(e[i])
    if ((hold>0)){
      countt=countt+1
      cur_cap2=d[i]
      pred=logmodel$coefficients[1]
      for (j in 1:5){
        pred=pred+(logmodel$coefficients[j+1])*(cur_cap2)^j
      }
      add=abs(log(e[i])-pred)/abs(log(e[i]))
      mapett=mapett+add
    }
  }
}
mapett=mapett/(countt)                 
mapett
length(e)-countt

#6 log-MAPE for poly-model
#training-data
count6=0
mape6=0
for (i in 1:length(b)){
  cur_cap2=a[i]
  if (b[i]>0){
    hold=log(b[i])
    if ((hold>0)){
      pred=mylm$coefficients[1]
      for (j in 1:5){
        pred=pred+(mylm$coefficients[j+1])*(cur_cap2)^j
      }
      if (pred>0){
        add=abs(log(b[i])-log(pred))/abs(log(b[i]))
        mape6=mape6+add
        count6=count6+1
      }
    }
  }
}
mape6=mape6/(count6)                 
mape6
length(b)-count6

#test-data
count6t=0
mape6t=0
for (i in 1:length(e)){
  cur_cap2=d[i]
  if (e[i]>0){
    hold=log(e[i])
    if ((hold>0)){
      pred=mylm$coefficients[1]
      for (j in 1:5){
        pred=pred+(mylm$coefficients[j+1])*(cur_cap2)^j
      }
      if (pred>0){
        add=abs(log(e[i])-log(pred))/abs(log(e[i]))
        mape6t=mape6t+add
        count6t=count6t+1
      }
    }
  }
}
mape6t=mape6t/(count6t)                 
mape6t
length(e)-count6t

#7. poly-MAPE for log-model
#training-data:
count7=0
mape7=0
for (i in 1:length(b)){
  cur_cap2=a[i]
  if (b[i]>0){
    pred=logmodel$coefficients[1]
    for (j in 1:5){
      pred=pred+(logmodel$coefficients[j+1])*(cur_cap2)^j
    }
    add=abs(b[i]-exp(pred))/abs(b[i])
    mape7=mape7+add
    count7=count7+1
  }
}
mape7=mape7/(count7)                 
mape7
length(b)-count7

#test-data
#7. poly-MAPE for log-model
count7t=0
mape7t=0
for (i in 1:length(e)){
  cur_cap2=d[i]
  if (e[i]>0){
    pred=logmodel$coefficients[1]
    for (j in 1:5){
      pred=pred+(logmodel$coefficients[j+1])*(cur_cap2)^j
    }
    add=abs(e[i]-exp(pred))/abs(e[i])
    mape7t=mape7t+add
    count7t=count7t+1
  }
}
mape7t=mape7t/(count7t)                 
mape7t
length(e)-count7t


#8. poly-MAPE for poly-model
#Calculate MAPE for training data
count8=0
mape8=0
for (i in 1:length(b)){
  hold=b[i]
  if (hold>0){
    count8=count8+1
    res=mylm$residuals[i]
    add=abs(res)/abs(b[i])
    mape8=mape8+add
  }
}
mape8=mape8/(count8)                 
mape8

#test-data
count8t=0
mape8t=0
for (i in 1:length(e)){
  hold=e[i]
  cur_cap2=d[i]
  if (hold>0){
    count8t=count8t+1
    pred=logmodel$coefficients[1]
    for (j in 1:5){
      pred=pred+(logmodel$coefficients[j+1])*(cur_cap2)^j
    }
    add=abs(e[i]-pred)/abs(e[i])
    mape8t=mape8t+add
  }
}
mape8t=mape8t/(count8t)                 
mape8t
length(e)-count8t


#test 7
count7=0
mape7=0
for (i in 1:length(e)){
  cur_cap2=d[i]
  if (e[i]>10^-10){
    pred=logmodel$coefficients[1]
    for (j in 1:5){
      pred=pred+(logmodel$coefficients[j+1])*(cur_cap2)^j
    }
    add=abs(e[i]-exp(pred))/abs(e[i])
    mape7=mape7+add
    count7=count7+1
  }
}
mape7=mape7/(count7)                 
mape7
length(b)-count7