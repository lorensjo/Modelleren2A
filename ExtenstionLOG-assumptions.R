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
plot(a,b,cex=0.01)
lines(cap_axis, exp(beta[1]+beta[2]*cap_axis+beta[3]*cap_axis^2+beta[4]*cap_axis^3+beta[5]*cap_axis^4+beta[6]*cap_axis^5), col="red")  #theoretical line based on the fit of the model

newdata = data.frame(x=xlist)
ynew = predict.lm(mylm, newdata)

qqnorm(logRUL,main="Normal QQ-plot of the RUL")
p=c()
q=c()
for (i in 1:length(b)){
  if (is.na(logmodel$residuals[i])==FALSE){
    p <- append(p,a[i])
    q <- append(q,logmodel$residuals[i])
  }
}

plot(p,q,cex=0.1,main="Residuals when degree=5 with logarithmic data",xlab='capacity',ylab='residuals')
mean(logmodel$residuals)
