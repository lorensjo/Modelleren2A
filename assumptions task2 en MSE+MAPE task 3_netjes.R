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
################################################################################

f = formula("y ~ poly(x, degree=5, raw = TRUE)")
variables = data.frame(x=a, y=b)
mylm = lm(f,variables)

xlist = seq(0.85,1.10,by=0.001)
newdata = data.frame(x=xlist)
ynew = predict.lm(mylm, newdata)

################################################################################
# Check assumptions of regression model
################################################################################

#check residuals have mean zero and equal variance for different capacities
plot(a,residuals(mylm),cex=0.01,main="Residuals when degree=5",xlab='capacity',ylab='residuals')
mean(residuals(mylm))

#check if data is normal
qqnorm(b,main="Normal QQ-plot of the RUL")

################################################################################
# Test model performance
################################################################################

#Calculate MAPE for training data
count=0
mape=0
for (i in 1:length(b)){
  hold=b[i]
  if (hold>10^-10){
    count=count+1
  res=my.lm$residuals[i]
  add=abs(res)/abs(b[i])
  mape=mape+add
}
}
mape=mape/(count)                 
mape

#calculate MAPE for test data
count2=0
mape2=0
for (i in 1:length(e)){
  hold=e[i]
  cur_cap=d[i]
  pred=my.lm$coefficients[1]
  if (hold>10^-10){
    for (j in 1:5){
      pred=pred+(my.lm$coefficients[j+1])*(cur_cap)^j
    }
    count2=count2+1
    addi=abs(e[i]-pred)/abs(e[i])
    mape2=mape2+addi
  }
}
mape2=mape2/(count2)                 
mape2

#Calculate MSE for test data
mse=0
for (i in 1:length(e)){
  cur_cap=d[i]
  pred=my.lm$coefficients[1]
  for (j in 1:5){
    pred=pred+(my.lm$coefficients[j+1])*(cur_cap)^j
  }
    addmse=(e[i]-pred)^2
    mse=mse+addmse
}
mse=mse/length(e)
mse

#calculate MSE for training data
mse2=0
for (i in 1:length(b)){
  cur_cap2=a[i]
  pred=my.lm$coefficients[1]
  for (j in 1:5){
    pred=pred+(my.lm$coefficients[j+1])*(cur_cap2)^j
  }
  addmse2=(b[i]-pred)^2
  mse2=mse2+addmse2
}
mse2=mse2/length(b)
mse2
