# R Script. Modelleren 2A.
data = read.csv("Battery_train.csv.csv")
num = length(data)-1


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
# Plot 1.
################################################################################
plot(NULL, xlim=c(0,2000), ylim=c(0.85, 1.10), ylab="Capacity Ah", xlab="Cycle")
abline(0.88, 0, col="red", lty=2)

for (i in 2:num) {
  lines(data$Cycle, data[,i])
}



################################################################################
# Plot 2. RUL grafiek.
################################################################################
plot(NULL, xlim=c(0.85, 1.10), ylim=c(0, 2000), ylab="RUL", xlab="Capacity Ah")
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


#task 2
plot(NULL, xlim=c(0.85, 1.10), ylim=c(0, 2000), ylab="RUL", xlab="Capacity Ah")

for (i in 2:num) {
  cap = data[,i]
  rul = get_RUL_data(cap)
  lines(cap, rul)
}
m=seq(0.85,1.10,by=0.001)
my.lm=lm(b~poly(a,5,raw = TRUE))
lines(m,47514-130408*m+m^2*114740+m^3*-31639 ,col='green',lwd=2)

################################################################################
plot(NULL, xlim=c(0.85, 1.10), ylim=c(0, 2000), ylab="RUL", xlab="Capacity Ah")

for (i in 2:num) {
  cap = data[,i]
  rul = get_RUL_data(cap)
  lines(cap, rul)
}
f = formula("y ~ poly(x, degree=5, raw = TRUE)")
variables = data.frame(x=a, y=b)
mylm = lm(f,variables)

xlist = seq(0.85,1.10,by=0.001)
newdata = data.frame(x=xlist)
ynew = predict.lm(mylm, newdata)
lines(xlist, ynew, col="red", lwd = 2)
summary(mylm)

#check residuals
plot(a,residuals(mylm),cex=0.01,main="Residuals when degree=5",xlab='capacity',ylab='residuals')
mean(residuals(mylm))

#check if data is normal
qqnorm(b,main="Normal QQ-plot of the RUL")

#check MAPE
test = read.csv("Battery_test.csv")

#for training data
count=0
mape=0
for (i in 1:length(b)){
  hold=b[i]
  if (hold>10^-20){
    count=count+1
  res=my.lm$residuals[i]
  add=abs(res)/abs(b[i])
  mape=mape+add
}
}
mape=mape/(count)                 
mape

#for test data:
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
plot(d,e,cex=0.01)

count2=0
mape2=0
for (i in 1:length(e)){
  hold=e[i]
  cur_cap=d[i]
  pred=my.lm$coefficients[1]
  if (hold>10^-20){
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

mse=0
for (i in 1:length(e)){
  hold=e[i]
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
