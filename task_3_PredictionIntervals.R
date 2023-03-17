# R Script. Modelleren 2A.
file_train = "C:\\Users\\ltermaat\\Downloads\\Battery_train.csv"
file_test = "C:\\Users\\ltermaat\\Downloads\\Battery_test.csv"

file_train = file.choose()
data = read.csv(file_train)
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

plot(a,residuals(my.lm))
length(a)
length(residuals(my.lm))
anova(my.lm)
summary(my.lm)



################################################################################
plot(NULL, xlim=c(0.85, 1.10), ylim=c(0, 2000), ylab="RUL", xlab="Capacity Ah")

for (i in 2:num) {
  cap = data[,i]
  rul = get_RUL_data(cap)
  lines(cap, rul)
}
f = formula("y ~ 0 + poly(x, degree=5, raw = TRUE)")
variables = data.frame(x=a, y=b)
mylm = lm(f,variables)

xlist = seq(0.85,1.10,by=0.001)
newdata = data.frame(x=xlist)
ynew = predict.lm(mylm, newdata)
lines(xlist, ynew, col="red", lwd = 2)
summary(mylm)



################################################################################
### Task 3
################################################################################
# HEnceforth, we shall use the test data
file_test = file.choose()
testdata = read.csv(file_test)
L = length(testdata)-1

## find RUL and capacities, in same way as in train data
Capa_list = c()
RUL_list = c()
for (i in 2:L) {
  cap = testdata[,i]
  rul = get_RUL_data(cap)
  for (j in 1:length(cap)){
    if (is.na(cap[j])==FALSE){ 
      Capa_list <- append(Capa_list,cap[j])
      RUL_list <- append(RUL_list,rul[j])
    }
  }
}
## Plot the capacity against the RUL values for the test data
plot(Capa_list, RUL_list, cex=0.01,ylab="RUL", xlab="Capacity Ah")
lines(xlist, ynew, col="red", lwd = 2)  # adding expectation line from polynomial model

#Determining Confidence intervals 80% and 95%         does not work
ci_95 = predict.lm(mylm,newdata=data.frame(x=xlist),interval="prediction", level = 0.90)
ci_80 = predict.lm(mylm,newdata=data.frame(x=xlist),interval="prediction", level = 0.50)
# Plotting CI's
lines(xlist, ci_95[,2], col="blue", lty=2)
lines(xlist, ci_95[,3], col="blue", lty=2)
lines(xlist, ci_80[,2], col="lightblue", lty=2)
lines(xlist, ci_80[,3], col="lightblue", lty=2)


## Check how much is in 50% interval
count = 0
for (index in 1:length(Capa_list) ) {
  cap = Capa_list[index]
  rul = RUL_list[index]
  #first find position j of cap in xlist so to speak
  for (j in 1:length(xlist) ) {
    if (cap <= xlist[j]) {
      break
    }
  }
  # Now if the real RUl value is within inteval, add to count
  if (ci_80[,2][j] <= rul && rul <= ci_80[,3][j]) {
    count = count + 1
  }
}
percentage = count/length(Capa_list)