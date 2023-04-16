# R Script. Modelleren 2A.

library(Matrix)
library(cmna)
library(MASS)
setwd("~/Library/Courses/Modelling2A")

# Construct a data object. This contains a list of the batteries and
# the cycles corresponding to the measurements.
load_data = function(file) {
	o = list()
	data = read.csv(file)
	o$batteries = unname(data[,2:length(data)])
	o$cycles = data$Cycle
	return(o)
}

printstr = function(s) {
  #cat(s)
}

# Get the lifetime of the battery given a list.
get_lifetime = function(cap) {
	return (match(min(cap, na.rm=TRUE), cap)-1)
}

# Get the RUL data given a domain and battery. Usually domain is just
# the cycles list.
get_RUL_data_2 = function(cap) {
	life = get_lifetime(cap)
	dom = 1:length(cap)
	return (life - dom)
}

# Return transformed RUL data.
get_RUL_data = function(cap) {
  rul = get_RUL_data_2(cap)
  for (i in 1:length(rul)) {
    if (rul[i] <= 0) {
      rul[i] = NA
    }
  }
  return (log(rul))
}

# Plotting the battery degradation until 0.88 Ah.
# The [,i] notation is equivalent to unlist on [i]. This happens because for
# lists in R, the index returns a 'list' object.
plot_capacity = function(lim, batteries) {
	cycles = 1:length(batteries[,1])
	plot(NULL, xlim=c(0, lim), ylim=c(0.85, 1.10), ylab="Capacity Ah", xlab="Cycle")
	abline(0.88, 0, col="red", lty=2)
	for (i in 1:length(batteries)) {
		lines(cycles, batteries[,i])
	}
}

# Plot the RUL in cycles against the battery capacities.
plot_rul = function(lim, batteries) {
	plot(NULL, xlim=c(0.85, 1.10), ylim=c(0, lim), ylab="RUL", xlab="Capacity Ah")
	for (i in 1:length(batteries)) {
		cap = batteries[,i]
		rul = get_RUL_data(cap)
		lines(cap, rul)
	}
}

# Compute and return the linear model given a formula.
compute_lm = function(form, batteries) {
	# Create a large vector matching for (cap, rul) plotting.
	capacities = c()
	ruls = c()
	for (i in 1:length(batteries)) {
		cap = batteries[,i]
		rul = get_RUL_data(cap)
		capacities = append(capacities, cap)
		ruls = append(ruls, rul)
	}
	variables = data.frame(x=capacities, y=ruls)
	model = lm(form, variables)
	return (model)
}

rbattery = function(data) {
	index = floor(runif(1, 1, length(data$batteries)))
	return (data$batteries[,index])
}

penalty_cost = function(x) {
	y = 0.1 * x
	return(y)
}

battery_policy = function(data, lm, c_main, c_repair, penalty_fun, step) {
	cost = 0
	start_cycle = 0
	bat = rbattery(data)
	life = get_lifetime(bat)
	for (cyc_total in seq(step, 2000, step)) {
		# Cur_cyc is the cycle that matters.
		cycle = cyc_total - start_cycle
		ah = bat[cycle]
		rul_pred = exp(unname(predict.lm(lm, data.frame(x=c(ah))))[1])
		rul = life - cycle
		printstr(sprintf("%-6d %-6d %-6.2f %-6.0f %-6.0f\n", cyc_total, cycle, ah, rul_pred, rul))
	
		# The battery does not even last a single maintenance!
		if (is.na(ah)) {
			penalty = penalty_fun(abs(rul))
			cost = cost + c_main + c_repair + penalty
			bat = rbattery(data)
			life = get_lifetime(bat)
			start_cycle = cyc_total
			printstr(sprintf("Battery dead before maintain penalty M+R+P = +%.1f, Penalty=%.1f\n\n", c_main+c_repair+penalty, penalty))
		}

		# Both prediction and real are good. Only maintenance.
		else if (rul_pred > step && rul > step) {
			cost = cost + c_main	
		}

		# The prediction is higher than reality, penalty!
		# Replace the battery with a new one.
		else if (rul_pred > step && rul < step) {
			penalty = penalty_cost(abs(rul))
			cost = cost + c_main + c_repair + penalty
			bat = rbattery(data)
			life = get_lifetime(bat)
			start_cycle = cyc_total
	  	printstr(sprintf("Battery penalty replacement M+R+P = +%.1f, Penalty=%.1f\n\n", c_main+c_repair+penalty, penalty))
		}

		# The prediction is lower than the step. Most batteries should
		# end like this for a good model.
		else if (rul_pred < step) {
			cost = cost + c_main + c_repair
			bat = rbattery(data)
			life = get_lifetime(bat)
			start_cycle = cyc_total
			printstr(sprintf("Battery replacement M+R = +%.1f\n\n", c_main + c_repair))
		}
	}
	return (cost)
}

policy_cost = function(data, lm, nbat, c_main, c_repair, penalty_fun, step) {
	costs = c()
	for (i in 1:nbat) {
		costs = append(costs, battery_policy(data, lm, c_main, c_repair, penalty_fun, step))
	}
	return (costs)
}

# Plot a linear model between xstart and xend.
plot_lm = function(model, xstart, xend, step, col, lwd) {
	xlist = seq(xstart, xend, step)
	pred = predict.lm(model, data.frame(x=xlist), interval="prediction", level=0.95)
	lines(xlist, pred[,"fit"], col=col, lwd=lwd)
	lines(xlist, pred[,"lwr"], col="blue" ,lwd=lwd)
	lines(xlist, pred[,"upr"], col="blue" ,lwd=lwd)
}

# Perform n policy cost simulations.
rpolicy = function(n, data, lm, nbat, c_main, c_repair, penalty_fun, step) {
	policy_costs = c()
	for (i in 1:n) {
		printstr(i)
		val = policy_cost(data_train, mylm, nbat, c_main, c_rep, penalty_fun, step)
		policy_costs = append(policy_costs, mean(val))
	}
	return (policy_costs)
}

mean_lifetime = function(data) {
  lifetime = 0
  for (bat in data$batteries) {
    lifetime = lifetime + get_lifetime(bat)
  }
  return(lifetime / length(data$batteries))
}

# Load primary data.
# Retrieve all battery columns using [row, column] notation.
# Basically select ALL ROWS from columns 2 to end.
data_train = load_data("Battery_train.csv")
data_test = load_data("Battery_test.csv")


# Task 1. Plotting of the capacities and RUL.
par(mfrow=c(1,2))
plot_capacity(2000, data_train$batteries)
plot_capacity(2000, data_test$batteries)

par(mfrow=c(1,2))
plot_rul(log(2000), data_train$batteries)
plot_rul(log(2000), data_test$batteries)


# Task 2. Linear regression.
# For linear regression we must append all (capacity, RUL) pairs.
par(mfrow=c(1,1))
plot_rul(log(2000), data_train$batteries)
mylm = compute_lm("y ~ poly(x, degree=5, raw=TRUE)", data_train$batteries)
plot_lm(mylm, 0.85, 1.10, 0.01, "red", 2)

# Visualizing the model matrix. We can conclude numerical errors cause
# the problems with degrees >5.
modelmatrix = unname(as.matrix(mylm$model[2]))
gmatrix = t(modelmatrix) %*% modelmatrix
rmatrix = refmatrix(gmatrix)


# Policy Costs Interval Plot.
c_main = 0.5
c_rep = 3
c_pen = penalty_cost

samples = 10
step = 10
lower = 10
upper = 200
intervals = seq(lower, upper, step)

data = data.frame()

for (i in intervals) {
	print(i)
	pol_costs = rpolicy(10, data_train, mylm, samples, c_main, c_rep, c_pen, i)
	pol_intervals = rep(i, samples)
	data = rbind(data, data.frame(costs=pol_costs, intervals=pol_intervals))
}

# Boxplot.
par(mfrow=c(1,1))
boxplot(costs ~ intervals, data=data)

# Line plot.
mean_costs = sapply(split(data$costs, data$intervals), mean)
plot(intervals, mean_costs, type="l")
axis(1, at = intervals)
grid(nx = NULL, ny = 1, col = "lightgray", lty = "dotted")


# Policy Cost Scatter Plot.
policy_costs = rpolicy(50, data_train, mylm, c_main, c_rep, c_pen, 300, step)
par(mfrow=c(1,2))
hist(policy_costs, nclass=10, probability=TRUE, ylim=c(0, 0.02))

nfit = fitdistr(policy_costs, "normal")
mean = nfit$estimate["mean"]
sd = nfit$estimate["sd"]
print(sprintf("Normal Fit: Mean=%.2f Sd=%.2f", mean, sd))

xlist = seq(mean-4*sd, mean+4*sd)
ylist = dnorm(xlist, mean, sd)
lines(xlist, ylist, col="red")
qqnorm(policy_costs)

# Parallelism setup.
library(foreach)
library(tcltk)
library(doSNOW)

# Define the cluster of FORK type (Unix only).
cluster = makeSOCKcluster(8)
registerDoSNOW(cluster)

# Policy Costs Interval Plot.
c_main = 0.5
c_rep = 3
c_pen = penalty_cost

samples = 1000
step = 1
lower = 160
upper = 180
intervals = seq(lower, upper, step)

pb = tkProgressBar(max=1.0, label="Parallel Work")
progress = function(x) setTkProgressBar(pb, x / length(intervals))
opts = list(progress=progress)

data = foreach(i=intervals, .combine=rbind, .options.snow=opts, .inorder=FALSE) %dopar% {
  pol_costs = rpolicy(10, data_train, mylm, samples, c_main, c_rep, c_pen, i)
  pol_intervals = rep(i, samples)
  data.frame(costs=pol_costs, intervals=pol_intervals)
}

close(pb)

# Boxplot.
par(mfrow=c(1,1))
boxplot(costs ~ intervals, data=data)

# Line plot.
#par(xaxt="n")
mean_costs = sapply(split(data$costs, data$intervals), mean)
plot(intervals, mean_costs, type="l")
par(xaxt="s")
for (i in c(2,3,4, 5, 10)) {
  peak = floor(2000 / i)
  axis(1, at=c(peak))
  lines(c(peak, peak), c(0, 140), col="red", lwd=1)
}

# Find minimum.
min_val = min(mean_costs)
min_index = match(min_val, mean_costs)
min_interval = lower + min_index - 1