# R Script. Modelleren 2A.

library(Matrix)
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

# Get the lifetime of the battery given a list.
get_lifetime = function(cap) {
	return (match(min(cap, na.rm=TRUE), cap)-1)
}

# Get the RUL data given a domain and battery. Usually domain is just
# the cycles list.
get_RUL_data = function(cap) {
	life = get_lifetime(cap)
	dom = 1:length(cap)
	return (life - dom)
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

# Plot a linear model between xstart and xend.
plot_lm = function(model, xstart, xend, step, col, lwd) {
	xlist = seq(xstart, xend, step)
	ylist = predict.lm(model, data.frame(x=xlist))
	lines(xlist, ylist, col=col, lwd=lwd)
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
plot_rul(2000, data_train$batteries)
plot_rul(2000, data_test$batteries)


# Task 2. Linear regression.
# For linear regression we must append all (capacity, RUL) pairs.
par(mfrow=c(1,1))
plot_rul(2000, data_train$batteries)
mylm = compute_lm("y ~ poly(x, degree=5, raw=TRUE)", data_train$batteries)
plot_lm(mylm, 0.85, 1.10, 0.01, "red", 2)


# Visualizing the model matrix. We can conclude numerical errors cause
# the problems with degrees >5.
modelmatrix = unname(as.matrix(mylm$model[2]))
gmatrix = t(modelmatrix) %*% modelmatrix